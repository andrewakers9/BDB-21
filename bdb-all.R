
#load dependencies
library(tidyverse) #1.3.0
library(lubridate) #1.7.9.2
library(xgboost) #1.2.0.1
library(caret) #6.0-86
library(foreach) #1.5.1
library(mvtnorm) #1.1-1

#set seed to be able to reproduce results
set.seed(42)

#read in all data
games <- read.csv("../input/nfl-big-data-bowl-2021/games.csv")
plays <- read.csv("../input/nfl-big-data-bowl-2021/plays.csv")
players <- read.csv("../input/nfl-big-data-bowl-2021/players.csv")
target <- read.csv("../input/additional-data-targeted-receiver")
coverage <- read.csv("../input/additional-data-coverage-schemes-for-week-1")

tracking <- data.frame()
for(w in 1:17){

  tracking_temp <- read_csv(paste0("../input/nfl-big-data-bowl-2021/week",w,".csv"),
                            col_types = cols())

  tracking <- bind_rows(df_tracking_temp, df_tracking)

}

#initial data wrangling/manipulation
plays <- plays %>%
  mutate(
    yards_to_goal = if_else(possessionTeam != yardlineSide,
                            yardlineNumber,
                            as.integer(100 - yardlineNumber)),
    num_rb = str_match(personnelO, "(\\d)\\sRB")[, 2],
    num_wr = str_match(personnelO, "(\\d)\\sWR")[, 2],
    num_te = str_match(personnelO, "(\\d)\\sTE")[, 2],
    num_db = str_match(personnelD, "(\\d)\\sDB")[, 2]
  )

plays <- plays %>%
  left_join(
    select(
      games,
      gameId, homeTeamAbbr, visitorTeamAbbr
    ),
    by = "gameId"
  )

#normalize data so all plays are from the offense's perspective
tracking <- tracking %>%
  mutate(
    time = hms(str_match(time, "\\d{2}:\\d{2}:\\d{2}\\.\\d{3}")),
    x = if_else(playDirection == "left", 120 - x, x),
    y = if_else(playDirection == "left", 160 / 3 - y, y),
    o = case_when(
      playDirection == "left" & o > 180 ~ o - 180,
      playDirection == "left" & o < 180 ~ o + 180,
      TRUE ~ o
    ),
    dir = case_when(
      playDirection == "left" & dir > 180 ~ dir - 180,
      playDirection == "left" & dir < 180 ~ dir + 180,
      TRUE ~ dir
    )
  ) %>%
  arrange(gameId, playId)

#nest tracking data as list col in data frame
play_detail <- tracking %>%
  group_by(gameId, playId) %>%
  nest() %>%
  ungroup()

#function to get parameters of bivariate gaussian player influence dist
#methodology adapted from Fernandez and Born
get_player_influence <- function(x,
                                 y,
                                 x_ball,
                                 y_ball,
                                 speed,
                                 angle,
                                 plot_it = FALSE) {

  deg_2_rad <- function(deg) {
    rad <- deg * pi /180
    return(rad)
  }

  angle_rotate <- function(a, rotation) {
    a <- a + rotation
    a <- ifelse(a < 0, a + 360,
                ifelse(a > 360, a - 360, a))
  }

  angle_rot <- angle_rotate(angle, 90) %>% deg_2_rad()

  position <- c(x, y)
  R <- matrix(
    c(cos(angle_rot), -sin(angle_rot), sin(angle_rot), cos(angle_rot)),
    ncol = 2
  )

  speed <- (speed * 1760) / 3600
  speed_ratio <- speed^2 / 7^2

  distance <- sqrt(
    (x - x_ball)^2 + (y - y_ball)^2
  )

  #when distance is less than 5, the player radius is capped at 3 yards
  #otherwise, player radius increases linearly based on the relationship between distance and
  #the time it takes a pass to arrive
  get_player_radius <- function(distance) {
    if(distance < 5) {
      return(3)
    } else {
      return(2.915764 +  0.202729 * distance)
    }
  }

  R_i_t <- get_player_radius(distance)

  S_i_t <- matrix(
    c((R_i_t + (R_i_t * speed_ratio))/2, 0, 0, (R_i_t - (R_i_t * speed_ratio))/2),
    ncol = 2
  )

  COV_i_t <- R %*% S_i_t %*% S_i_t %*% MASS::ginv(R)
  mu_i_t <- c(
    position[1] + speed * sin(deg_2_rad(angle)) / 2,
    position[2] + speed * cos(deg_2_rad(angle)) / 2
  )

  if(plot_it) {

    center_den <- dmvnorm(mu_i_t, mu_i_t, COV_i_t)
    plot_grid <- expand.grid(
      x = seq(0.5, 119.5, by = 1),
      y = seq(53.3 / 53 / 2, 53.3, by = 53.3 / 53)
    ) %>%
      rowwise() %>%
      mutate(
        prob = dmvnorm(c(x,y), mu_i_t, COV_i_t)
      ) %>%
      mutate(
        player_infl = prob / center_den
      )

    print(
      plot_field(x - 10, x + 10) +
        geom_contour_filled(
          data = plot_grid,
          aes(
            x = 53.33 - y,
            y = x,
            z = player_infl
          ),
          bins = 5,
          alpha = 0.8
        ) +
        annotate(
          "point",
          x = 53.3 - position[2],
          y = position[1]
        ) +
        annotate(
          "segment",
          x = 53.3 - position[2],
          y = position[1],
          xend = 53.3 - mu_i_t[2],
          yend = mu_i_t[1],
          arrow = arrow(length = unit(0.1, "inches"))
        ) +
        scale_fill_manual(
          values = c(NA, "#DDEBF7", "#BDD7EE", "#9BC2E6", "#2F75B5")
        )
    )

  }

  return_list <- list(
    mu = mu_i_t,
    sigma = COV_i_t
  )

  return(return_list)

}

#function to extract features for cluster model from each play
#features are extracted for both sides of the play
#receiver data to the left of the qb is mirrored
#play must have at least two receivers to be considered a route combo
#backfield receivers are allocated to a side based on the route direction
get_cluster_features <- function(play) {

  ball_snap_df <- play %>%
    filter(
      event == "ball_snap",
      displayName == "Football"
    ) %>%
    select(x, y, frameId)

  ball_snap_x <- unlist(round(ball_snap_df[, 1], 0))
  ball_snap_y <- unlist(round(ball_snap_df[, 2], 0))
  frame_ids <- unlist(ball_snap_df[, 3]) + c(10, 20, 30)

  routes <- c("HITCH", "OUT", "FLAT", "CROSS", "GO", "SLANT",
              "SCREEN", "CORNER", "IN", "ANGLE", "POST", "WHEEL")

  backfield_pull <- play %>%
    filter(
      route %in% routes,
      y > ball_snap_y - 3.5,
      y < ball_snap_y + 3.5,
      frameId == 11
    ) %>%
    pull(nflId)

  backfield_left <- NULL
  backfield_right <- NULL

  if(!is_null(backfield_pull)) {

    backfield <- play %>%
      filter(
        nflId %in% backfield_pull,
        frameId %in% c(11, 35)
      ) %>%
      group_by(nflId) %>%
      summarise(change_y = y - lag(y), .groups = "drop") %>%
      filter(!is.na(change_y)) %>%
      mutate(
        route_dir = case_when(
          change_y < 0 ~ "right",
          change_y >= 0 ~ "left"
        )
      )

    backfield_left <- backfield %>%
      filter(route_dir == "left") %>%
      pull(nflId)

    backfield_right <- backfield %>%
      filter(route_dir == "right") %>%
      pull(nflId)

  }

  targets_left <- play %>%
    filter(
      route %in% routes,
      frameId == 11,
      (y >= ball_snap_y + 3.5 | nflId %in% backfield_left)
    ) %>%
    pull(nflId)

  targets_right <- play %>%
    filter(
      route %in% routes,
      frameId == 11,
      (y <= ball_snap_y - 3.5 | nflId %in% backfield_right)
    ) %>%
    pull(nflId)

  if(length(targets_left) > 1 & length(targets_right) > 1) {
    play_test <- play %>%
      filter(
        (nflId %in% c(targets_left, targets_right) |
           displayName == "Football"),
        frameId %in% (frame_ids)
      )
  } else if(length(targets_left) > 1) {
    play_test <- play %>%
      filter(
        (nflId %in% targets_left | displayName == "Football"),
        frameId %in% (frame_ids)
      )
  } else if(length(targets_right) > 1) {
    play_test <- play %>%
      filter(
        (nflId %in% targets_right | displayName == "Football"),
        frameId %in% (frame_ids)
      )
  } else {
    return(NULL)
  }

  play_test <- play_test %>%
    with_groups(
      frameId,
      ~ mutate(
        .x,
        x_ball = x[which(displayName == "Football")],
        y_ball = y[which(displayName == "Football")]
      )
    ) %>%
    filter(
      displayName != "Football"
    ) %>%
    mutate(
      route_dir = case_when(
        nflId %in% targets_left ~ "left",
        nflId %in% targets_right ~ "right"
      ),
      y = ifelse(route_dir == "right", 53.33 - y, y),
      y_ball = ifelse(route_dir == "right", 53.33 - y, y),
      dir = ifelse(route_dir == "right", -dir, dir)
    ) %>%
    rowwise() %>%
    mutate(
      infl_params = list(get_player_influence(
        x, y, x_ball, y_ball, s, dir
      ))
    ) %>%
    ungroup() %>%
    mutate(
      center_den = map(infl_params, ~ dmvnorm(.x$mu, .x$mu, .x$sigma))
    )

  df_grid <- expand.grid(
    x = seq(ball_snap_x - 5, ball_snap_x + 25, by = 1),
    y = seq(26, 53, by = 1),
    frame_ids = frame_ids
  ) %>%
    left_join(
      select(play_test, frameId, route_dir, infl_params, center_den),
      by = c("frame_ids" = "frameId")
    ) %>%
    mutate(
      influence = pmap_dbl(
        list(x = x, y = y, params = infl_params, z = center_den),
        function(x, y, params, z) dmvnorm(c(x, y), params$mu, params$sigma) / z
      )
    )

  df_grid <- df_grid %>%
    group_by(route_dir, x, y, frame_ids) %>%
    summarise(
      team_infl = sum(influence),
      .groups = "drop"
    )

  ret_lst <- list(
    team_infl = df_grid$team_infl,
    route_dir = unique(df_grid$route_dir)
  )

  return(ret_lst)

}

#function to filter plays to include in cluster model
#play must have a route run, a forward pass, and last at least 2.5 seconds from the snap
get_play_filter <- function(x) {

  pass_bool <- "pass_forward" %in% x$event

  routes <- c("HITCH", "OUT", "FLAT", "CROSS", "GO", "SLANT",
              "SCREEN", "CORNER", "IN", "ANGLE", "POST", "WHEEL")

  route_bool <- any(x$route %in% routes)

  max_frame_bool <- max(x$frameId) >= 41

  pass_outcome_frame <- x %>%
    filter(
      str_detect(event, "pass_outcome")
    ) %>%
    dplyr::slice(1) %>%
    pull(frameId)

  pass_outcome_bool <- pass_outcome_frame >= 36

  return(
    all(pass_bool, route_bool, max_frame_bool, pass_outcome_bool)
  )

}

#apply play filter
play_filter <- map_lgl(play_detail$data, get_play_filter)

#loop over plays in parallel and extract cluster features
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)
cluster_features <- foreach(
  play = play_detail$data[play_filter],
  .packages = c("lubridate", "dplyr", "purrr", "mvtnorm"),
  .errorhandling = "pass"
) %dopar% {

  get_cluster_features(play)

}
parallel::stopCluster(cl)

#filter out plays that returned NULL (and 3 plays that had data issues)
feature_filter <- map_lgl(
  cluster_features,
  ~ class(.x)[1] != "list"
)
cluster_features <- cluster_features[!feature_filter]

#create data frame to ID cluster model observations
play_id <- play_detail %>%
  select(!data) %>%
  filter(play_filter) %>%
  filter(!feature_filter)

num_obs <- map_dbl(
  cluster_features,
  ~ length(.x[[1]]) / 2604 #each route combo has 2604 features
)

play_id <- data.frame(
  gameId = rep(play_id$gameId, times = num_obs),
  playId = rep(play_id$playId, times = num_obs),
  route_dir = unlist(map(cluster_features, ~ .x[[2]]))
)

#unlist cluster features into a matrix
cluster_mat <- cluster_features %>%
  map(~ .x[[1]]) %>%
  unlist() %>%
  matrix(
    data = .,
    ncol = 2604,
    byrow = TRUE
  )

#apply pca to raw features
cluster_pca <- prcomp(cluster_mat)

#apply kmeans algorithm to first 10 principal components
cluster_mod <- kmeans(cluster_pca$x[, 1:10], 15, nstart = 5, iter.max = 20)

#get cluster centers for original features
cluster_df <- as_tibble(cluster_mat) %>%
  mutate(cluster = cluster_mod$cluster) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

#add cluster assignment to play_id df
play_id$cluster <- cluster_mod$cluster

#helper function
angle_diff <- function(a, b) {
  a <- a - b
  a <- (a + 180) %% 360 - 180
  return(abs(a))
}

#function to extract summary features for coverage classification model
play_summary <- function(data, home_team, possession_team) {

  ball_snap <- data %>%
    filter(event == "ball_snap") %>%
    dplyr::slice(1) %>%
    pull(frameId)

  y_start <- data %>%
    filter(
      event == "ball_snap",
      displayName == "Football"
    ) %>%
    pull(y)

  qb_pocket <- data %>%
    filter(
      position == "QB",
      y < y_start + 3,
      y > y_start - 3
    ) %>%
    pull(frameId) %>%
    max()

  if(home_team == possession_team) {
    defense <- "away"
  } else {
    defense <- "home"
  }

  idx <- which(
    data$event %in% c(
      "pass_forward",
      "qb_strip_sack",
      "pass_shovel",
      "fumble"
    )
  )

  if(!is_empty(idx)) {
    frame_id <- data$frameId[max(idx)]
    data <- data %>%
      filter(
        frameId <= frame_id
      )
  }

  data <- data %>%
    filter(
      position == "CB",
      team == defense,
      frameId <= qb_pocket,
      frameId >= ball_snap
    ) %>%
    group_by(nflId) %>%
    summarise(
      across(c(x, y, s, dir, o), .fns = var, .names = "var_{col}"),
      .groups = "drop"
    ) %>%
    summarise(
      across(!nflId, mean)
    )

  return(data)

}

#function to extract relative play summary features based on
#closest teammate and opponent
rel_play_summary <- function(data, home_team, possession_team) {

  if(home_team == possession_team) {
    defense <- "away"
    offense <- "home"
  } else {
    defense <- "home"
    offense <- "away"
  }

  o_names <- unique(data$displayName[which(data$team == offense)])
  d_names <- unique(data$displayName[which(data$team == defense)])

  ball_snap <- data %>%
    filter(event == "ball_snap") %>%
    dplyr::slice(1) %>%
    pull(frameId)

  idx <- which(
    data$event %in% c(
      "pass_forward",
      "qb_strip_sack",
      "pass_shovel",
      "fumble"
    )
  )

  if(!is_empty(idx)) {
    pass_forward <- data$frameId[max(idx)]
    data <- data %>%
      filter(
        frameId >= ball_snap,
        frameId <= pass_forward
      )
  } else {
    data <- data %>%
      filter(
        frameId >= ball_snap
      )
  }

  data_nest <- data %>%
    filter(displayName != "Football") %>%
    group_by(frameId) %>%
    nest()

  calc_dist_mat <- function(data) {
    mat <- as.matrix(data[, 2:3])
    rownames(mat) <- data$displayName
    dist_mat <- as.matrix(dist(mat))
    diag(dist_mat) <- NA_real_
    return(dist_mat)
  }

  data_nest <- data_nest %>%
    mutate(
      dist_mat = map(data, calc_dist_mat)
    )

  get_closest_player <- function(dist_mat) {

    row_idx_o <- which(rownames(dist_mat) %in% o_names)
    dist_mat_o <- dist_mat[row_idx_o, ]
    min_dist_o <- apply(dist_mat_o, 2, min, na.rm = TRUE)
    close_plyr_o <- apply(dist_mat_o, 2,
                          function(x) rownames(dist_mat_o)[which.min(x)])

    row_idx_d <- which(rownames(dist_mat) %in% d_names)
    dist_mat_d <- dist_mat[row_idx_d, ]
    min_dist_d <- apply(dist_mat_d, 2, min, na.rm = TRUE)
    close_plyr_d <- apply(dist_mat_d, 2,
                          function(x) rownames(dist_mat_d)[which.min(x)])

    return(
      data.frame(
        close_plyr_o = close_plyr_o,
        min_dist_o = min_dist_o,
        close_plyr_d = close_plyr_d,
        min_dist_d = min_dist_d
      )
    )

  }

  data <- data_nest %>%
    mutate(
      dist_vars = map(dist_mat, get_closest_player)
    ) %>%
    select(!dist_mat) %>%
    unnest(cols = c(data, dist_vars)) %>%
    ungroup()

  data_join_o <- data %>%
    select(
      frameId, close_plyr_o
    ) %>%
    distinct() %>%
    left_join(
      select(data, frameId, displayName, close_plyr_o_dir = dir),
      by = c("frameId", "close_plyr_o" = "displayName")
    )

  data <- data %>%
    left_join(
      data_join_o,
      by = c("frameId", "close_plyr_o")
    ) %>%
    mutate(
      angle_diff_o = angle_diff(dir, close_plyr_o_dir)
    )

  cb_stats <- data %>%
    filter(position == "CB") %>%
    group_by(nflId) %>%
    summarise(
      across(contains("dist"), list(mean = mean, var = var)),
      across(contains("angle"), list(mean = mean, var = var)),
      .groups = "drop"
    ) %>%
    summarise(
      var_min_dist_o_mean = var(min_dist_o_mean),
      var_min_dist_d_mean = max(min_dist_d_mean),
      across(!nflId, mean)
    )

  return(cb_stats)

}

#combined summary function
cmb_play_summary <- function(data, home_team, possession_team) {
  df1 <- play_summary(data, home_team, possession_team)
  df2 <- rel_play_summary(data, home_team, possession_team)
  return(cbind(df1, df2))
}

#get features, including joins from plays
cov_features <- play_detail %>%
  left_join(
    select(
      games,
      gameId, homeTeamAbbr, visitorTeamAbbr
    ),
    by = "gameId"
  ) %>%
  left_join(
    select(
      plays,
      gameId, playId, possessionTeam, defendersInTheBox, numberOfPassRushers
    ),
    by = c("gameId", "playId")
  ) %>%
  inner_join(coverage, by = c("gameId", "playId")) %>%
  filter(!is.na(coverage)) %>%
  mutate(
    coverage = str_to_lower(coverage),
    man_cov = if_else(str_detect(coverage, "man"), 1, 0)
  )

cov_features <- cov_features %>%
  mutate(
    cb_stats = pmap(
      list(data, homeTeamAbbr, possessionTeam),
      cmb_play_summary
    )
  ) %>%
  select(!data) %>%
  unnest(cb_stats)

#filter out one play with no CBs; explicitly fill missing values
cov_features <- cov_features %>%
  filter(!is.nan(var_x)) %>%
  mutate(
    var_min_dist_o_mean = ifelse(is.na(var_min_dist_o_mean), 0, var_min_dist_o_mean),
    var_min_dist_d_mean = ifelse(is.na(var_min_dist_d_mean), 0, var_min_dist_d_mean),
    numberOfPassRushers = ifelse(is.na(numberOfPassRushers), 4, numberOfPassRushers)
  )

#split data for man/zone xgboost and tune hyperparameters
X_train_idx <- createDataPartition(
  y = cov_features$man_cov,
  p = 0.8
) %>% unlist()

X_train <- cov_features[X_train_idx, ]
X_test <- cov_features[-X_train_idx, ]

folds <- createFolds(
  X_train$man_cov
)

X_train_xgb <- xgb.DMatrix(
  data = as.matrix(X_train[, c(6, 7, 10:22)]),
  label = X_train$man_cov
)

X_test_xgb <- xgb.DMatrix(
  data = as.matrix(X_test[, c(6, 7,10:22)]),
  label = X_test$man_cov
)

param_grid <- expand.grid(
  max_depth = 3:10,
  min_child_weight = 1:5
)

param_grid$cv_auc <- rep(NA_real_, nrow(param_grid))
for(i in 1:nrow(param_grid)) {

  max_depth <- param_grid$max_depth[i]
  min_child_weight <- param_grid$min_child_weight[i]

  xgb_cv <- xgb.cv(
    data = X_train_xgb,
    folds = folds,
    metrics = "auc",
    objective = "binary:logistic",
    nrounds = 100,
    early_stopping_rounds = 10,
    max_depth = 3, #param_grid$max_depth[i],
    min_child_weight = 1, #param_grid$min_child_weight[i],
    subsample = 1, #param_grid$subsample[i],
    colsample_bytree = 1, #param_grid$colsample_bytree[i],
    eta = 0.1,
    prediction = TRUE
  )

  param_grid$cv_auc[i] <- max(xgb_cv$evaluation_log$test_auc_mean)

}

#train model with selected hyperparameters
xgb_mod <- xgb.train(
  data = X_train_xgb,
  nrounds = 33,
  objective = "binary:logistic",
  max_depth = 3,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1,
  eta = 0.1
)

new_data <- play_detail %>%
  filter(play_filter) %>%
  filter(!feature_filter) %>%
  left_join(
    select(
      games,
      gameId, homeTeamAbbr, visitorTeamAbbr
    ),
    by = "gameId"
  ) %>%
  left_join(
    select(
      plays,
      gameId, playId, possessionTeam, defendersInTheBox, numberOfPassRushers
    ),
    by = c("gameId", "playId")
  )

#get xgboost features for plays in the cluster model
new_data <- new_data %>%
  mutate(
    data = map(data, ~ mutate(.x, time = as.numeric(time)))
  ) %>%
  mutate(
    cb_stats = map(
      list(data, homeTeamAbbr, possessionTeam),
      safely(cmb_play_summary)
    )
  )

new_data <- new_data %>%
  select(!data) %>%
  mutate(
    cb_stats = map(cb_stats, ~ .x[[1]])
  ) %>%
  unnest(cb_stats)

new_data <- new_data %>%
  filter(!is.nan(var_x)) %>%
  mutate(
    var_min_dist_o_mean = ifelse(is.na(var_min_dist_o_mean), 0, var_min_dist_o_mean),
    var_min_dist_d_mean = ifelse(is.na(var_min_dist_d_mean), 0, var_min_dist_d_mean),
    numberOfPassRushers = ifelse(is.na(numberOfPassRushers), 4, numberOfPassRushers)
  )

new_data_xgb <- xgb.DMatrix(
  data = as.matrix(new_data[, 6:20])
)

new_preds <- predict(
  xgb_mod,
  newdata = new_data_xgb,
  type = "prob"
)

new_data <- new_data %>%
  mutate(
    man_prob = new_preds,
    is_zone = ifelse(man_prob < 0.4, 1, 0)
  )

#join cluster assignment and filter for plays with zone defense predicted
play_id <- play_id %>%
  left_join(
    select(
      new_data,
      gameId, playId, is_zone
    ),
    by = c("gameId", "playId")
  ) %>%
  filter(is_zone == 1)

#start model data frame for epa regression
play_id_wide <- play_id %>%
  pivot_wider(
    id_cols = c(gameId, playId),
    names_from = route_dir,
    names_glue = "cluster_{route_dir}",
    values_from = cluster
  )

#function to extract which side the target receiver was on
get_target_side <- function(play, target) {

  ball_snap_df <- play %>%
    filter(
      event == "ball_snap",
      displayName == "Football"
    ) %>%
    select(x, y, frameId)

  ball_snap_x <- unlist(round(ball_snap_df[, 1], 0))
  ball_snap_y <- unlist(round(ball_snap_df[, 2], 0))

  routes <- c("HITCH", "OUT", "FLAT", "CROSS", "GO", "SLANT",
              "SCREEN", "CORNER", "IN", "ANGLE", "POST", "WHEEL")

  target_route <- play %>%
    filter(nflId == target) %>%
    dplyr::slice(1) %>%
    pull(route)

  if(!target_route %in% routes) {
    return("No route")
  }

  backfield_pull <- play %>%
    filter(
      route %in% routes,
      y > ball_snap_y - 3.5,
      y < ball_snap_y + 3.5,
      frameId == 11
    ) %>%
    pull(nflId)

  backfield_left <- NULL
  backfield_right <- NULL

  if(!is_null(backfield_pull)) {

    backfield <- play %>%
      filter(
        nflId %in% backfield_pull,
        frameId %in% c(11, 35)
      ) %>%
      group_by(nflId) %>%
      summarise(change_y = y - lag(y), .groups = "drop") %>%
      filter(!is.na(change_y)) %>%
      mutate(
        route_dir = case_when(
          change_y < 0 ~ "right",
          change_y >= 0 ~ "left"
        )
      )

    backfield_left <- backfield %>%
      filter(route_dir == "left") %>%
      pull(nflId)

    backfield_right <- backfield %>%
      filter(route_dir == "right") %>%
      pull(nflId)

  }

  targets_left <- play %>%
    filter(
      route %in% routes,
      frameId == 11,
      (y >= ball_snap_y + 3.5 | nflId %in% backfield_left)
    ) %>%
    pull(nflId)

  targets_right <- play %>%
    filter(
      route %in% routes,
      frameId == 11,
      (y <= ball_snap_y - 3.5 | nflId %in% backfield_right)
    ) %>%
    pull(nflId)

  if(target %in% targets_left) {
    return("left")
  } else if(target %in% targets_right) {
    return("right")
  }

}

#join targets and apply function above
play_id_wide <- play_id_wide %>%
  left_join(
    play_detail,
    by = c("gameId", "playId")
  ) %>%
  left_join(
    targets,
    by = c("gameId", "playId")
  ) %>%
  filter(!is.na(targetNflId)) %>%
  mutate(
    target_side = map2(
      data, targetNflId,
      safely(get_target_side)
    )
  )

side_filter <- map_lgl(play_id_wide$target_side, ~ !is_null(.x[[2]]))

#add target side to model data frame
play_id_wide <- play_id_wide %>%
  select(!c(data, targetNflId)) %>%
  filter(!side_filter) %>%
  mutate(
    target_side = unlist(map(target_side, ~.x[[1]]))
  )

#get the cluster assignment for the target side
#only keep plays where there is a cluster assignment for the target side
play_id_wide <- play_id_wide %>%
  mutate(
    target_cluster = case_when(
      target_side == "left" ~ cluster_left,
      target_side == "right" ~ cluster_right,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(target_cluster))

#get control vars from plays df
plays <- plays %>%
  mutate(
    gameClock = if_else(
      gameClock == "",
      str_match(playDescription, "\\(([0-9:]*)\\)")[, 2],
      gameClock
    ),
    gameClock = if_else(
      str_detect(gameClock, "^:"),
      paste0("00", gameClock),
      gameClock
    )
  )

plays <- plays %>%
  mutate(
    offense_home = if_else(possessionTeam == homeTeamAbbr, 1, 0),
    gameClock = ms(str_match(gameClock, "\\d+:\\d{2}")),
    time_remaining = if_else(
      quarter %in% c(1, 3),
      as.numeric(gameClock) / 60 + 15,
      as.numeric(gameClock) / 60
    )
  )

plays_sel <- plays %>%
  select(
    gameId,
    playId,
    quarter,
    down,
    yardsToGo,
    yards_to_goal,
    offense_home,
    time_remaining,
    epa
  )

#join control vars to model data frame
play_id_wide <- play_id_wide %>%
  left_join(plays_sel, by = c("gameId", "playId")) %>%
  mutate(
    target_cluster = as.factor(target_cluster),
    quarter = as.factor(quarter),
    down = as.factor(down)
  )

#set contrasts to keep all factor levels
contrasts_arg = lapply(
  play_id_wide[sapply(play_id_wide, is.factor)],
  contrasts, contrasts = FALSE
)

#expand factors with dummy vars
X_epa <- model.matrix(
  ~ ., play_id_wide[, 6:13],
  contrasts.arg = contrasts_arg
)
X_epa <- X_epa[, -1]

#train xgboost regression
#hyperparameters tuned via cross val; no test set needed for descriptive analysis
X_train_xgb <- xgb.DMatrix(
  data = as.matrix(X_epa[, -29]),
  label = X_epa[, "epa"]
)

param_grid <- expand.grid(
  max_depth = 3:10,
  min_child_weight = 1:5
)

param_grid$nrounds <- rep(NA_real_, nrow(param_grid))
param_grid$rmse <- rep(NA_real_, nrow(param_grid))

for(i in 1:nrow(param_grid)) {

  max_depth <- param_grid$max_depth[i]
  min_child_weight <- param_grid$min_child_weight[i]

  xgb_cv <- xgb.cv(
    data = X_train_xgb,
    folds = folds,
    metrics = "rmse",
    objective = "reg:squarederror",
    nrounds = 100,
    early_stopping_rounds = 10,
    max_depth = param_grid$max_depth[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = 1, #param_grid$subsample[i],
    colsample_bytree = 1, #param_grid$colsample_bytree[i],
    eta = 0.1,
  )

  param_grid$rmse[i] <- min(xgb_cv$evaluation_log$test_rmse_mean)
  param_grid$nrounds[i] <- xgb_cv$best_iteration

}

#train model with selected hyperparameters
xgb_mod <- xgb.train(
  data = X_train_xgb,
  nrounds = 28,
  objective = "reg:squarederror",
  max_depth = 4,
  min_child_weight = 3,
  subsample = 1,
  colsample_bytree = 1,
  eta = 0.1
)

#get SHAP values for each obs
shap_values <- SHAPforxgboost::shap.values(xgb_mod, X_epa[, -29])[[1]]

col_filt <- which(apply(shap_values, 2, function(x) all(x == 0)))
shap_df_long <- shap_values %>%
  as_tibble() %>%
  select(!all_of(col_filt)) %>%
  mutate(obs = 1:nrow(.)) %>%
  pivot_longer(
    cols = -obs,
    names_to = "var",
    values_to = "shap"
  )
