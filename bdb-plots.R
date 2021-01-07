#load dependencies
library(ggnewscale)
library(gganimate)

animate_play <- function(play, influence = FALSE) {
  
  play <- play %>%
    mutate(
      x_next = x + (s * 1760 / 3600 * sin(deg_2_rad(dir))),
      y_next = y + (s * 1760 / 3600 * cos(deg_2_rad(dir)))
    ) 
  
  cols_fill <- c("#FB4F14", "#663300", "#A5ACAF")
  cols_col <- c("#000000", "#663300", "#000000")
  
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  #specific boundaries
  ymin <- max(round(min(play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
  
  #hash marks
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  if(influence == TRUE) {
    
    max_frame <- max(play$frameId)
    
    data <- play %>%
      filter(
        position %in% c("SS", "FS", "MLB", "CB", "LB", "OLB", "DL", "ILB", "DB", "NT", "S", "DE", "DT", "")
      ) %>%
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
    
    df <- expand.grid(
      x = seq(ymin - 10, ymax + 10, by = 1),
      y = seq(53.3 / 53 / 2, 53.3, by = 53.3 / 53),
      frameId = 1:max_frame
    ) %>%
      left_join(
        select(data, frameId, infl_params, center_den)
      ) %>%
      mutate(
        influence = pmap_dbl(
          list(x = x, y = y, params = infl_params, z = center_den),
          function(x, y, params, z) dmvnorm(c(x, y), params$mu, params$sigma) / z
        )
      )
    
    df2 <- df %>%
      group_by(x, y, frameId) %>%
      summarise(
        team_infl = sum(influence),
        .groups = "drop"
      )
    
    df2 <- df2 %>%
      mutate(
        alpha = ifelse(team_infl < 0.2, 0, 0.8)
      )
    
  }
  
  p1 <- ggplot() +
    
    #setting size and color parameters
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
    scale_fill_manual(values = cols_fill, guide = FALSE) + 
    scale_colour_manual(values = cols_col, guide = FALSE) +
    
    #adding hash marks
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    
    #adding yard lines
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    
    #adding field yardline text
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    
    #adding field exterior
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    
    #adding players
    geom_point(data = play, aes(x = (xmax-y),
                                y = x, 
                                shape = team,
                                fill = team,
                                group = nflId,
                                size = team,
                                colour = team) 
    ) +  
    
    #adding jersey numbers
    geom_text(data = play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    
    geom_segment(
      data = play,
      aes(x = (xmax - y), y = x, xend = (xmax - y_next), yend = x_next),
      arrow = arrow(length = unit(0.1, "in")) 
    ) 
  
  if(influence == TRUE) {
    
    p1 +
      
      new_scale_fill() +
      
      geom_raster(
        data = df2,
        aes(x = 53.3 - y, y = x, fill = team_infl, alpha = alpha),
        interpolate = TRUE
      )  +
      scale_fill_distiller(palette = "BuGn", direction = 1, name = "Influence") +
      #scale_fill_gradient(low = "white", high = "#2F75B5") +
      scale_alpha(limits = c(0.5, 1), na.value = 0, guide = FALSE) +
      
      #applying plot limits
      ylim(ymin, ymax) + 
      coord_fixed() +
      
      #theme
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()
      ) +
      
      #setting animation parameters
      transition_time(frameId)  +
      ease_aes('linear') + 
      NULL
    
  } else {
    
    p1 +
      
      #applying plot limits
      ylim(ymin, ymax) + 
      coord_fixed() +
      
      #theme
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()
      ) +
      
      #setting animation parameters
      transition_time(frameId)  +
      ease_aes('linear') + 
      NULL
    
  }
  
}


#plots 

plot_field <- function(ymin = 0, ymax = 120) {
  cols_fill <- c("#FB4F14", "#663300", "#A5ACAF")
  cols_col <- c("#000000", "#663300", "#000000")
  
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  #specific boundaries
  ymin <- max(round(ymin - 10, -1), 0)
  ymax <- min(round(ymax + 10, -1), 120)
  
  #hash marks
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  ggplot() +
    
    #adding hash marks
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    
    #adding yard lines
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    
    #adding field yardline text
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    
    #adding field exterior
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    
    #applying plot limits
    ylim(ymin, ymax) + 
    coord_fixed() +
    
    #theme
    theme_minimal() +
    
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
  
}

#player influence plot
#run player influence function first to get required dist params
plot_field(x - 5, x + 5) +
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
    values = c(NA, "#DDEBF7", "#BDD7EE", "#9BC2E6", "#2F75B5"),
    name = "Player Influence"
  ) 

#requires grid of player influence densities as input found in main script 
clust_plot <- 1 # select cluster centroid to plot 

df_grid <- expand.grid(
  x = seq(ball_snap_x - 5, ball_snap_x + 25, by = 1),
  y = seq(26, 53, by = 1),
  frame_ids = c(21, 31, 41)
) %>%
  arrange(x, y, frame_ids) %>%
  mutate(
    team_infl = unlist(cluster_df[clust_plot, -1]),
    frame_ids = case_when(
      frame_ids == 21 ~ "1 second from snap",
      frame_ids == 31 ~ "2 seconds from snap",
      TRUE ~ "3 seconds from snap"
    )
  )

ggplot() +
  geom_contour_filled(
    data = df_grid,
    aes(53.3 - y, x - ball_snap_x, z = team_infl),
    alpha = 0.8,
    bins = 5
  ) + 
  coord_fixed(ratio = 1.05) +
  scale_fill_manual(
    values = c(NA, "#ccece6", "#99d8c9", "#2ca25f", "#006d2c"),
    name = "Influence"
  ) +
  facet_wrap(vars(frame_ids), ncol = 3) +
  ylab("Yards from LOS") +
  xlab("") +
  ylim(c(-5, 25)) +
  xlim(c(0, 53.33)) +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.background = element_rect(fill = NA),
    strip.text = element_text(size = 5, lineheight = 0.01),
    strip.background = element_rect(size = 0.5),
    axis.text.y = element_text(size = 5),
    axis.title.y = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.title = element_text(size= 5),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm")
  )

#sina SHAP plot
#df input from main script 
shap_df_long %>%
  ggplot(aes(var, shap)) +
  ggforce::geom_sina() +
  coord_flip() +
  labs(x = "", y = "SHAP Value", title = "SHAP Values for EPA Model Features")