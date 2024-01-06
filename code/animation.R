library(tidyverse)
library(ggplot2)

orientation <- master_in_zone %>%
  mutate(
    left_o = if_else(type %in% c("lb", "tackler"), (o + shift_angle) %% 360, NA),
    right_o = if_else(type %in% c("lb", "tackler"), (o - shift_angle) %% 360, NA),
    lb_oR = if_else(type %in% c("lb", "tackler"), o * (pi / 180), NA),
    left_oR = left_o * (pi / 180),
    right_oR = right_o * (pi / 180),
    lb_xN = if_else(type %in% c("lb", "tackler"), x + magnitude * cos(lb_oR), NA),
    lb_yN = if_else(type %in% c("lb", "tackler"), y + magnitude * sin(lb_oR), NA),
    lb_xL = x + magnitude * cos(left_oR),
    lb_yL = y + magnitude * sin(left_oR),
    lb_xR = x + magnitude * cos(right_oR),
    lb_yR = y + magnitude * sin(right_oR),
    type = if_else(type == "bc" & is_bc_in_zone == 1, "bc_in_zone", type))

mycolors <- c(
  "fb" = "#492E22",
  "bc" = "black",
  "lb" = "white",
  "tackler" = "orange",
  "bc_in_zone" = "green"
)

mysize <- c("fb" = 1,
            "bc" = 3,
            "lb" = 3,
            "tackler" = 4,
            "bc_in_zone" = 4
)

myalpha <- c("football" = 1,
             "bc" = 0.7,
             "lb" = 0.7,
             "tackler" = 1,
             "bc_in_zone" = 0.7
)


library(gganimate)

#NFL field in ggplot (credit to Marschall Furman)
source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')


animate_play <- function(game_input, play_input) {
  
  data_animation <- orientation %>%
    select(gameId, playId, time, x, y, frameId, type, displayName, 
           lb_xN, lb_yN, lb_xL, lb_yL, lb_xR, lb_yR, left_o, right_o) %>%
    filter(gameId == game_input,
           playId == play_input) %>%
    mutate(to_label = if_else(type == "lb", displayName, " ")) %>%
    distinct() 
  
  
  action <- data_animation %>%
    filter(type == "lb")
  
  start_time <- first(action$time)
  
  data_animation <- data_animation %>% filter(time >= start_time)
  
  play_animation <- 
    ggplot(data = data_animation,
           aes(
             x = x,
             y = y,
             color = type,
             size = type
           )) +
    gg_field() +
    scale_colour_manual(values = mycolors) +
    scale_size_manual(values = mysize) +
    scale_alpha_manual(values = myalpha) +
    geom_point(show.legend = FALSE, aes(alpha = type)) +
    #   geom_segment(aes(x = x, y = y, xend = lb_xN, yend = lb_yN), color = "yellow", linewidth = 1, na.rm = TRUE, show.legend = FALSE) +
    geom_segment(aes(x = x, y = y, xend = lb_xL, yend = lb_yL), color = "yellow", linewidth = 1, na.rm = TRUE, show.legend = FALSE) + 
    geom_segment(aes(x = x, y = y, xend = lb_xR, yend = lb_yR), color = "yellow", linewidth = 1, na.rm = TRUE, show.legend = FALSE) + 
    geom_curve(aes(x = lb_xR, y = lb_yR, xend = lb_xL, yend = lb_yL), color = "yellow", linewidth = 1, na.rm = TRUE, show.legend = FALSE) + 
    geom_text(
      data = data_animation,
      aes(label = to_label, x = x, y = y),
      size = 3,
      color = "black",
      show.legend = FALSE,
      fontface = "bold",
      vjust = -0.5
    ) +
    geom_text(data = data_animation, aes(label = as.character(frameId), x = 35, y = 56.5), size = 5, color = "black", show.legend = FALSE) + 
    transition_time(time) +
    labs(title = "Date - Time: {format(frame_time, '%Y/%m/%d %H:%M:%OS2')} FRAME:")
  
  anim_save(
    'play.gif',
    animate(
      play_animation,
      width = 840,
      height = 374,
      fps = 20,
      duration = 8,
      end_pause = 30
    )
  )
}

# animate_play(2022091105, 3461)
