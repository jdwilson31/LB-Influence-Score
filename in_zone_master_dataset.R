## Creating new Data Set

library(data.table)
library(tidyverse)
library(cluster)
library(ggplot2)
library(gt)


### Setting up the data

list.files(path = "../input")

# Load new tracking master, and play master as well as everything else

tracking_master <- fread("../input/one-back-run-plays/tracking_master.csv") %>%
  select(-nflId.y, -tackle, -assist, -forcedFumble, -pff_missedTackle) %>%
  distinct() %>%
  rename(nflId = nflId.x)

games <- read.csv("../input/nfl-big-data-bowl-2024/games.csv")
players <- read.csv("../input/nfl-big-data-bowl-2024/players.csv")
plays <- read.csv("../input/nfl-big-data-bowl-2024/plays.csv")
tackles <- read.csv("../input/nfl-big-data-bowl-2024/tackles.csv")

plays_master <- read.csv("../input/one-back-run-plays/plays_master.csv")

## Defines the list of all linebackers

linebackers <- players %>%
  filter(position %in% c("MLB", "ILB", "OLB"), weight <= 255) 

linebacker_Ids <- linebackers$nflId


## Reduces all tackler information into one row for each play

play_tacklers <- tackles %>%
  filter(tackle == 1 | assist == 1) %>%
  group_by(gameId, playId) %>%
  summarize(tackler1Id = first(nflId), tackler2Id = last(nflId)) %>%
  mutate(tackler2Id = if_else(tackler1Id == tackler2Id, NA, tackler2Id))

play_fumble_players <- tackles %>%
  filter(forcedFumble == 1) %>%
  group_by(gameId, playId) %>%
  summarize(forcedFumbleId = first(nflId))

tackles_by_play <- play_tacklers %>%
  left_join(play_fumble_players, by = c("gameId", "playId"))

missed_tacklers <- tackles %>%
  filter(pff_missedTackle == 1) %>%
  group_by(gameId, playId) %>%
  mutate(missed_tackle_1Id = ifelse(row_number() == 1, nflId, NA),
         missed_tackle_2Id = ifelse(row_number() == 2, nflId, NA),
         missed_tackle_3Id = ifelse(row_number() == 3, nflId, NA),
         missed_tackle_4Id = ifelse(row_number() == 4, nflId, NA),
         missed_tackle_5Id = ifelse(row_number() == 5, nflId, NA),
         missed_tackle_6Id = ifelse(row_number() == 6, nflId, NA)) %>%
  summarize(across(starts_with("missed_tackle"), ~first(na.omit(.))))

tackles_by_play <- tackles_by_play %>%
  left_join(missed_tacklers, by = c("gameId", "playId"))



## Combines expanded player information with original information

tackles_reduced <- tackles %>%
  group_by(gameId, playId) %>%
  summarize(tackle = if_else(sum(tackle) > 0 & sum(assist) < 1, 1, 0), 
            assist = if_else(sum(assist) > 0, 1, 0), 
            forcedFumble = sum(forcedFumble),
            pff_missedTackles = sum(pff_missedTackle)
  ) %>%
  left_join(tackles_by_play, by = c("gameId", "playId"))

## Adding reduced tackle data to tracking_master

tracking_master <- tracking_master %>%
  left_join(tackles_reduced, by = c("gameId", "playId"))


### Creating Master Data

## Function to scale speed to shift angle for zone
scale_speed_to_angle <- function(speed) {
  min_speed <- 0
  max_speed <- 8.8
  min_angle <- 20
  max_angle <- 60
  
  new_speed <- if_else(speed > 8.8, 8.8, speed)
  
  # Use linear scaling
  scaled_angle <- (1 - ((new_speed - min_speed) / (max_speed - min_speed))) * (max_angle - min_angle) + min_angle
  
  return(scaled_angle)
}


## Function for speed to magnituded
# yps -> yards

scale_speed_to_distance <- function(speed) {
  max_yards_coverable <- 10
  scaled_distance <- ((-1 * max_yards_coverable) / ((.5 * speed) + 1)) + max_yards_coverable + 1
  return(0.75 * scaled_distance)
}


## Focusing on the key parts

key_players <- tracking_master %>%
  group_by(gameId, playId) %>%
  mutate(
    tackle_index = if_else(event %in% c("tackle", "qb_slide", "fumble", "out_of_bounds", 
                                        "safety", "fumble_defense_recovered"), frameId, NA),  # Create an index for tackle events
    tackle_index = max(tackle_index, na.rm = TRUE),  # Replace NAs with the last known tackle index
    after_tackle = if_else(frameId >= tackle_index, 1, 0)
  ) %>%
  ungroup() %>%
  filter(nflId == ballCarrierId | nflId %in% linebacker_Ids | displayName == "football") %>%
  mutate(
    type = case_when(
      (nflId == tackler1Id | nflId == tackler2Id) & after_tackle == 1 ~ "tackler",
      nflId == ballCarrierId ~ "bc",
      nflId %in% linebacker_Ids ~ "lb",
      displayName == "football" ~ "fb"
    ),
    o = ((90 - o) %% 360),
    dir = ((90 - dir) %% 360),
    shift_angle = scale_speed_to_angle(s),
    magnitude = scale_speed_to_distance(s))

## This is a function to determine distance between two points

distance <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(dist)
}

## Function to calculate when a bc is in the zone

in_zone <- function(lb_x, lb_y, o, mag, bc_x, bc_y, shift_angle) {
  # Calculate angle between RB and LB for all rows
  phi <- ((atan2(bc_y - lb_y, bc_x - lb_x)) * (180 / pi)) %% 360
  
  # Calculate distance between RB and LB for all rows
  dist <- distance(lb_x, lb_y, bc_x, bc_y)
  
  # Create a logical vector indicating if the condition is met for each row
  condition <- (dist <= mag) & (pmin(abs(o - phi), 360 - abs(o - phi)) <= shift_angle)
  
  # Return the logical vector
  return(condition)
}


## Creating master data set

bc_locations <- key_players %>%
  filter(type == "bc") %>%
  select(gameId, playId, frameId, x, y, s, dir) %>%
  rename(bc_x = x, bc_y = y, bc_s = s, bc_dir = dir)

fb_locations <- key_players %>%
  filter(type == "fb") %>%
  select(gameId, playId, frameId, x, y) %>%
  rename(fb_x = x, fb_y = y)

tmp_in_zone <- key_players %>% 
  left_join(bc_locations, by = c("gameId", "playId", "frameId")) %>%
  left_join(fb_locations, by = c("gameId", "playId", "frameId")) %>%
  mutate(dist_to_bc = distance(x, y, bc_x, bc_y),
         next_to_bc = if_else(dist_to_bc <= .5, 1, 0), 
         is_bc_in_zone = if_else(type %in% c("lb", "tackler"),
                                 as.numeric(in_zone(x, y, o, magnitude, bc_x, bc_y, shift_angle)),
                                 NA),
         is_bc_in_zone = if_else(is_bc_in_zone == 0 & next_to_bc == 1, 1, is_bc_in_zone))

in_zone_data <- tmp_in_zone %>%
  filter(!is.na(is_bc_in_zone)) %>%
  group_by(gameId, playId, frameId) %>%
  summarize(is_bc_in_zone = max(is_bc_in_zone))

lb_in_zone <- tmp_in_zone %>%
  filter(type %in% c("lb", "tackler"))

other_in_zone <- tmp_in_zone %>%
  filter(!(type %in% c("lb", "tackler"))) %>%
  select(-is_bc_in_zone) %>%
  left_join(in_zone_data, by = c("gameId", "playId", "frameId"))

master_in_zone <- rbind(lb_in_zone, other_in_zone)
