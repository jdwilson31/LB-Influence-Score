## Extracting Data and creating new Data Frames for quicker loading time

library(tidyverse) 

## Loading all data and combining

games <- read.csv("../input/nfl-big-data-bowl-2024/games.csv")
players <- read.csv("../input/nfl-big-data-bowl-2024/players.csv")
plays <- read.csv("../input/nfl-big-data-bowl-2024/plays.csv")
tackles <- read.csv("../input/nfl-big-data-bowl-2024/tackles.csv")

track_week1 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_1.csv")
track_week2 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_2.csv")
track_week3 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_3.csv")
track_week4 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_4.csv")
track_week5 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_5.csv")
track_week6 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_6.csv")
track_week7 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_7.csv")
track_week8 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_8.csv")
track_week9 <- read.csv("../input/nfl-big-data-bowl-2024/tracking_week_9.csv")

track_week_total <- rbind(track_week1, track_week2, track_week3, track_week4, track_week5, track_week6, track_week7, track_week8, track_week9)

## Filtering plays

run_plays <- plays %>%
  filter(grepl("pass ", playDescription) == FALSE, grepl("scramble", playDescription) == FALSE,
         grepl("TOUCHDOWN", playDescription) == FALSE, !is.na(offenseFormation),
         !(offenseFormation %in% c("JUMBO", "EMPTY", "I_FORM", "WILDCAT"))) 


## Creating new plays DF

plays_master <- run_plays %>%
  inner_join(tackles, by = c("gameId", "playId"))

write_csv(plays_master, '/kaggle/working/plays_master.csv')

## Creating new Tracking Data DF

tracking_master <- track_week_total %>%
  distinct() %>%
  inner_join(plays_master, by = c("gameId", "playId"), relationship = "many-to-many")

write_csv(tracking_master, '/kaggle/working/tracking_master.csv')
