library(tidyverse)
library(cluster)
library(ggplot2)

### Creating individual metrics

## Getting play information

playDirInfo <- master_in_zone %>%
  filter(type %in% c("lb", "tackler")) %>%
  group_by(gameId, playId, nflId) %>% 
  summarize(lb_start = first(x), bc_start = first(bc_x), los = first(absoluteYardlineNumber)) %>%
  mutate(play_direction = if_else(bc_start > lb_start, "left", "right")) %>%
  select(-lb_start, -bc_start)

master_in_zone <- master_in_zone %>%
  left_join(playDirInfo, by = c("gameId", "playId", "nflId"), relationship = "many-to-many")


## Getting info needed for metrics

tmp_master_in_zone <- master_in_zone %>%
  group_by(gameId, playId, nflId) %>%
  mutate(speed_change = if_else(is_bc_in_zone == 1, bc_s - lag(bc_s), 0),
         dir_change = if_else(is_bc_in_zone == 1, bc_dir - lag(bc_dir), 0),
         dir_change = pmin(abs(dir_change), 360 - abs(dir_change)),
         entered_zone = if_else(is_bc_in_zone == 1 & lag(is_bc_in_zone, default = 0) == 0, bc_x, NA),
         first_frame_in_zone = if_else(cumsum(!is.na(entered_zone)) > 1, NA, entered_zone),
         bc_end_x = if_else(event %in% c("tackle", "qb_slide", "fumble", "out_of_bounds", 
                                         "safety", "fumble_defense_recovered"), bc_x, NA),
         dis_in_zone = if_else(is_bc_in_zone == 1, dis, 0),
         loc_of_influence = if_else(is_bc_in_zone == 1, x - los, NA),
         loc_of_influence = if_else(play_direction == "right", -1*loc_of_influence, loc_of_influence)
  ) %>%
  ungroup()


## Calculating all metrics into final_data

final_data <- tmp_master_in_zone %>%
  filter(type %in% c("lb", "tackler")) %>%
  group_by(gameId, playId, nflId) %>%
  summarize(count = n(), player = last(displayName), 
            frames_in_zone = sum(is_bc_in_zone, na.rm = TRUE),
            percent_frames_in_zone = frames_in_zone / count,
            total_change_in_speed = round(sum(speed_change, na.rm = TRUE) / frames_in_zone, 2),
            total_change_in_dir = round(sum(dir_change, na.rm = TRUE) / frames_in_zone, 2),
            yards_after_zone = first(first_frame_in_zone, na_rm = TRUE) - last(bc_end_x, na_rm = TRUE),
            total_dis_in_zone = round(sum(dis_in_zone) / frames_in_zone, 2), 
            avg_influence_loc = round(mean(loc_of_influence, na.rm = TRUE), 2),
            avg_dist_to_ball = round(mean(dist_to_bc), 2),
            solo_tackle = 0,
            assist_tackle = 0,
            in_on_tackle = 0,
            playDir = last(play_direction),
            playDesc = last(playDescription),
            yards_gained = last(prePenaltyPlayResult),
            tackler1 = last(tackler1Id),
            tackler2 = last(tackler2Id),
            player_team = last(club),
            offensive_team = last(possessionTeam)
  ) %>%
  mutate(yards_after_zone = if_else(playDir == "right", -1 * yards_after_zone, yards_after_zone),
         yards_after_zone = if_else(frames_in_zone != 0, 
                                    yards_after_zone,
                                    if_else(yards_gained < 0, 0, yards_gained)),
         avg_influence_loc = if_else(frames_in_zone != 0,
                                     avg_influence_loc,
                                     if_else(yards_gained < 0, 0, -1 * yards_gained)),
         total_change_in_speed = if_else(frames_in_zone != 0,
                                         total_change_in_speed,
                                         0.25),
         total_change_in_dir= if_else(frames_in_zone != 0,
                                      total_change_in_dir,
                                      0),
         total_dis_in_zone = if_else(frames_in_zone != 0,
                                     total_dis_in_zone,
                                     0),
         solo_tackle = if_else(nflId == tackler1 & is.na(tackler2), 1, 0),
         assist_tackle = if_else((nflId == tackler1 & !is.na(tackler2)) | nflId == tackler2, 1, 0),
         assist_tackle = if_else(is.na(assist_tackle), 0, assist_tackle),
         in_on_tackle = if_else(solo_tackle == 1 | assist_tackle == 1, 1, 0)
  )



### K-Means Clustering

## Separating Plays
in_zone_plays <- final_data %>%
  filter(frames_in_zone > 0)

not_in_zone <- final_data %>%
  filter(frames_in_zone == 0)


## Elbow Method

#7 - percent_frames_in_zone
#8 - total_change_in_speed
#9 - total_change_in_dir
#10 - yards_after_zone
#11 - total_dis_in_zone
#12 - avg_influence_loc
#13 - avg_dist_to_ball

clustering_data <- in_zone_plays[,7:13] %>%
  select(-total_dis_in_zone) %>%
  mutate(total_change_in_dir = log(total_change_in_dir + 1))

#clustering_data
#change range for adding more variables to df

#clustering_data_ranked <- apply(clustering_data, 2, rank)
set.seed(33)


#ELBOW METHOD
k_values <- 1:50
inertias <- numeric(length(k_values))

for (k in k_values) {
  km_model <- kmeans(clustering_data, centers = k, iter.max = 20)
  inertias[k] <- km_model$tot.withinss
}

plot(k_values, inertias, type = "b", pch = 19, frame = FALSE, col = "blue", main = "Elbow Method", xlab = "Number of Clusters (k)", ylab = "Inertia")


## Creating The Clusters

clustering_data <- in_zone_plays[,7:13] %>%
  select(-total_dis_in_zone) %>%
  mutate(total_change_in_dir = log(total_change_in_dir + 1))

number_runs <- 10

K_means_data <- in_zone_plays

for (i in 1:number_runs) {
  
  model <- kmeans(clustering_data, 10, iter.max = 25, nstart = 25)
  
  K_means_data$cluster <- as.factor(model$cluster) 
  
  centers <- model$centers
  centers_ranked <- as.data.frame(centers)
  centers_ranked <- centers_ranked %>%
    mutate(cluster = as.factor(row_number()))
  centers_ranked$framesR <- rank(centers_ranked$percent_frames_in_zone)
  centers_ranked$speedR <- rank(centers_ranked$total_change_in_speed * -1)
  centers_ranked$dirR <- rank(centers_ranked$total_change_in_dir)
  centers_ranked$yards_afterR <- rank(centers_ranked$yards_after_zone * -1)
  centers_ranked$inf_locR <- rank(centers_ranked$avg_influence_loc)
  centers_ranked$avg_disR <- rank(centers_ranked$avg_dist_to_ball)
  
  centers_ranked$total_rank <- rowMeans(centers_ranked[, c("framesR", "speedR", "dirR", "yards_afterR", "inf_locR", "avg_disR")])
  
  vars <- c("framesR", "speedR", "dirR", "yards_afterR", "inf_locR", "avg_disR")
  weights <- c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1)
  
  centers_ranked$weight_rank <- rowMeans(centers_ranked[vars] * weights)
  
  centers_ranked$cluster_rank <- rank(centers_ranked$total_rank)
  
  centers_ranked$weight_cluster_rank <- rank(centers_ranked$weight_rank)
  
  col_names <- c("cluster", paste0("weight_cluster_rank_", i))
  
  centers_ranked_join <- centers_ranked %>%
    select(cluster, weight_cluster_rank)
  
  colnames(centers_ranked_join) <- col_names
  
  K_means_data <- K_means_data %>%
    left_join(centers_ranked_join, by = "cluster")
  
}

variable_columns <- grep("weight_cluster_rank_", colnames(K_means_data), value = TRUE)

K_means_data$total_score <- rowMeans(K_means_data[, variable_columns])   

## Left with K_means_data where total score in the plays Influence Score

## Looking into the clusters
cluster_info <- K_means_data %>%
  group_by(cluster) %>%
  summarize(count = n(), 
            avg_frames = mean(percent_frames_in_zone),
            avg_speed = mean(total_change_in_speed),
            avg_dir = mean(total_change_in_dir),
            avg_yards = mean(yards_after_zone),
            avg_dis = mean(total_dis_in_zone),
            avg_loc = mean(avg_influence_loc),
            avg_from_ball = mean(avg_dist_to_ball),
            avg_rank = mean(total_score))

cluster_info %>%
  arrange(-avg_rank)


## Creating the Player Ranking
not_in_zone_cluster <- not_in_zone %>%
  mutate(cluster = as.factor(0), 
         cluster_rank = 0, 
         weight_cluster_rank_1 = 0, 
         total_score = 0,
         in_zone_play = 0, great_play = 0)

cluster_players <- K_means_data %>%
  mutate(in_zone_play = 1, 
         great_play = if_else(total_score >= 8.5, 1, 0)) %>%
  rbind(not_in_zone_cluster) %>%
  group_by(player) %>%
  summarize(count = n(), avg_score = mean(total_score), in_zone_count = sum(in_zone_play), team = last(player_team),
            out_of_zone_count = count - in_zone_count, per_in_zone = in_zone_count / count, tackles = sum(in_on_tackle), great_plays = sum(great_play)) %>%
  filter(count >= 50) %>%
  arrange(-avg_score) %>%
  mutate(rank = row_number())
