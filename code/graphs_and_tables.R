library(ggrepel)
library(ggplot2)
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(gt)

cluster_players_graph <- cluster_players %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Tackles Vs Score Graph
cluster_players_graph %>%
  ggplot(aes(x = avg_score, y = tackles)) + 
  geom_smooth(method = "lm", se = F, color = "grey", alpha = .75) +
  geom_hline(yintercept = mean(cluster_players_graph$tackles), color = "red", linetype = "dashed", alpha = .7) +
  geom_vline(xintercept = mean(cluster_players_graph$avg_score), color = "red", linetype = "dashed", alpha = .7) +
  geom_point(color = cluster_players_graph$team_color, cex = cluster_players_graph$count/ 30) +
  geom_text_repel(aes(label = player), 
                  point.padding = 2,
                  box.padding = .5,
                  segment.color = "black",
                  segment.size = 0.5,
                  max.overlaps = 8) +
  theme_bw() + 
  labs(
    title = "Linebacker Influence Score VS Tackles Made",
    subtitle = "For Linebackers with more then 50 opportunities in the first half of 2022",
    x = "Influence Score",
    y = "Tackles Made"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title = element_text(size = 15)
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  annotate("text", label = "High Tackles\nLow Score", x = 5.1, y = 48, size = 7, color = "firebrick") + 
  annotate("text", label = "High Tackles\nHigh Score", x = 7.25, y = 48, size = 7, color = "firebrick") + 
  annotate("text", label = "Low Tackles\nHigh Score", x = 7.25, y = 5, size = 7, color = "firebrick") + 
  annotate("text", label = "Low Tackles\nLow Score", x = 5, y = 13, size = 7, color = "firebrick")


## Top 20 Table

top_20 <- cluster_players %>%
  filter(rank <= 20)

top_20_tble <- top_20 %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) %>%
  select(player, team_wordmark, avg_score, rank, count, in_zone_count, per_in_zone, tackles, great_plays) %>%
  mutate(per_in_zone = round(per_in_zone * 100, 2),
         avg_score = round(avg_score, 2))


top_20_tble %>%
  select(-rank) %>%
  # filter(rank <= 10) %>%
  gt() %>%
  cols_label(
    X = "Rank",
    avg_score = "Avg Influence Score",
    player = "Linebacker",
    count = "Number of plays",
    in_zone_count = "# Plays with influence",
    per_in_zone = "% Of plays influenced",
    tackles = "Tackles",
    great_plays = "Great plays",
    team_wordmark = "Team") %>%
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_header(title = "Most Influential Linebackers",
             subtitle = "On Singleback/Shotgun Run Plays in the first half of 2022") %>% 
  data_color(count,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(min(top_20_tble$count),
                                                 max(top_20_tble$count)))) %>% 
  data_color(in_zone_count,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(min(top_20_tble$in_zone_count),
                                                 max(top_20_tble$in_zone_count)))) %>%  
  data_color(per_in_zone,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(min(top_20_tble$per_in_zone),
                                                 max(top_20_tble$per_in_zone)))) %>%  
  data_color(tackles,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(min(top_20_tble$tackles),
                                                 max(top_20_tble$tackles)))) %>%  
  data_color(great_plays,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(min(top_20_tble$great_plays),
                                                 max(top_20_tble$great_plays)))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_text(size = "larger", weight = "bold"),
            locations = cells_body(columns = avg_score)) %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.weight = "bolder",
  ) %>% 
  cols_align(
    align = "center"
  )

## Dre Greenlaw Vs Fred Warner Example Figures

# Fred Warner First

examples <- master_in_zone %>%
  filter(gameId == 2022091102, playId == 467, type %in% c("lb", "tackler"),
         displayName == "Fred Warner") %>%
  mutate(is_bc_in_zone = if_else(is_bc_in_zone == 1, "yes", "no"))

# Shows change in speed
examples %>%
  ggplot(aes(x = frameId, y = bc_s)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) + 
  theme_bw() + 
  labs(
    title = "Warner's Influence on DJ Montgomery's Speed",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Frame ID",
    y = "Ball Carrier Speed (Yards/Sec)",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_color_manual(values=c("grey30", "lightgreen"))

# Shows change in Dir
examples %>%
  mutate(bc_dirG = if_else(bc_dir < 50, bc_dir + 360, bc_dir)) %>%
  ggplot(aes(x = frameId, y = bc_dirG)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) + 
  theme_bw() + 
  labs(
    title = "Warner's Influence on DJ Montgomery's Direction",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Frame ID",
    y = "Ball Carrier Direction (Degree)",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen"))

# Shows dist to bc
examples %>%
  group_by(gameId, playId) %>%
  mutate(frames_in = if_else(is_bc_in_zone == 1, frameId, NA),
         first_frame_in = first(frames_in, na_rm = TRUE)) %>%
  ungroup() %>%
  mutate(after_first_frame = if_else(frameId >= first_frame_in, 1, 0)) %>%
  ggplot(aes(x = frameId, y = dist_to_bc)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) + 
  theme_bw() + 
  labs(
    title = "Warner's Distance From DJ Montgomery",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Frame ID",
    y = "Distance to Ball Carrier (Yards)",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen")) + 
  geom_hline(yintercept = 6.7) 


# Shows Yards gained after zone and average influence location
examples %>%
  group_by(gameId, playId) %>%
  mutate(frames_in = if_else(is_bc_in_zone == 1, frameId, NA),
         first_frame_in = first(frames_in, na_rm = TRUE)) %>%
  ungroup() %>%
  mutate(after_first_frame = if_else(frameId >= first_frame_in, 1, 0)) %>%
  ggplot(aes(x = bc_x, y = bc_y)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) +
  theme_bw() + 
  labs(
    title = "Warner's Influence Related To DJ Montgomery's Location",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Ball Carrier X Coordinate",
    y = "Ball Carrier Y Coordinate",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen")) + 
  geom_segment(xend = 75.48, x = 79.69, y = 24.92, yend = 24.92, 
               arrow = arrow(length = unit(0.2, "inches")), lineend = "round", linejoin = "bevel") + 
  geom_vline(xintercept = examples$los, color = "khaki1", linewidth = 1)


# Dre Greenlaw

examples <- master_in_zone %>%
  filter(gameId == 2022091102, playId == 467, type %in% c("lb", "tackler"),
         displayName == "Dre Greenlaw")


# Shows change in speed
examples %>%
  ggplot(aes(x = frameId, y = bc_s)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) + 
  theme_bw() + 
  labs(
    title = "Greenlaw's Influence on DJ Montgomery's Speed",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Frame ID",
    y = "Ball Carrier Speed (Yards/Sec)",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen"))

# Shows change in Dir
examples %>%
  mutate(bc_dirG = if_else(bc_dir < 50, bc_dir + 360, bc_dir)) %>%
  ggplot(aes(x = frameId, y = bc_dirG)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) + 
  theme_bw() + 
  labs(
    title = "Greenlaw's Influence on DJ Montgomery's Direction",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Frame ID",
    y = "Ball Carrier Direction (Degree)",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen"))

# Shows dist to bc
examples %>%
  group_by(gameId, playId) %>%
  mutate(frames_in = if_else(is_bc_in_zone == 1, frameId, NA),
         first_frame_in = first(frames_in, na_rm = TRUE)) %>%
  ungroup() %>%
  mutate(after_first_frame = if_else(frameId >= first_frame_in, 1, 0)) %>%
  ggplot(aes(x = frameId, y = dist_to_bc)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) + 
  theme_bw() + 
  labs(
    title = "Greenlaw's Distance From DJ Montgomery",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Frame ID",
    y = "Distance to Ball Carrier (Yards)",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen")) + 
  geom_hline(yintercept = 6.75) 


# Shows Yards gained after zone and average influence location
examples %>%
  group_by(gameId, playId) %>%
  mutate(frames_in = if_else(is_bc_in_zone == 1, frameId, NA),
         first_frame_in = first(frames_in, na_rm = TRUE)) %>%
  ungroup() %>%
  mutate(after_first_frame = if_else(frameId >= first_frame_in, 1, 0)) %>%
  ggplot(aes(x = bc_x, y = bc_y)) + 
  geom_point(aes(color = as.factor(is_bc_in_zone)), size = 3, show.legend = F) + 
  geom_line(color = "grey", alpha = .75) +
  theme_bw() + 
  labs(
    title = "Greenlaw's Influence Related To DJ Montgomery's Location",
    subtitle = "Bears VS 49ers 1st Qtr: (8:22) D.Montgomery left guard to CHI 35 for 3 yards (T.Hufanga, F.Warner).",
    x = "Ball Carrier X Coordinate",
    y = "Ball Carrier Y Coordinate",
    color = "Ball Carrier in Influence Zone"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 23, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 12),
    axis.title = element_text(size = 15) 
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_color_manual(values=c("grey30", "lightgreen")) + 
  geom_segment(xend = 75.48, x = 81.43, y = 24.39, yend = 24.39, 
               arrow = arrow(length = unit(0.2, "inches")), lineend = "round", linejoin = "bevel") + 
  geom_vline(xintercept = 78, color = "khaki1", linewidth = 1)


## Team Data and Graphs

# D Ranks
cluster_team_data_D <- K_means_data %>%
  mutate(in_zone_play = 1, 
         great_play = if_else(total_score >= 8.5, 1, 0)) %>%
  rbind(not_in_zone_cluster) %>%
  group_by(gameId, playId, player_team) %>%
  summarize(agregate_score = mean(total_score), yards = last(yards_gained)) %>%
  group_by(player_team) %>%
  summarize(count = n(), team_score = mean(agregate_score), avg_yards = sum(yards) / count)

# D Score vs Yards
cluster_team_data_D %>%
  ggplot(aes(x = team_score, y = avg_yards)) + 
  geom_smooth(method = "lm", se = F, color = "grey", alpha = .75) +
  geom_hline(yintercept = mean(cluster_team_data_D$avg_yards), color = "red", linetype = "dashed", alpha = .75) + 
  geom_vline(xintercept = mean(cluster_team_data_D$team_score), color = "red", linetype = "dashed", alpha = .75) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = player_team), width = .075, alpha = .75) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 6)) + 
  theme_bw() + 
  labs(
    title = "Average Linebacker Core Influence Score VS Average Yards Allowed",
    subtitle = "For all Singleback/Shotgun Run plays in the first half of 2022",
    x = "Average Linebacker Core Influence Score",
    y = "Average Yards Allowed"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title = element_text(size = 15)
  ) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  annotate("text", label = "Low Yards Allowed\nLow LB Core Score", x = 5.65, y = 3.5, size = 7, color = "firebrick") + 
  annotate("text", label = "High Yards Allowed\nLow LB Core Score", x = 5.62, y = 4.75, size = 7, color = "firebrick") +  
  annotate("text", label = "Low Yards Allowed\nHigh LB Core Score", x = 7.02, y = 3.6, size = 7, color = "firebrick") +
  annotate("text", label = "High Yards Allowed\nHigh LB Core Score", x = 7, y = 5.3, size = 7, color = "firebrick")

# D Table
cluster_team_data_D %>% 
  arrange(-team_score) %>% 
  left_join(teams_colors_logos, by = c("player_team" = "team_abbr")) %>% 
  mutate(rank = row_number(),
         team_score = round(team_score, 3),
         avg_yards = round(avg_yards, 2)) %>% 
  filter(rank <= 10) %>% 
  select(team_wordmark, rank, count, team_score, avg_yards) %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    rank = "Rank",
    count = "# of plays",
    team_score = "Average Score Amongst Linebackers",
    avg_yards = "Average Yards given up"
  ) %>% 
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_header(
    title = "Best Linebacker Cores based on Influence",
    subtitle = "On all Singleback/Shotgun run plays in the first half of 2022"
  ) %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.weight = "bolder") %>% 
  cols_align(
    align = "center") %>% 
  data_color(team_score,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(6.5, 7.22))) %>%  
  data_color(avg_yards,
             fn = scales::col_numeric(palette = c("lightgreen", "lightyellow1"),
                                      domain = c(3.36,
                                                 4.96)))


# O Ranks
cluster_team_data_O <- K_means_data %>%
  mutate(in_zone_play = 1, 
         great_play = if_else(total_score >= 8.5, 1, 0)) %>%
  rbind(not_in_zone_cluster) %>%
  group_by(gameId, playId, offensive_team) %>%
  summarize(agregate_score = mean(total_score), yards = last(yards_gained)) %>%
  group_by(offensive_team) %>%
  summarize(count = n(), team_score = mean(agregate_score), avg_yards = sum(yards) / count)

# O Yards vs Score
cluster_team_data_O %>%
  ggplot(aes(x = team_score, y = avg_yards)) + 
  geom_smooth(method = "lm", se = F, color = "grey", alpha = .75) +
  geom_hline(yintercept = mean(cluster_team_data_O$avg_yards), color = "red", linetype = "dashed", alpha = .75) + 
  geom_vline(xintercept = mean(cluster_team_data_O$team_score), color = "red", linetype = "dashed", alpha = .75) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = offensive_team), width = .075, alpha = .75) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 6)) + 
  theme_bw() + 
  labs(
    title = "Average Opposing Linebacker Core Influence Score VS Average Yards Gained",
    subtitle = "For all Singleback/Shotgun Run plays in the first half of 2022",
    x = "Average Opposing Linebacker Core Influence Score",
    y = "Average Yards Gained"
  ) + 
  theme(
    plot.title = element_text(hjust = .5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 15),
    axis.title = element_text(size = 15)
  ) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  annotate("text", label = "Low Yards Gained\nLow LB Core Score", x = 6.05, y = 3.5, size = 7, color = "firebrick") + 
  annotate("text", label = "High Yards Gained\nLow Opp. LB Score", x = 5.96, y = 5.05, size = 6, color = "firebrick") +  
  annotate("text", label = "Low Yards Gained\nLow Opp. LB Score", x = 6.92, y = 3.25, size = 7, color = "firebrick") +
  annotate("text", label = "High Yards Gained\nHigh Opp. LB Score", x = 6.85, y = 5.1, size = 7, color = "firebrick")

# O Table
cluster_team_data_O %>% 
  arrange(team_score) %>% 
  left_join(teams_colors_logos, by = c("offensive_team" = "team_abbr")) %>% 
  mutate(rank = row_number(),
         team_score = round(team_score, 3),
         avg_yards = round(avg_yards, 2)) %>% 
  filter(rank <= 10) %>% 
  select(team_wordmark, rank, count, team_score, avg_yards) %>% 
  gt() %>% 
  cols_label(
    team_wordmark = "Team",
    rank = "Rank",
    count = "# of plays",
    team_score = "Average Opp. Linebacker Score",
    avg_yards = "Average Yards Gained"
  ) %>% 
  gtExtras::gt_theme_espn() %>%
  gtExtras::gt_img_rows(team_wordmark) %>% 
  tab_header(
    title = "Best O-Lines at Getting to the Second Level",
    subtitle = "Limiting LB influence on all Singleback/Shotgun run plays in the first half of 2022"
  ) %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.weight = "bolder") %>% 
  cols_align(
    align = "center") %>% 
  data_color(team_score,
             fn = scales::col_numeric(palette = c("lightgreen", "lightyellow1"),
                                      domain = c(5.866, 6.2))) %>%  
  data_color(avg_yards,
             fn = scales::col_numeric(palette = c("lightyellow1", "lightgreen"),
                                      domain = c(4.37,
                                                 5.28)))
