library(readr)

PBP <- read_csv("MLB_2025.csv")
P_stats <- read_csv("P_stats_2025.csv")
View(PBP)
View(P_stats)

library(dplyr)

# id tag in P_stats
P_stats <- P_stats %>%
  rename(pitcher = player_id)

# get rid of batter name in PBP
PBP <- PBP %>%
  select(-player_name)

# add pitcher name to PBP 
pitcher_name <- P_stats %>%
  transmute(
    pitcher = as.integer(pitcher),          
    pitcher_name = player_full_name
  ) %>%
  distinct()
PBP <- PBP %>%
  mutate(pitcher = as.integer(pitcher)) %>%
  left_join(pitcher_name, by = "pitcher")

# Edit to pitchers with 30+ IP
eligible_pitchers <- P_stats %>%
  filter(innings_pitched >= 30) %>%
  distinct(pitcher, player_full_name)  

P_stats <- P_stats %>%
  semi_join(eligible_pitchers, by = "pitcher")

PBP <- PBP %>%
  semi_join(eligible_pitchers, by = "pitcher")

write_csv(P_stats, "P_stats_30IP")
write_csv(PBP, "PBP_30IP")


