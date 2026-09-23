# ============================================================
# ADD NEW 2026 PBL TRACKMAN GAMES
# Only processes new CSVs and updates 2026 RDS files
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(rsconnect)

# -----------------------------
# 1. FILE PATHS
# -----------------------------

base_folder <- "C:/Users/hanna/OneDrive/Documents/Baseball Analytics PBL"

season_year <- "2026"

raw_folder <- file.path(base_folder, "data/raw", season_year)
output_folder <- file.path(base_folder, "data/processed", season_year)

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

master_path <- file.path(output_folder, "master_pitch_level.rds")

# -----------------------------
# 2. HELPERS
# -----------------------------

safe_rate <- function(num, den) {
  ifelse(den == 0 | is.na(den), NA, num / den)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

# -----------------------------
# 3. FIND RAW 2026 FILES
# -----------------------------

raw_files <- list.files(
  raw_folder,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

raw_files <- raw_files[
  !str_detect(tolower(raw_files), "unverified") &
    !str_detect(tolower(raw_files), "playerpositioning")
]

if (length(raw_files) == 0) {
  stop("No valid 2026 CSV files found.")
}

# -----------------------------
# 4. LOAD EXISTING MASTER
# -----------------------------

if (file.exists(master_path)) {
  old_master <- readRDS(master_path) %>% as_tibble()
  already_processed <- unique(old_master$source_file)
} else {
  old_master <- tibble()
  already_processed <- character()
}

new_raw_files <- raw_files[
  !basename(raw_files) %in% already_processed
]

if (length(new_raw_files) == 0) {
  cat("No new 2026 CSV files to process.\n")
  quit(save = "no")
}

cat("New 2026 CSV files found:", length(new_raw_files), "\n")
print(new_raw_files)



# -----------------------------
# 5. READ ONLY NEW CSV FILES
# -----------------------------

df <- new_raw_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>%
            select(-any_of(c(
              "HomeTeamForeignID",
              "AwayTeamForeignID"
            ))) %>%
            mutate(source_file = basename(.x))) %>%
  clean_names()

# -----------------------------
# 6. CLEAN + CREATE CORE FLAGS
# -----------------------------

new_clean <- df %>%
  mutate(
    date = as.Date(date),
    year = as.character(season_year),
    
    pitch_type = case_when(
      auto_pitch_type == "Four-Seam" ~ "FB",
      auto_pitch_type == "Sinker" ~ "SI",
      auto_pitch_type == "Slider" ~ "SL",
      auto_pitch_type == "Curveball" ~ "CB",
      auto_pitch_type == "Changeup" ~ "CH",
      auto_pitch_type == "Cutter" ~ "CT",
      auto_pitch_type == "Splitter" ~ "SP",
      auto_pitch_type == "Other" ~ "OTHER",
      is.na(auto_pitch_type) ~ "UNKNOWN",
      TRUE ~ str_to_upper(auto_pitch_type)
    ),
    
    pitcher_name = pitcher,
    batter_name = batter,
    pitcher_team_name = pitcher_team,
    batter_team_name = batter_team,
    pitcher_throws_clean = pitcher_throws,
    batter_side_clean = batter_side,
    
    count_bucket = case_when(
      balls == 0 & strikes == 0 ~ "First Pitch",
      balls > strikes ~ "Batter Ahead",
      strikes > balls ~ "Pitcher Ahead",
      TRUE ~ "Even"
    ),
    
    two_strike_count = if_else(strikes == 2, 1, 0),
    first_pitch = if_else(pitchof_pa == 1, 1, 0),
    
    in_zone = if_else(
      abs(plate_loc_side) <= 0.83 &
        plate_loc_height >= 1.5 &
        plate_loc_height <= 3.5,
      1, 0, missing = 0
    ),
    
    zone_label = if_else(in_zone == 1, "In Zone", "Out"),
    
    zone_bucket = case_when(
      abs(plate_loc_side) <= 0.56 &
        plate_loc_height >= 1.9 &
        plate_loc_height <= 3.1 ~ "Heart",
      abs(plate_loc_side) <= 0.83 &
        plate_loc_height >= 1.5 &
        plate_loc_height <= 3.5 ~ "Shadow",
      abs(plate_loc_side) <= 1.5 &
        plate_loc_height >= 1.0 &
        plate_loc_height <= 4.0 ~ "Chase",
      TRUE ~ "Waste"
    ),
    
    swing = if_else(
      pitch_call %in% c("FoulBall", "StrikeSwinging", "SwingMiss", "InPlay"),
      1, 0, missing = 0
    ),
    
    whiff = if_else(
      pitch_call %in% c("StrikeSwinging", "SwingMiss"),
      1, 0, missing = 0
    ),
    
    called_strike = if_else(
      pitch_call %in% c("CalledStrike", "StrikeCalled"),
      1, 0, missing = 0
    ),
    
    foul = if_else(pitch_call == "FoulBall", 1, 0, missing = 0),
    bip = if_else(pitch_call == "InPlay", 1, 0, missing = 0),
    
    contact = if_else(
      pitch_call %in% c("FoulBall", "InPlay"),
      1, 0, missing = 0
    ),
    
    strike = if_else(
      pitch_call %in% c(
        "CalledStrike", "StrikeCalled",
        "StrikeSwinging", "SwingMiss",
        "FoulBall", "InPlay"
      ),
      1, 0, missing = 0
    ),
    
    ball = if_else(
      pitch_call %in% c("BallCalled", "Ball"),
      1, 0, missing = 0
    ),
    
    csw = if_else(called_strike == 1 | whiff == 1, 1, 0),
    
    chase_swing = if_else(swing == 1 & in_zone == 0, 1, 0),
    zone_swing = if_else(swing == 1 & in_zone == 1, 1, 0),
    chase_contact = if_else(contact == 1 & in_zone == 0, 1, 0),
    zone_contact = if_else(contact == 1 & in_zone == 1, 1, 0),
    
    has_batted_ball = if_else(!is.na(exit_speed) & bip == 1, 1, 0),
    
    hard_hit = if_else(exit_speed >= 95, 1, 0, missing = 0),
    
    damage = if_else(
      exit_speed >= 95 & angle >= 10 & angle <= 30,
      1, 0, missing = 0
    ),
    
    ground_ball = if_else(angle < 10 & has_batted_ball == 1, 1, 0, missing = 0),
    line_drive = if_else(angle >= 10 & angle <= 25 & has_batted_ball == 1, 1, 0, missing = 0),
    fly_ball = if_else(angle > 25 & angle < 50 & has_batted_ball == 1, 1, 0, missing = 0),
    pop_up = if_else(angle >= 50 & has_batted_ball == 1, 1, 0, missing = 0),
    
    pull_side = case_when(
      batter_side_clean == "Right" & direction < -10 ~ 1,
      batter_side_clean == "Left" & direction > 10 ~ 1,
      TRUE ~ 0
    ),
    
    oppo_side = case_when(
      batter_side_clean == "Right" & direction > 10 ~ 1,
      batter_side_clean == "Left" & direction < -10 ~ 1,
      TRUE ~ 0
    ),
    
    middle_field = if_else(
      direction >= -10 & direction <= 10,
      1, 0, missing = 0
    ),
    
    hit = if_else(
      play_result %in% c("Single", "Double", "Triple", "HomeRun"),
      1, 0, missing = 0
    ),
    
    single = if_else(play_result == "Single", 1, 0, missing = 0),
    double = if_else(play_result == "Double", 1, 0, missing = 0),
    triple = if_else(play_result == "Triple", 1, 0, missing = 0),
    homerun = if_else(play_result == "HomeRun", 1, 0, missing = 0),
    
    walk = if_else(kor_bb == "Walk", 1, 0, missing = 0),
    strikeout = if_else(kor_bb == "Strikeout", 1, 0, missing = 0),
    
    at_bat_event = if_else(
      play_result %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Error") |
        kor_bb == "Strikeout",
      1, 0, missing = 0
    ),
    
    pa_event = if_else(
      at_bat_event == 1 | kor_bb %in% c("Walk", "HitByPitch"),
      1, 0, missing = 0
    ),
    
    total_bases = case_when(
      play_result == "Single" ~ 1,
      play_result == "Double" ~ 2,
      play_result == "Triple" ~ 3,
      play_result == "HomeRun" ~ 4,
      TRUE ~ 0
    )
  )

# -----------------------------
# 7. APPEND NEW DATA TO MASTER
# -----------------------------

if (nrow(old_master) > 0) {
  master_updated <- bind_rows(old_master, new_clean) %>%
    distinct(source_file, pitch_no, game_id, .keep_all = TRUE)
} else {
  master_updated <- new_clean
}

cat("Old master rows:", nrow(old_master), "\n")
cat("New rows added:", nrow(new_clean), "\n")
cat("Updated master rows:", nrow(master_updated), "\n")

# Use updated master for summaries
df_clean <- master_updated

# -----------------------------
# 8. REBUILD SUMMARY FILES FROM UPDATED MASTER
# -----------------------------

pitcher_summary <- df_clean %>%
  group_by(pitcher_name, pitcher_team_name) %>%
  summarise(
    pitches = n(),
    strike_pct = mean(strike, na.rm = TRUE),
    zone_pct = mean(in_zone, na.rm = TRUE),
    heart_pct = mean(zone_bucket == "Heart", na.rm = TRUE),
    shadow_pct = mean(zone_bucket == "Shadow", na.rm = TRUE),
    chase_zone_pct = mean(zone_bucket == "Chase", na.rm = TRUE),
    waste_pct = mean(zone_bucket == "Waste", na.rm = TRUE),
    first_pitch_strike_pct = safe_rate(sum(strike == 1 & first_pitch == 1, na.rm = TRUE), sum(first_pitch == 1, na.rm = TRUE)),
    swing_pct = mean(swing, na.rm = TRUE),
    whiff_pct = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    csw_pct = mean(csw, na.rm = TRUE),
    chase_pct = safe_rate(sum(chase_swing, na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
    zone_swing_pct = safe_rate(sum(zone_swing, na.rm = TRUE), sum(in_zone == 1, na.rm = TRUE)),
    zone_contact_pct = safe_rate(sum(zone_contact, na.rm = TRUE), sum(zone_swing, na.rm = TRUE)),
    chase_contact_pct = safe_rate(sum(chase_contact, na.rm = TRUE), sum(chase_swing, na.rm = TRUE)),
    contact_pct = safe_rate(sum(contact, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    bip_pct = mean(bip, na.rm = TRUE),
    foul_pct = mean(foul, na.rm = TRUE),
    avg_velo = mean(rel_speed, na.rm = TRUE),
    max_velo = safe_max(rel_speed),
    avg_spin = mean(spin_rate, na.rm = TRUE),
    avg_ivb = mean(induced_vert_break, na.rm = TRUE),
    avg_hb = mean(horz_break, na.rm = TRUE),
    avg_vaa = mean(vert_appr_angle, na.rm = TRUE),
    avg_haa = mean(horz_appr_angle, na.rm = TRUE),
    avg_extension = mean(extension, na.rm = TRUE),
    avg_rel_height = mean(rel_height, na.rm = TRUE),
    avg_rel_side = mean(rel_side, na.rm = TRUE),
    avg_ev_allowed = mean(exit_speed[has_batted_ball == 1], na.rm = TRUE),
    peak_ev_allowed = safe_max(exit_speed[has_batted_ball == 1]),
    avg_la_allowed = mean(angle[has_batted_ball == 1], na.rm = TRUE),
    hard_hit_pct_allowed = safe_rate(sum(hard_hit, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    damage_pct_allowed = safe_rate(sum(damage, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    gb_pct_allowed = safe_rate(sum(ground_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    ld_pct_allowed = safe_rate(sum(line_drive, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    fb_pct_allowed = safe_rate(sum(fly_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    pu_pct_allowed = safe_rate(sum(pop_up, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    k_pct = safe_rate(sum(strikeout, na.rm = TRUE), sum(pa_event, na.rm = TRUE)),
    bb_pct = safe_rate(sum(walk, na.rm = TRUE), sum(pa_event, na.rm = TRUE)),
    k_minus_bb_pct = k_pct - bb_pct,
    .groups = "drop"
  )

pitcher_pitch_type_summary <- df_clean %>%
  group_by(pitcher_name, pitcher_team_name, pitch_type) %>%
  summarise(
    pitches = n(),
    avg_velo = mean(rel_speed, na.rm = TRUE),
    max_velo = safe_max(rel_speed),
    avg_spin = mean(spin_rate, na.rm = TRUE),
    avg_ivb = mean(induced_vert_break, na.rm = TRUE),
    avg_hb = mean(horz_break, na.rm = TRUE),
    avg_vaa = mean(vert_appr_angle, na.rm = TRUE),
    avg_extension = mean(extension, na.rm = TRUE),
    strike_pct = mean(strike, na.rm = TRUE),
    zone_pct = mean(in_zone, na.rm = TRUE),
    swing_pct = mean(swing, na.rm = TRUE),
    whiff_pct = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    csw_pct = mean(csw, na.rm = TRUE),
    chase_pct = safe_rate(sum(chase_swing, na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
    contact_pct = safe_rate(sum(contact, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    bip_pct = mean(bip, na.rm = TRUE),
    avg_ev_allowed = mean(exit_speed[has_batted_ball == 1], na.rm = TRUE),
    hard_hit_pct_allowed = safe_rate(sum(hard_hit, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    damage_pct_allowed = safe_rate(sum(damage, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    gb_pct_allowed = safe_rate(sum(ground_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    ld_pct_allowed = safe_rate(sum(line_drive, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    fb_pct_allowed = safe_rate(sum(fly_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    pu_pct_allowed = safe_rate(sum(pop_up, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  group_by(pitcher_name) %>%
  mutate(usage_pct = pitches / sum(pitches)) %>%
  ungroup()

hitter_summary <- df_clean %>%
  group_by(batter_name, batter_team_name) %>%
  summarise(
    pitches_seen = n(),
    plate_appearances = sum(pa_event, na.rm = TRUE),
    at_bats = sum(at_bat_event, na.rm = TRUE),
    hits = sum(hit, na.rm = TRUE),
    singles = sum(single, na.rm = TRUE),
    doubles = sum(double, na.rm = TRUE),
    triples = sum(triple, na.rm = TRUE),
    homeruns = sum(homerun, na.rm = TRUE),
    total_bases_sum = sum(total_bases, na.rm = TRUE),
    walks = sum(walk, na.rm = TRUE),
    strikeouts = sum(strikeout, na.rm = TRUE),
    avg = safe_rate(hits, at_bats),
    obp = safe_rate(hits + walks, plate_appearances),
    slg = safe_rate(total_bases_sum, at_bats),
    ops = obp + slg,
    k_pct = safe_rate(strikeouts, plate_appearances),
    bb_pct = safe_rate(walks, plate_appearances),
    swing_pct = mean(swing, na.rm = TRUE),
    whiff_pct = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    chase_pct = safe_rate(sum(chase_swing, na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
    zone_swing_pct = safe_rate(sum(zone_swing, na.rm = TRUE), sum(in_zone == 1, na.rm = TRUE)),
    zone_contact_pct = safe_rate(sum(zone_contact, na.rm = TRUE), sum(zone_swing, na.rm = TRUE)),
    chase_contact_pct = safe_rate(sum(chase_contact, na.rm = TRUE), sum(chase_swing, na.rm = TRUE)),
    contact_pct = safe_rate(sum(contact, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    avg_ev = mean(exit_speed[has_batted_ball == 1], na.rm = TRUE),
    peak_ev = safe_max(exit_speed[has_batted_ball == 1]),
    avg_la = mean(angle[has_batted_ball == 1], na.rm = TRUE),
    avg_distance = mean(distance[has_batted_ball == 1], na.rm = TRUE),
    hard_hit_pct = safe_rate(sum(hard_hit, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    damage_pct = safe_rate(sum(damage, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    gb_pct = safe_rate(sum(ground_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    ld_pct = safe_rate(sum(line_drive, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    fb_pct = safe_rate(sum(fly_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    pu_pct = safe_rate(sum(pop_up, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    pull_pct = safe_rate(sum(pull_side, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    middle_pct = safe_rate(sum(middle_field, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    oppo_pct = safe_rate(sum(oppo_side, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    pull_ev = mean(exit_speed[pull_side == 1], na.rm = TRUE),
    middle_ev = mean(exit_speed[middle_field == 1], na.rm = TRUE),
    oppo_ev = mean(exit_speed[oppo_side == 1], na.rm = TRUE),
    pull_la = mean(angle[pull_side == 1], na.rm = TRUE),
    middle_la = mean(angle[middle_field == 1], na.rm = TRUE),
    oppo_la = mean(angle[oppo_side == 1], na.rm = TRUE),
    contact_height = mean(contact_position_z, na.rm = TRUE),
    contact_side = mean(contact_position_x, na.rm = TRUE),
    contact_depth = mean(contact_position_y, na.rm = TRUE),
    .groups = "drop"
  )

hitter_pitch_type_summary <- df_clean %>%
  group_by(batter_name, batter_team_name, pitch_type) %>%
  summarise(
    pitches_seen = n(),
    swing_pct = mean(swing, na.rm = TRUE),
    whiff_pct = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    chase_pct = safe_rate(sum(chase_swing, na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
    contact_pct = safe_rate(sum(contact, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    avg_ev = mean(exit_speed[has_batted_ball == 1], na.rm = TRUE),
    peak_ev = safe_max(exit_speed[has_batted_ball == 1]),
    avg_la = mean(angle[has_batted_ball == 1], na.rm = TRUE),
    hard_hit_pct = safe_rate(sum(hard_hit, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    damage_pct = safe_rate(sum(damage, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    gb_pct = safe_rate(sum(ground_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    ld_pct = safe_rate(sum(line_drive, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    fb_pct = safe_rate(sum(fly_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    pu_pct = safe_rate(sum(pop_up, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    .groups = "drop"
  )

zone_summary <- df_clean %>%
  group_by(pitcher_name, pitcher_team_name, pitch_type, zone_bucket) %>%
  summarise(
    pitches = n(),
    swing_pct = mean(swing, na.rm = TRUE),
    whiff_pct = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    contact_pct = safe_rate(sum(contact, na.rm = TRUE), sum(swing, na.rm = TRUE)),
    bip_pct = mean(bip, na.rm = TRUE),
    avg_ev_allowed = mean(exit_speed[has_batted_ball == 1], na.rm = TRUE),
    damage_pct_allowed = safe_rate(sum(damage, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)),
    .groups = "drop"
  )

spray_chart_data <- df_clean %>%
  filter(has_batted_ball == 1) %>%
  select(
    date, game_id, pitch_no, batter_name, batter_team_name,
    pitcher_name, pitcher_team_name, pitch_type, batter_side_clean,
    exit_speed, angle, direction, distance, play_result,
    tagged_hit_type, auto_hit_type, pull_side, middle_field, oppo_side,
    contact_position_x, contact_position_y, contact_position_z,
    position_at110x, position_at110y, position_at110z
  )

pitch_location_data <- df_clean %>%
  select(
    date, game_id, pitch_no, pitcher_name, pitcher_team_name,
    batter_name, batter_team_name, pitch_type, batter_side_clean,
    balls, strikes, count_bucket, plate_loc_side, plate_loc_height,
    zone_label, zone_bucket, swing, whiff, contact, bip,
    hard_hit, damage, exit_speed, angle, play_result
  )

# -----------------------------
# 9. SAVE UPDATED 2026 RDS FILES
# -----------------------------

saveRDS(df_clean, file.path(output_folder, "master_pitch_level.rds"))
saveRDS(pitcher_summary, file.path(output_folder, "pitcher_summary.rds"))
saveRDS(pitcher_pitch_type_summary, file.path(output_folder, "pitcher_pitch_type_summary.rds"))
saveRDS(hitter_summary, file.path(output_folder, "hitter_summary.rds"))
saveRDS(hitter_pitch_type_summary, file.path(output_folder, "hitter_pitch_type_summary.rds"))
saveRDS(zone_summary, file.path(output_folder, "zone_summary.rds"))
saveRDS(spray_chart_data, file.path(output_folder, "spray_chart_data.rds"))
saveRDS(pitch_location_data, file.path(output_folder, "pitch_location_data.rds"))

cat("2026 update complete.\n")
cat("Files saved to:", output_folder, "\n")
