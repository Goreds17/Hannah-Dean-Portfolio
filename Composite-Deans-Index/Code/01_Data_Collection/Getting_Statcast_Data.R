library(remotes)

# Install baseballr if needed:
remotes::install_github("BillPetti/baseballr")

library(tidyverse)

# ============================================================
# 2026 STATCAST DATA
# March 25, 2026 through July 23, 2026
# ============================================================

get_statcast_weekly <- function(start_date, end_date, step_days = 7) {
  
  starts <- seq.Date(
    from = as.Date(start_date),
    to   = as.Date(end_date),
    by   = paste(step_days, "days")
  )
  
  map_dfr(starts, function(s) {
    
    e <- min(
      s + (step_days - 1),
      as.Date(end_date)
    )
    
    message("Pulling: ", s, " to ", e)
    
    x <- baseballr::statcast_search(
      start_date = as.character(s),
      end_date   = as.character(e)
    )
    
    # Tag each weekly pull for debugging and documentation
    x %>%
      mutate(
        pull_start = s,
        pull_end   = e
      )
  })
}


# ---- Pull 2026 Statcast Data ----

data_2026 <- get_statcast_weekly(
  start_date = "2026-03-25",
  end_date   = "2026-07-23",
  step_days  = 7
)


# ============================================================
# CREATE MOVEMENT AND CONTACT VARIABLES
# ============================================================

data_2026 <- data_2026 %>%
  mutate(
    
    # Convert movement from feet to inches
    IVB = pfx_z * 12,
    HB  = pfx_x * 12,
    
    # Whiff indicator
    whiff = if_else(
      description %in% c(
        "swinging_strike",
        "swinging_strike_blocked"
      ),
      1L,
      0L
    ),
    
    # Hard-contact indicator
    hard_contact = case_when(
      is.na(launch_speed) ~ NA_integer_,
      launch_speed >= 95  ~ 1L,
      TRUE                ~ 0L
    )
  )


# ---- Calculate Gravity-Adjusted Vertical Break ----

data_2026 <- data_2026 %>%
  mutate(
    
    gravity_drop = case_when(
      !is.na(release_pos_y) &
        !is.na(vy0) &
        vy0 != 0 ~
        (0.5 * 32.174 * (release_pos_y / abs(vy0))^2) * 12,
      
      TRUE ~ NA_real_
    ),
    
    VB = IVB - gravity_drop
  )


# ============================================================
# 2026 MLB PITCHER SEASON STATISTICS
# ============================================================

pitcher_stats_2026 <- baseballr::mlb_stats(
  stat_type   = "season",
  stat_group  = "pitching",
  season      = 2026,
  sport_ids   = 1,       # MLB
  player_pool = "All",
  position    = "P",
  limit       = 5000
)


# Keep pitchers only
pitcher_stats_2026 <- pitcher_stats_2026 %>%
  filter(position_abbreviation == "P")


# ============================================================
# SAVE 2026 DATA
# ============================================================

saveRDS(
  data_2026,
  "MLB_2026.RDS"
)

write_csv(
  data_2026,
  "MLB_2026.csv"
)

saveRDS(
  pitcher_stats_2026,
  "P_stats_2026.RDS"
)

write_csv(
  pitcher_stats_2026,
  "P_stats_2026.csv"
)


# ============================================================
# OPTIONAL CHECKS
# ============================================================

cat("\nStatcast rows:", nrow(data_2026), "\n")
cat("Pitcher-stat rows:", nrow(pitcher_stats_2026), "\n")

cat(
  "Statcast date range:",
  as.character(min(data_2026$game_date, na.rm = TRUE)),
  "through",
  as.character(max(data_2026$game_date, na.rm = TRUE)),
  "\n"
)

# Install devtools if you haven't already
install.packages("devtools")

# Install the latest version of baseballr from GitHub
devtools::install_github("BillPetti/baseballr")

# Load the package
library(baseballr)

