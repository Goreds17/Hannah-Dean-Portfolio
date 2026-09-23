# ============================================================
# PBL ANALYTICS PORTAL - SHINY APP
# ============================================================

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(bslib)
library(scales)
library(zoo)
library(data.table)   

# -----------------------------
# LOAD DATA
# -----------------------------

processed_folder <- "data/processed"

years_to_load <- c("2025", "2026")

load_year_file <- function(year, file_stub) {
  rds_path <- file.path(processed_folder, year, paste0(file_stub, ".rds"))
  
  if (!file.exists(rds_path)) {
    warning(paste("Missing file:", rds_path))
    return(tibble())
  }
  
  readRDS(rds_path) %>%
    as_tibble() %>%
    mutate(
      year = as.character(year),
      across(any_of(c("date", "utc_date")), as.Date),
      across(any_of(c("time", "utc_time", "local_date_time", "utc_date_time", "tilt")), as.character)
    )
}

master_2025 <- load_year_file("2025", "master_pitch_level")
master_2026 <- load_year_file("2026", "master_pitch_level")
pitcher_summary <- map_dfr(years_to_load, load_year_file, file_stub = "pitcher_summary")
pitcher_pitch_type <- map_dfr(years_to_load, load_year_file, file_stub = "pitcher_pitch_type_summary")
pitch_location <- map_dfr(years_to_load, load_year_file, file_stub = "pitch_location_data")

available_years <- sort(unique(c(master_2025$year, master_2026$year)), decreasing = TRUE)

# -----------------------------
# HELPERS
# -----------------------------

safe_rate <- function(num, den) {
  ifelse(den == 0 | is.na(den), NA, num / den)
}

pct <- function(x) round(x * 100, 1)

notes_path <- "data/development_notes.csv"

if (!dir.exists("data")) {
  dir.create("data")
}

if (!file.exists(notes_path)) {
  write_csv(tibble(
    note_id        = character(),
    date           = as.Date(character()),
    player_type    = character(),
    team           = character(),
    player         = character(),
    focus_area     = character(),
    priority       = character(),
    status         = character(),
    observation    = character(),
    adjustment_plan = character(),
    follow_up      = character()
  ), notes_path)
}

# -----------------------------
# COLORS
# -----------------------------

team_teal      <- "#00A3A3"
dark_teal      <- "#006D6D"
baseball_red   <- "#D62828"
dark_gray      <- "#2E2E2E"
light_gray     <- "#F4F6F8"

pitch_colors <- c(
  "FB"      = "#D62828",
  "SI"      = "#00A3A3",
  "SL"      = "#4E9F3D",
  "CB"      = "#F28E2B",
  "CH"      = "#4E79A7",
  "CT"      = "#EDC948",
  "SP"      = "#9C755F",
  "OTHER"   = "#BAB0AC",
  "UNKNOWN" = "#6C757D"
)

pitch_family_colors <- c(
  "Fastball" = "#D62828",
  "Breaking" = "#00A3A3",
  "Offspeed" = "#4E79A7",
  "Other"    = "#6C757D"
)

get_pitch_family <- function(pitch_type) {
  case_when(
    pitch_type %in% c("FB", "FF", "SI", "FT", "FC", "CT") ~ "Fastball",
    pitch_type %in% c("SL", "CB", "CU", "KC", "SV")       ~ "Breaking",
    pitch_type %in% c("CH", "SP", "FS")                    ~ "Offspeed",
    TRUE                                                    ~ "Other"
  )
}

# -----------------------------
# VISUAL HELPERS
# -----------------------------

add_strike_zone <- function() {
  list(
    geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
              fill = NA, color = "black", linewidth = 1.2),
    geom_vline(xintercept = c(-0.83/3, 0.83/3), color = "gray75", linewidth = 0.4),
    geom_hline(yintercept = c(1.5 + 2/3, 1.5 + 4/3), color = "gray75", linewidth = 0.4)
  )
}

add_home_plate <- function() {
  list(
    geom_segment(aes(x = -0.7, xend = 0,   y = 0.45, yend = 0.75), color = "black", inherit.aes = FALSE),
    geom_segment(aes(x = 0,    xend = 0.7,  y = 0.75, yend = 0.45), color = "black", inherit.aes = FALSE),
    geom_segment(aes(x = 0.7,  xend = 0.7,  y = 0.45, yend = 0.1),  color = "black", inherit.aes = FALSE),
    geom_segment(aes(x = 0.7,  xend = -0.7, y = 0.1,  yend = 0.1),  color = "black", inherit.aes = FALSE),
    geom_segment(aes(x = -0.7, xend = -0.7, y = 0.1,  yend = 0.45), color = "black", inherit.aes = FALSE)
  )
}

ev_scale <- scale_color_gradientn(
  colors = c("#1E00FF", "#B8B8B8", "#D10000"),
  values = scales::rescale(c(60, 85, 120)),
  limits = c(60, 120),
  oob    = scales::squish,
  breaks = c(60, 85, 120),
  labels = c("< 60", "85", "> 120"),
  name   = "EV"
)

ev_fill_scale <- scale_fill_gradientn(
  colors = c("#1E00FF", "#B8B8B8", "#D10000"),
  values = scales::rescale(c(60, 85, 120)),
  limits = c(60, 120),
  oob    = scales::squish,
  breaks = c(60, 85, 120),
  labels = c("< 60", "85", "> 120"),
  name   = "EV"
)

add_plate_front <- function() {
  list(
    geom_polygon(
      data = tibble(x = c(-0.708, 0.708, 0.708, 0, -0.708),
                    y = c(0.35, 0.35, 0.15, -0.10, 0.15)),
      aes(x = x, y = y),
      fill = "white", color = "black", linewidth = 1, inherit.aes = FALSE
    )
  )
}

add_batters_boxes_front <- function() {
  list(
    geom_rect(xmin = -2.05, xmax = -1.15, ymin = -1.95, ymax = 2.00,
              fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE),
    geom_rect(xmin =  1.15, xmax =  2.05, ymin = -1.95, ymax = 2.00,
              fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE)
  )
}

add_top_plate_reference <- function() {
  list(
    geom_polygon(
      data = tibble(
        x = c(-0.7083,  0.7083,  0.7083,  0,      -0.7083),
        y = c( 1.4167,  1.4167,  0.7083,  0,       0.7083)
      ),
      aes(x = x, y = y),
      fill = NA, color = "black", linewidth = 1.2, inherit.aes = FALSE
    ),
    geom_vline(xintercept = 0, color = "gray60", linetype = "dashed", linewidth = 0.7)
  )
}

add_side_plate_reference <- function() {
  list(
    geom_polygon(
      data = tibble(x = c(0.00, 0.95, 1.40, 1.40, 0.70),
                    y = c(0.25, 0.40, 0.40, 0.15, 0.05)),
      aes(x = x, y = y),
      fill = "white", color = "black", linewidth = 1, inherit.aes = FALSE
    ),
    geom_rect(xmin = 0.00, xmax = 1.40, ymin = 1.50, ymax = 3.50,
              fill = NA, color = "black", linewidth = 1.1, inherit.aes = FALSE),
    geom_segment(aes(x = 0.00, xend = 0.75, y = 1.50, yend = 1.75), color = "black", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = 0.00, xend = 0.75, y = 3.50, yend = 3.75), color = "black", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = 1.40, xend = 0.75, y = 1.50, yend = 1.75), color = "black", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = 1.40, xend = 0.75, y = 3.50, yend = 3.75), color = "black", linetype = "dashed", inherit.aes = FALSE),
    geom_segment(aes(x = 0.75, xend = 0.75, y = 1.75, yend = 3.75), color = "black", linewidth = 1, inherit.aes = FALSE)
  )
}

make_contact_type <- function(d) {
  d %>%
    mutate(contact_type = case_when(
      ground_ball == 1 ~ "GB",
      line_drive  == 1 ~ "LD",
      fly_ball    == 1 ~ "FB",
      pop_up      == 1 ~ "PU",
      TRUE             ~ "Other"
    ))
}

make_zone25 <- function(d, metric = "pitch_count") {
  x_breaks <- c(-1.83, -0.83, -0.2767, 0.2767, 0.83, 1.83)
  y_breaks <- c(0.5, 1.5, 2.1667, 2.8333, 3.5, 4.5)
  
  d %>%
    mutate(
      zone_x = cut(plate_loc_side,   breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
      zone_y = cut(plate_loc_height, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
    ) %>%
    filter(!is.na(zone_x), !is.na(zone_y)) %>%
    group_by(zone_x, zone_y) %>%
    summarise(
      x_mid      = mean(c(x_breaks[zone_x[1]], x_breaks[zone_x[1] + 1])),
      y_mid      = mean(c(y_breaks[zone_y[1]], y_breaks[zone_y[1] + 1])),
      width      = x_breaks[zone_x[1] + 1] - x_breaks[zone_x[1]],
      height     = y_breaks[zone_y[1] + 1] - y_breaks[zone_y[1]],
      pitch_count = n(),
      swing      = mean(swing,     na.rm = TRUE) * 100,
      whiff      = mean(whiff,     na.rm = TRUE) * 100,
      bip        = mean(bip,       na.rm = TRUE) * 100,
      hard_hit   = mean(hard_hit,  na.rm = TRUE) * 100,
      damage     = mean(damage,    na.rm = TRUE) * 100,
      exit_speed = mean(exit_speed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(value = .data[[metric]])
}

plot_zone25_heatmap <- function(d, metric) {
  heat_df <- make_zone25(d, metric)
  
  ggplot(heat_df, aes(x = x_mid, y = y_mid, fill = value)) +
    geom_tile(aes(width = width, height = height), color = "#91C9FF") +
    geom_text(aes(label = round(value, 1)), color = "white", fontface = "bold", size = 4, na.rm = TRUE) +
    add_strike_zone() +
    scale_fill_gradient(low = "#2525D9", high = "#B30000", na.value = "white") +
    coord_fixed(xlim = c(-1.83, 1.83), ylim = c(0.5, 4.5)) +
    theme_minimal() +
    labs(x = "Plate Loc Side", y = "Plate Loc Height", fill = metric)
}

# -----------------------------
# SPRAY CHART HELPERS
# -----------------------------

prepare_spray_data <- function(d) {
  d %>%
    mutate(
      spray_angle    = case_when(
        "bearing" %in% names(.) & !is.na(bearing) ~ bearing,
        !is.na(direction)                          ~ direction,
        TRUE                                       ~ NA_real_
      ),
      spray_distance = case_when(!is.na(distance) ~ distance, TRUE ~ NA_real_),
      spray_x        = spray_distance * sin(spray_angle * pi / 180),
      spray_y        = spray_distance * cos(spray_angle * pi / 180)
    )
}

make_field_zone <- function(start_angle, end_angle, r_inner, r_outer, n = 80) {
  outer_angles <- seq(start_angle, end_angle, length.out = n) * pi / 180
  inner_angles <- seq(end_angle, start_angle, length.out = n) * pi / 180
  bind_rows(
    tibble(x = r_outer * sin(outer_angles), y = r_outer * cos(outer_angles)),
    tibble(x = r_inner * sin(inner_angles), y = r_inner * cos(inner_angles))
  )
}

spray_sections <- bind_rows(
  make_field_zone(-45, -15, 0, 170)   %>% mutate(section = "IF Pull"),
  make_field_zone(-15,  15, 0, 170)   %>% mutate(section = "IF Middle"),
  make_field_zone( 15,  45, 0, 170)   %>% mutate(section = "IF Oppo"),
  make_field_zone(-45, -15, 170, 430) %>% mutate(section = "OF Pull"),
  make_field_zone(-15,  15, 170, 430) %>% mutate(section = "OF Middle"),
  make_field_zone( 15,  45, 170, 430) %>% mutate(section = "OF Oppo")
)

spray_fill_colors <- c(
  "IF Pull"   = "#F8D9A6",
  "IF Middle" = "#F8D9A6",
  "IF Oppo"   = "#F8D9A6",
  "OF Pull"   = "#BFE8B8",
  "OF Middle" = "#BFE8B8",
  "OF Oppo"   = "#BFE8B8"
)

add_spray_field <- function(d) {
  d <- prepare_spray_data(d)
  
  section_pct <- d %>%
    filter(!is.na(spray_angle), !is.na(spray_distance)) %>%
    mutate(
      side_section  = cut(spray_angle, breaks = c(-45, -15, 15, 45),
                          labels = c("Pull", "Middle", "Oppo"), include.lowest = TRUE),
      depth_section = if_else(spray_distance <= 170, "IF", "OF"),
      spray_section = paste(depth_section, side_section)
    ) %>%
    count(spray_section) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
  
  section_labels <- tibble(
    spray_section = c("IF Pull", "IF Middle", "IF Oppo", "OF Pull", "OF Middle", "OF Oppo"),
    angle         = c(-30, 0, 30, -30, 0, 30),
    radius        = c(105, 105, 105, 300, 300, 300)
  ) %>%
    mutate(x = radius * sin(angle * pi / 180), y = radius * cos(angle * pi / 180)) %>%
    left_join(section_pct, by = "spray_section") %>%
    mutate(label = paste0(ifelse(is.na(pct), 0, pct), "%"))
  
  list(
    geom_polygon(data = spray_sections, aes(x = x, y = y, group = section, fill = section),
                 inherit.aes = FALSE, alpha = 0.45, color = "white", linewidth = 1.2),
    geom_segment(aes(x = 0, y = 0, xend = -430 * sin(45 * pi / 180), yend = 430 * cos(45 * pi / 180)),
                 inherit.aes = FALSE, color = "gray70", linewidth = 0.8),
    geom_segment(aes(x = 0, y = 0, xend =  430 * sin(45 * pi / 180), yend = 430 * cos(45 * pi / 180)),
                 inherit.aes = FALSE, color = "gray70", linewidth = 0.8),
    geom_curve(aes(x = -430 * sin(45 * pi / 180), y = 430 * cos(45 * pi / 180),
                   xend = 430 * sin(45 * pi / 180), yend = 430 * cos(45 * pi / 180)),
               inherit.aes = FALSE, curvature = -0.45, color = "gray70", linewidth = 0.8),
    geom_curve(aes(x = -170 * sin(45 * pi / 180), y = 170 * cos(45 * pi / 180),
                   xend = 170 * sin(45 * pi / 180), yend = 170 * cos(45 * pi / 180)),
               inherit.aes = FALSE, curvature = -0.45, color = "white", linewidth = 1.4),
    geom_point(aes(x = 0, y = 0), inherit.aes = FALSE, shape = 23, size = 4, fill = "white", color = "black"),
    geom_text(data = section_labels, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, color = "gray45", fontface = "bold", size = 4.5),
    scale_fill_manual(values = spray_fill_colors, guide = "none")
  )
}

# -----------------------------
# HEAT MAP HELPERS
# -----------------------------

heatmap_metrics <- tribble(
  ~id,              ~label,
  "pitch_pct",      "Pitch %",
  "pitch_count",    "Pitch #",
  "swing_pct",      "Swing %",
  "swing_count",    "Swing #",
  "whiff_pct",      "Whiff %",
  "whiff_count",    "Whiff #",
  "bb_count",       "Batted Balls #",
  "avg_ev",         "Avg Exit Velo",
  "hard_hit_pct",   "Hard Hit %",
  "hard_hit_count", "Hard Hit #",
  "damage_pct",     "Damage %",
  "damage_count",   "Damage #",
  "gb_pct",         "GB %",
  "ld_pct",         "LD %",
  "fb_pct",         "FB %",
  "pu_pct",         "PU %"
)

heatmap_tab_ui <- function(prefix) {
  tagList(lapply(seq(1, nrow(heatmap_metrics), by = 3), function(i) {
    fluidRow(lapply(i:min(i + 2, nrow(heatmap_metrics)), function(j) {
      column(4, div(class = "card",
                    div(class = "section-title", heatmap_metrics$label[j]),
                    plotlyOutput(paste0(prefix, "_", heatmap_metrics$id[j]), height = "420px")))
    }))
  }))
}

make_report_heatmap <- function(d, metric_key) {
  x_breaks <- c(-1.83, -0.83, -0.2767, 0.2767, 0.83, 1.83)
  y_breaks <- c(0.5, 1.5, 2.1667, 2.8333, 3.5, 4.5)
  
  total_pitches <- nrow(d)
  
  heat_df <- d %>%
    make_contact_type() %>%
    mutate(
      zone_x = cut(plate_loc_side,   breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
      zone_y = cut(plate_loc_height, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
    ) %>%
    filter(!is.na(zone_x), !is.na(zone_y)) %>%
    group_by(zone_x, zone_y) %>%
    summarise(
      x_mid        = mean(c(x_breaks[zone_x[1]], x_breaks[zone_x[1] + 1])),
      y_mid        = mean(c(y_breaks[zone_y[1]], y_breaks[zone_y[1] + 1])),
      width        = x_breaks[zone_x[1] + 1] - x_breaks[zone_x[1]],
      height       = y_breaks[zone_y[1] + 1] - y_breaks[zone_y[1]],
      pitch_count  = n(),
      pitch_pct    = safe_rate(n(), total_pitches) * 100,
      swing_count  = sum(swing,  na.rm = TRUE),
      swing_pct    = mean(swing, na.rm = TRUE) * 100,
      whiff_count  = sum(whiff,  na.rm = TRUE),
      whiff_pct    = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)) * 100,
      bb_count     = sum(has_batted_ball, na.rm = TRUE),
      avg_ev       = mean(exit_speed[has_batted_ball == 1], na.rm = TRUE),
      hard_hit_count = sum(hard_hit, na.rm = TRUE),
      hard_hit_pct   = safe_rate(sum(hard_hit, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)) * 100,
      damage_count = sum(damage, na.rm = TRUE),
      damage_pct   = safe_rate(sum(damage, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)) * 100,
      gb_pct       = safe_rate(sum(ground_ball, na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)) * 100,
      ld_pct       = safe_rate(sum(line_drive,  na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)) * 100,
      fb_pct       = safe_rate(sum(fly_ball,    na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)) * 100,
      pu_pct       = safe_rate(sum(pop_up,      na.rm = TRUE), sum(has_batted_ball, na.rm = TRUE)) * 100,
      .groups = "drop"
    ) %>%
    mutate(value = .data[[metric_key]])
  
  metric_label <- heatmap_metrics$label[heatmap_metrics$id == metric_key]
  
  p <- ggplot(heat_df,
              aes(x = x_mid, y = y_mid, fill = value,
                  text = paste0(metric_label, ": ", round(value, 1),
                                "<br>Pitch #: ", pitch_count,
                                "<br>Pitch %: ", round(pitch_pct, 1),
                                "<br>Batted Balls: ", bb_count,
                                "<br>Avg EV: ", round(avg_ev, 1)))) +
    geom_tile(aes(width = width, height = height), color = "#91C9FF") +
    geom_text(aes(label = ifelse(is.na(value), "", round(value, 1))),
              color = "white", fontface = "bold", size = 4) +
    add_strike_zone() +
    coord_fixed(xlim = c(-1.83, 1.83), ylim = c(0.5, 4.5)) +
    theme_minimal() +
    labs(x = "Plate Loc Side", y = "Plate Loc Height", fill = metric_label)
  
  if (metric_key == "avg_ev") {
    p + ev_fill_scale
  } else {
    p + scale_fill_gradient(low = "#2525D9", high = "#B30000", na.value = "white")
  }
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    body {
      background-color: #F4F6F8;
      color: #2E2E2E;
      font-family: Arial, sans-serif;
    }
    .main-title { font-size: 38px; font-weight: 800; color: #2E2E2E; margin-bottom: 5px; }
    .subtitle   { font-size: 18px; color: #006D6D; margin-bottom: 20px; }
    .card       { background-color: white; border-radius: 14px; padding: 18px; margin-bottom: 18px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
    .section-title { font-size: 26px; font-weight: 700; color: #006D6D; margin-bottom: 12px; }
    .nav-tabs > li > a        { color: #2E2E2E; font-weight: 700; }
    .nav-tabs > li.active > a { background-color: #00A3A3 !important; color: white !important; }
    table.dataTable thead th  { background-color: #003F66 !important; color: white !important; text-align: center !important; }
    .print-button { background-color: #006D6D; color: white; border: none; border-radius: 8px; padding: 10px 16px; font-weight: 700; margin-bottom: 15px; display: inline-block; text-decoration: none; }
    .report-page  { background: white; padding: 22px; border-radius: 14px; margin-bottom: 22px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
    .report-player-title { font-size: 24px; font-weight: 800; color: #006D6D; margin-bottom: 4px; }
    .report-subtitle     { font-size: 15px; color: #2E2E2E; margin-bottom: 12px; }
    .report-card         { background: white; padding: 15px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); }
    .strategy-print-title    { display: none; }
    .strategy-report-col     { margin-bottom: 14px; }
    .strategy-report-card    { width: 100%; }

    @media print {
      body { background: white !important; font-size: 11px !important; }
      body.printing-strategy .main-title,
      body.printing-strategy .subtitle,
      body.printing-strategy .navbar,
      body.printing-strategy .nav,
      body.printing-strategy .tabbable > .nav-tabs,
      body.printing-strategy .no-print,
      body.printing-strategy .print-button,
      body.printing-strategy .dataTables_length,
      body.printing-strategy .dataTables_filter,
      body.printing-strategy .dataTables_info,
      body.printing-strategy .dataTables_paginate { display: none !important; }

      body.printing-strategy .strategy-print-title { display: block !important; font-size: 20px !important; font-weight: 900 !important; color: #006D6D !important; border-bottom: 3px solid #006D6D !important; margin-bottom: 10px !important; padding-bottom: 5px !important; }
      body.printing-strategy .section-title { display: none !important; }
      body.printing-strategy .card { box-shadow: none !important; border: none !important; padding: 0 !important; margin: 0 !important; }
      body.printing-strategy .strategy-report-col { width: 50% !important; float: left !important; padding-left: 5px !important; padding-right: 5px !important; margin-bottom: 10px !important; }
      body.printing-strategy .report-page,
      body.printing-strategy .strategy-report-card { page-break-after: auto !important; break-after: auto !important; page-break-inside: avoid !important; break-inside: avoid !important; border: 1px solid #D9D9D9 !important; border-radius: 8px !important; padding: 7px !important; margin-bottom: 0 !important; box-shadow: none !important; width: 100% !important; }
      body.printing-strategy .report-player-title  { font-size: 14px !important; font-weight: 900 !important; color: #2E2E2E !important; margin-bottom: 2px !important; }
      body.printing-strategy .report-subtitle       { font-size: 8.5px !important; color: #006D6D !important; margin-bottom: 5px !important; font-weight: 700 !important; }
      body.printing-strategy .dataTables_wrapper    { width: 100% !important; }
      body.printing-strategy .dataTables_scrollHead,
      body.printing-strategy .dataTables_scrollBody { overflow: visible !important; }
      body.printing-strategy .dataTables_scrollHeadInner,
      body.printing-strategy .dataTables_scrollHeadInner table { width: 100% !important; }
      body.printing-strategy .dataTables_scrollBody table thead { display: none !important; }
      body.printing-strategy table.dataTable { width: 100% !important; table-layout: fixed !important; border-collapse: collapse !important; font-size: 6.7px !important; }
      body.printing-strategy table.dataTable thead th { background-color: #006D6D !important; color: white !important; font-weight: 800 !important; border: 1px solid #006D6D !important; padding: 2px !important; text-align: center !important; white-space: normal !important; vertical-align: middle !important; }
      body.printing-strategy table.dataTable tbody td { border: 1px solid #D9D9D9 !important; padding: 2px !important; text-align: center !important; white-space: normal !important; vertical-align: middle !important; }
      body.printing-strategy table.dataTable tbody tr:nth-child(even) { background-color: #F4F6F8 !important; }
      body.printing-strategy .pitcher-print-grid  { column-count: 2 !important; column-gap: 10px !important; }
      body.printing-strategy .pitcher-print-card  { display: inline-block !important; width: 100% !important; break-inside: avoid !important; page-break-inside: avoid !important; margin-bottom: 10px !important; }
      body.printing-strategy .hitter-strategy-report-col { width: 25% !important; float: left !important; padding-left: 4px !important; padding-right: 4px !important; margin-bottom: 8px !important; }
      body.printing-strategy .hitter-strategy-report-col .strategy-report-card { padding: 5px !important; }
      body.printing-strategy .hitter-strategy-report-col .report-player-title  { font-size: 11px !important; }
      body.printing-strategy .hitter-strategy-report-col .report-subtitle       { font-size: 7.5px !important; }
    }
  "))),
  
  div(class = "main-title", "PBL Team Analytics Portal"),
  div(class = "subtitle",   "Player Development • Pitch Reports • Hitter Reports • Game Strategy"),
  
  navbarPage(title = NULL,
             
             # ============================================================
             # PITCHER REPORTS
             # ============================================================
             tabPanel("Pitcher Reports",
                      
                      div(class = "card", h3("Filters"),
                          fluidRow(
                            column(2, selectInput("pitcher_year", "Year:",
                                                  choices = available_years,
                                                  selected = available_years[1])),
                            column(3, uiOutput("pitcher_team_selector")),
                            column(3, uiOutput("pitcher_selector")),
                            column(4, uiOutput("pitch_type_selector"))
                          )
                      ),
                      
                      div(class = "card",
                          h2(textOutput("pitcher_header")),
                          h4(textOutput("pitcher_team_header"))),
                      
                      tabsetPanel(
                        
                        tabPanel("Pitch Outcomes",
                                 div(class = "card", div(class = "section-title", "Pitch Type Metric Review"), DTOutput("pitch_type_metric_review")),
                                 
                                 fluidRow(
                                   column(4,
                                          div(class = "card",
                                              selectInput("zone_filter", "Zone Plot Filter:",
                                                          choices  = c("All Pitches" = "all", "Swings" = "swing", "Whiffs" = "whiff",
                                                                       "Balls In Play" = "bip", "Hits" = "hit", "Hard Hit" = "hard_hit",
                                                                       "Damage" = "damage", "GB" = "ground_ball", "LD" = "line_drive",
                                                                       "FB" = "fly_ball", "PU" = "pop_up"),
                                                          selected = "all"),
                                              plotlyOutput("pitch_zone_filtered", height = "500px")
                                          )
                                   ),
                                   column(8,
                                          div(class = "card", div(class = "section-title", "RHH vs LHH Outcomes"), DTOutput("handedness_outcomes")),
                                          div(class = "card",
                                              selectInput("trend_metric", "Trend Metric:",
                                                          choices  = c("Velocity" = "rel_speed", "Spin Rate" = "spin_rate",
                                                                       "V Break" = "vert_break", "Ind V Break" = "induced_vert_break",
                                                                       "H Break" = "horz_break", "VAA" = "vert_appr_angle",
                                                                       "Rel Height" = "rel_height", "Rel Side" = "rel_side",
                                                                       "Extension" = "extension"),
                                                          selected = "rel_speed"),
                                              plotlyOutput("pitch_trend_plot", height = "400px")
                                          )
                                   )
                                 ),
                                 
                                 div(class = "card", div(class = "section-title", "RHH / LHH Sequence Summary"),
                                     fluidRow(
                                       column(6, h4("vs RHH"), DTOutput("sequence_rhh")),
                                       column(6, h4("vs LHH"), DTOutput("sequence_lhh"))
                                     )
                                 ),
                                 
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Pitch Sequencing"),  plotlyOutput("pitch_sequence_chart",  height = "420px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Count Sequencing"),  plotlyOutput("count_sequence_chart",  height = "420px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Result Sequencing"), plotlyOutput("result_sequence_chart", height = "420px")))
                                 ),
                                 
                                 div(class = "card", div(class = "section-title", "Pitch Performance Summary"), DTOutput("pitch_performance_summary"))
                        ),
                        
                        tabPanel("Batted Balls",
                                 div(class = "card", div(class = "section-title", "Batted Ball Review"), DTOutput("batted_ball_review")),
                                 
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Spray Chart"),    plotlyOutput("spray_chart_result",  height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "EV Spray Chart"), plotlyOutput("spray_chart_ev",      height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Contact Type"),   plotlyOutput("spray_chart_contact", height = "450px")))
                                 ),
                                 
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Heat Map 1"),
                                                 selectInput("bb_heat_metric_1", "Metric:",
                                                             choices  = c("Avg EV" = "avg_ev", "Hard Hit %" = "hard_hit_pct", "Hard Hit #" = "hard_hit_count",
                                                                          "Damage %" = "damage_pct", "Damage #" = "damage_count", "Avg LA" = "avg_la",
                                                                          "Batted Balls" = "batted_balls", "GB%" = "gb_pct", "LD%" = "ld_pct",
                                                                          "FB%" = "fb_pct", "PU%" = "pu_pct", "Swing%" = "swing_pct", "Swing#" = "swing_count"),
                                                             selected = "avg_ev"),
                                                 plotlyOutput("bb_heat_ev", height = "450px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Heat Map 2"),
                                                 selectInput("bb_heat_metric_2", "Metric:",
                                                             choices  = c("Avg EV" = "avg_ev", "Hard Hit %" = "hard_hit_pct", "Hard Hit #" = "hard_hit_count",
                                                                          "Damage %" = "damage_pct", "Damage #" = "damage_count", "Avg LA" = "avg_la",
                                                                          "Batted Balls" = "batted_balls", "GB%" = "gb_pct", "LD%" = "ld_pct",
                                                                          "FB%" = "fb_pct", "PU%" = "pu_pct", "Swing%" = "swing_pct", "Swing#" = "swing_count"),
                                                             selected = "hard_hit_pct"),
                                                 plotlyOutput("bb_heat_hardhit", height = "450px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Heat Map 3"),
                                                 selectInput("bb_heat_metric_3", "Metric:",
                                                             choices  = c("Avg EV" = "avg_ev", "Hard Hit %" = "hard_hit_pct", "Hard Hit #" = "hard_hit_count",
                                                                          "Damage %" = "damage_pct", "Damage #" = "damage_count", "Avg LA" = "avg_la",
                                                                          "Batted Balls" = "batted_balls", "GB%" = "gb_pct", "LD%" = "ld_pct",
                                                                          "FB%" = "fb_pct", "PU%" = "pu_pct", "Swing%" = "swing_pct", "Swing#" = "swing_count"),
                                                             selected = "damage_pct"),
                                                 plotlyOutput("bb_heat_damage", height = "450px")
                                   ))
                                 ),
                                 
                                 div(class = "card", div(class = "section-title", "Batted Ball By Field"), DTOutput("batted_ball_field")),
                                 
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Catcher View"),
                                                 selectInput("bb_catcher_color", "Color By:",
                                                             choices = c("EV" = "exit_speed", "Pitch Type" = "pitch_type",
                                                                         "Contact Type" = "contact_type", "Play Result" = "play_result"),
                                                             selected = "exit_speed"),
                                                 plotlyOutput("bb_catcher_view", height = "450px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Top View"),
                                                 selectInput("bb_top_color", "Color By:",
                                                             choices = c("EV" = "exit_speed", "Pitch Type" = "pitch_type",
                                                                         "Contact Type" = "contact_type", "Play Result" = "play_result"),
                                                             selected = "exit_speed"),
                                                 plotlyOutput("bb_top_view", height = "450px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Side View"),
                                                 selectInput("bb_side_color", "Color By:",
                                                             choices = c("EV" = "exit_speed", "Pitch Type" = "pitch_type",
                                                                         "Contact Type" = "contact_type", "Play Result" = "play_result"),
                                                             selected = "exit_speed"),
                                                 plotlyOutput("bb_side_view", height = "450px")
                                   ))
                                 ),
                                 
                                 div(class = "card", div(class = "section-title", "Point of Contact"), DTOutput("point_of_contact_table")),
                                 
                                 div(class = "card", div(class = "section-title", "Batted Ball Histogram"),
                                     fluidRow(
                                       column(3, selectInput("bb_hist_filter",  "Filter:",   choices = c("Batted Balls" = "all", "Hard Hit" = "hard_hit", "Damage" = "damage"), selected = "all")),
                                       column(3, selectInput("bb_hist_metric",  "X Axis:",   choices = c("Exit Velocity" = "exit_speed", "Launch Angle" = "angle", "Launch Direction" = "direction", "Distance" = "distance"), selected = "exit_speed")),
                                       column(3, selectInput("bb_bin_width",    "Bin Width:", choices = c(5, 10, 15, 20), selected = 5)),
                                       column(3, selectInput("bb_hist_color",   "Color By:", choices = c("All one color" = "one", "Pitch Type" = "pitch_type", "Play Result" = "play_result"), selected = "one"))
                                     ),
                                     plotlyOutput("bb_histogram", height = "500px")
                                 )
                        ),
                        
                        tabPanel("Break & Movement",
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "HB vs VB"),
                                                 selectInput("movement_view_1", NULL, choices = c("Pitches", "Clusters"), selected = "Pitches"),
                                                 plotlyOutput("hb_vb_plot", height = "500px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "HB vs IVB"),
                                                 selectInput("movement_view_2", NULL, choices = c("Pitches", "Clusters"), selected = "Pitches"),
                                                 plotlyOutput("hb_ivb_plot", height = "500px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Spin Axis / Tilt"), plotlyOutput("spin_axis_plot", height = "500px")))
                                 ),
                                 div(class = "card", div(class = "section-title", "Break & Movement Summary"), DTOutput("movement_table")),
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "V Break Over Time"),          plotlyOutput("vbreak_time_plot",  height = "430px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Induced V Break Over Time"), plotlyOutput("ivb_time_plot",     height = "430px"))),
                                   column(4, div(class = "card", div(class = "section-title", "H Break Over Time"),          plotlyOutput("hbreak_time_plot",  height = "430px")))
                                 )
                        ),
                        
                        tabPanel("Release Point",
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Release Point: Catcher View"), plotlyOutput("release_catcher_plot", height = "500px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Release Point: Back Mound"),   plotlyOutput("release_back_plot",    height = "500px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Release Point: Side Mound"),   plotlyOutput("release_side_plot",    height = "500px")))
                                 ),
                                 div(class = "card", div(class = "section-title", "Release Point"), DTOutput("release_table"))
                        ),
                        
                        tabPanel("Heat Maps", heatmap_tab_ui("pitcher_heat"))
                      )
             ),
             
             # ============================================================
             # HITTER REPORTS
             # ============================================================
             tabPanel("Hitter Reports",
                      
                      div(class = "card", h3("Hitter Filters"),
                          fluidRow(
                            column(2, selectInput("hitter_year", "Year:",
                                                  choices = available_years,
                                                  selected = available_years[1])),
                            column(3, uiOutput("hitter_team_selector")),
                            column(3, uiOutput("hitter_selector")),
                            column(4, uiOutput("hitter_pitch_type_selector"))
                          )
                      ),
                      
                      div(class = "card",
                          h2(textOutput("hitter_header")),
                          h4(textOutput("hitter_team_header"))),
                      
                      tabsetPanel(
                        
                        tabPanel("Batted Balls",
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Pitch Location"), plotlyOutput("hitter_bb_location",  height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Spray Chart"),    plotlyOutput("hitter_spray_result", height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Heat Map"),       plotlyOutput("hitter_bb_heat_ev",   height = "450px")))
                                 ),
                                 div(class = "card", div(class = "section-title", "Batted Ball"), DTOutput("hitter_batted_ball_review")),
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Spray Chart"),    plotlyOutput("hitter_spray_chart_result",  height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "EV Spray Chart"), plotlyOutput("hitter_spray_chart_ev",      height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Contact Type"),   plotlyOutput("hitter_spray_chart_contact", height = "450px")))
                                 ),
                                 div(class = "card", div(class = "section-title", "Batted Ball By Field"), DTOutput("hitter_batted_ball_field")),
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Catcher View"), plotlyOutput("hitter_catcher_view", height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Top View"),     plotlyOutput("hitter_top_view",     height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Side View"),    plotlyOutput("hitter_side_view",    height = "450px")))
                                 ),
                                 div(class = "card", div(class = "section-title", "Point of Contact"), DTOutput("hitter_point_of_contact_table")),
                                 div(class = "card", div(class = "section-title", "Batted Ball Histogram"),
                                     fluidRow(
                                       column(3, selectInput("hitter_bb_hist_filter", "Filter:",   choices = c("Batted Balls" = "all", "Hard Hit" = "hard_hit", "Damage" = "damage"), selected = "all")),
                                       column(3, selectInput("hitter_bb_hist_metric", "X Axis:",   choices = c("Exit Velocity" = "exit_speed", "Launch Angle" = "angle", "Launch Direction" = "direction", "Distance" = "distance"), selected = "exit_speed")),
                                       column(3, selectInput("hitter_bb_bin_width",   "Bin Width:", choices = c(5, 10, 15, 20), selected = 5)),
                                       column(3, selectInput("hitter_bb_hist_color",  "Color By:", choices = c("All one color" = "one", "Pitch Type" = "pitch_type", "Play Result" = "play_result"), selected = "one"))
                                     ),
                                     plotlyOutput("hitter_bb_histogram", height = "500px")
                                 )
                        ),
                        
                        tabPanel("Plate Discipline",
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Pitch Location"), plotlyOutput("hitter_poc_location", height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Spray Chart"),    plotlyOutput("hitter_poc_spray",    height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Contour Graph"),
                                                 selectInput("hitter_poc_contour_filter", NULL,
                                                             choices = c("All Pitches" = "all", "Swings" = "swing", "Swing and Miss" = "whiff", "In Play" = "bip"),
                                                             selected = "whiff"),
                                                 plotOutput("hitter_poc_contour", height = "420px")
                                   ))
                                 ),
                                 div(class = "card", div(class = "section-title", "Plate Discipline"), DTOutput("hitter_plate_discipline_table"))
                        ),
                        
                        tabPanel("Point of Contact",
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Catcher View"),
                                                 selectInput("hitter_poc_catcher_color", "Color By:",
                                                             choices = c("Exit Velocity" = "exit_speed", "Pitch Type" = "pitch_type",
                                                                         "Contact Type" = "contact_type", "Play Result" = "play_result"),
                                                             selected = "exit_speed"),
                                                 plotlyOutput("hitter_poc_catcher_view", height = "450px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Top View"),
                                                 selectInput("hitter_poc_top_color", "Color By:",
                                                             choices = c("Exit Velocity" = "exit_speed", "Pitch Type" = "pitch_type",
                                                                         "Contact Type" = "contact_type", "Play Result" = "play_result"),
                                                             selected = "exit_speed"),
                                                 plotlyOutput("hitter_poc_top_view", height = "450px")
                                   )),
                                   column(4, div(class = "card", div(class = "section-title", "Side View"),
                                                 selectInput("hitter_poc_side_color", "Color By:",
                                                             choices = c("Exit Velocity" = "exit_speed", "Pitch Type" = "pitch_type",
                                                                         "Contact Type" = "contact_type", "Play Result" = "play_result"),
                                                             selected = "exit_speed"),
                                                 plotlyOutput("hitter_poc_side_view", height = "450px")
                                   ))
                                 ),
                                 fluidRow(
                                   column(4, div(class = "card", div(class = "section-title", "Spray Chart"),    plotlyOutput("hitter_poc_spray_result",  height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "EV Spray Chart"), plotlyOutput("hitter_poc_spray_ev",      height = "450px"))),
                                   column(4, div(class = "card", div(class = "section-title", "Contact Type"),   plotlyOutput("hitter_poc_spray_contact", height = "450px")))
                                 ),
                                 div(class = "card", div(class = "section-title", "Point of Contact"), DTOutput("hitter_poc_main_table")),
                                 div(class = "card", div(class = "section-title", "Pull"),   DTOutput("hitter_poc_pull_table")),
                                 div(class = "card", div(class = "section-title", "Middle"), DTOutput("hitter_poc_middle_table")),
                                 div(class = "card", div(class = "section-title", "Oppo"),   DTOutput("hitter_poc_oppo_table"))
                        ),
                        
                        tabPanel("Blast Motion",
                                 fluidRow(
                                   column(6, div(class = "card", div(class = "section-title", "Batted Ball Profile"),          DTOutput("hitter_blast_profile_table"))),
                                   column(6, div(class = "card", div(class = "section-title", "Batted Ball Profile By Field"), DTOutput("hitter_blast_field_table")))
                                 )
                        ),
                        
                        tabPanel("Heat Maps", heatmap_tab_ui("hitter_heat"))
                      )
             ),
             
             # ============================================================
             # DEVELOPMENT PLANS
             # ============================================================
             tabPanel("Development Plans",
                      
                      div(class = "card", h3("Development Filters"),
                          fluidRow(
                            column(2, selectInput("dev_year", "Year:",
                                                  choices = available_years,
                                                  selected = available_years[1])),
                            column(2, selectInput("dev_player_type", "Player Type:", choices = c("Pitcher", "Hitter"), selected = "Pitcher")),
                            column(2, uiOutput("dev_team_selector")),
                            column(2, uiOutput("dev_player_selector")),
                            column(2, selectInput("dev_focus_area", "Focus Area:",
                                                  choices = c("Velocity", "Spin Rate", "Movement", "Command", "Strike Throwing",
                                                              "Whiff Ability", "Chase", "Contact Quality", "Approach", "Other"))),
                            column(2, selectInput("dev_priority", "Priority:", choices = c("Low", "Medium", "High"), selected = "Medium")),
                            column(2, selectInput("dev_status",   "Status:",   choices = c("Open", "In Progress", "Resolved"), selected = "Open"))
                          )
                      ),
                      
                      tabsetPanel(
                        tabPanel("Create Plan",
                                 div(class = "card", div(class = "section-title", "Player Development Note"),
                                     dateInput("dev_note_date",   "Date:", value = Sys.Date()),
                                     textAreaInput("dev_observation", "What are we seeing?",
                                                   placeholder = "Example: Spin rate has dropped over the last 3 outings and slider shape is getting flatter.", height = "120px"),
                                     textAreaInput("dev_adjustment", "Adjustment Plan:",
                                                   placeholder = "Example: Focus on staying behind the ball, tighter wrist position, and monitor slider spin during catch play.", height = "120px"),
                                     textAreaInput("dev_follow_up",  "Follow-Up Notes:",
                                                   placeholder = "Example: Recheck after next outing. Compare spin rate and HB/IVB trend.", height = "100px"),
                                     actionButton("save_dev_note", "Save Development Note", class = "print-button")
                                 ),
                                 div(class = "card", div(class = "section-title", "Saved Notes for Selected Player"), DTOutput("dev_player_notes_table"))
                        ),
                        
                        tabPanel("Metric Progress",
                                 div(class = "card", div(class = "section-title", "Metric Trend + Forecast"),
                                     fluidRow(
                                       column(4, uiOutput("dev_metric_selector")),
                                       column(4, selectInput("dev_forecast_games", "Forecast Games Ahead:", choices = c(3, 5, 7, 10), selected = 5)),
                                       column(4, dateInput("dev_adjustment_date", "Adjustment Date:", value = Sys.Date()))
                                     ),
                                     plotlyOutput("dev_metric_forecast_plot", height = "550px")
                                 ),
                                 div(class = "card", div(class = "section-title", "Before / After Adjustment"), DTOutput("dev_before_after_table"))
                        ),
                        
                        tabPanel("Active Plans",
                                 div(class = "card", div(class = "section-title", "All Development Plans"), DTOutput("dev_all_notes_table"))
                        )
                      )
             ),
             
             # ============================================================
             # GAME STRATEGY
             # ============================================================
             tabPanel("Game Strategy",
                      
                      div(class = "card no-print", h3("Game Strategy Filters"),
                          fluidRow(
                            column(2, selectInput("strategy_year", "Year:",
                                                  choices = available_years,
                                                  selected = available_years[1])),
                            column(3, uiOutput("strategy_team_selector")),
                            column(3, uiOutput("strategy_pitcher_selector")),
                            column(3, uiOutput("strategy_hitter_selector")),
                            column(3, br(),
                                   actionButton("run_strategy_reports", "Run Reports", class = "print-button"),
                                   tags$a(href = "#", class = "print-button",
                                          onclick = "document.body.classList.add('printing-strategy'); window.print(); setTimeout(function(){ document.body.classList.remove('printing-strategy'); }, 1000); return false;",
                                          "Print Report")
                            )
                          )
                      ),
                      
                      div(class = "strategy-print-title", textOutput("strategy_print_team")),
                      
                      tabsetPanel(
                        tabPanel("Pitch Type Summary",
                                 div(class = "card", div(class = "section-title", "Printable Pitcher Pitch Type Summary"), uiOutput("pitcher_strategy_report"))
                        ),
                        tabPanel("Hitter Summary",
                                 div(class = "card", div(class = "section-title", "Printable Hitter Spray / Approach Report"), uiOutput("hitter_strategy_report"))
                        )
                      )
             )
  )
)

# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output, session) {
  
  # ---- Year Selector ----
  
  get_master_for_year <- function(selected_year) {
    if (selected_year == "2025") {
      master_2025
    } else if (selected_year == "2026") {
      master_2026
    } else {
      tibble()
    }
  }
  
  # ---- Pitcher year/team/player selectors ----
  
  output$pitcher_team_selector <- renderUI({
    req(input$pitcher_year)
    
    teams <- pitcher_summary %>%
      filter(year == input$pitcher_year) %>%
      distinct(pitcher_team_name) %>%
      arrange(pitcher_team_name) %>%
      pull(pitcher_team_name)
    
    selectInput("pitcher_team", "Team:", choices = teams, selected = teams[1])
  })
  
  output$pitcher_selector <- renderUI({
    req(input$pitcher_year, input$pitcher_team)
    
    pitchers <- pitcher_summary %>%
      filter(
        year == input$pitcher_year,
        pitcher_team_name == input$pitcher_team
      ) %>%
      distinct(pitcher_name) %>%
      arrange(pitcher_name) %>%
      pull(pitcher_name)
    
    selectInput("pitcher", "Pitcher:", choices = pitchers, selected = pitchers[1])
  })
  
  output$pitch_type_selector <- renderUI({
    req(input$pitcher_year)
    
    types <- get_master_for_year(input$pitcher_year) %>%
      distinct(pitch_type) %>%
      arrange(pitch_type) %>%
      pull(pitch_type)
    
    checkboxGroupInput(
      "pitch_types",
      "Pitch Types:",
      choices = types,
      selected = types,
      inline = TRUE
    )
  })
  
  # ---- CACHED core reactives ----
  # bindCache() stores the result keyed by the cache arguments.
  # If the same combination of inputs is requested again, the cached
  # result is returned instantly without re-filtering master.
  
  pitcher_master <- reactive({
    req(input$pitcher_year, input$pitcher, input$pitcher_team, input$pitch_types)
    
    get_master_for_year(input$pitcher_year) %>%
      filter(
        pitcher_name == input$pitcher,
        pitcher_team_name == input$pitcher_team,
        pitch_type %in% input$pitch_types
      )
  }) %>% bindCache(input$pitcher_year, input$pitcher, input$pitcher_team, input$pitch_types)
  
  pitcher_type_data <- reactive({
    req(input$pitcher_year, input$pitcher, input$pitcher_team, input$pitch_types)
    
    pitcher_pitch_type %>%
      filter(
        year == input$pitcher_year,
        pitcher_name == input$pitcher,
        pitcher_team_name == input$pitcher_team,
        pitch_type %in% input$pitch_types
      )
  }) %>% bindCache(input$pitcher_year, input$pitcher, input$pitcher_team, input$pitch_types)
  
  pitcher_location_data <- reactive({
    req(input$pitcher_year, input$pitcher, input$pitcher_team, input$pitch_types)
    
    pitch_location %>%
      filter(
        year == input$pitcher_year,
        pitcher_name == input$pitcher,
        pitcher_team_name == input$pitcher_team,
        pitch_type %in% input$pitch_types
      )
  }) %>% bindCache(input$pitcher_year, input$pitcher, input$pitcher_team, input$pitch_types)
  
  bb_data <- reactive({
    pitcher_master() %>% filter(has_batted_ball == 1)
  }) %>% bindCache(
    input$pitcher_year,
    input$pitcher,
    input$pitcher_team,
    input$pitch_types
  )
  
  output$pitcher_header      <- renderText({ paste("Pitcher Report:", input$pitcher) })
  output$pitcher_team_header <- renderText({ paste("Team:", input$pitcher_team) })
  
  # ---- Hitter selector UI ----
  # ---- Hitter year/team/player selectors ----
  
  output$hitter_team_selector <- renderUI({
    req(input$hitter_year)
    
    teams <- get_master_for_year(input$hitter_year) %>%
      distinct(batter_team_name) %>%
      arrange(batter_team_name) %>%
      pull(batter_team_name)
    
    selectInput("hitter_team", "Team:", choices = teams, selected = teams[1])
  })
  
  output$hitter_selector <- renderUI({
    req(input$hitter_year, input$hitter_team)
    
    hitters <- get_master_for_year(input$hitter_year) %>%
      filter(
        batter_team_name == input$hitter_team
      ) %>%
      distinct(batter_name) %>%
      arrange(batter_name) %>%
      pull(batter_name)
    
    selectInput("hitter", "Hitter:", choices = hitters, selected = hitters[1])
  })
  
  output$hitter_pitch_type_selector <- renderUI({
    req(input$hitter_year)
    
    types <- get_master_for_year(input$hitter_year) %>%
      distinct(pitch_type) %>%
      arrange(pitch_type) %>%
      pull(pitch_type)
    
    checkboxGroupInput(
      "hitter_pitch_types",
      "Pitch Types:",
      choices = types,
      selected = types,
      inline = TRUE
    )
  })
  
  # ---- CACHED hitter reactives ----
  hitter_master <- reactive({
    req(input$hitter_year, input$hitter, input$hitter_team, input$hitter_pitch_types)
    
    get_master_for_year(input$hitter_year) %>%
      filter(
        batter_name == input$hitter,
        batter_team_name == input$hitter_team,
        pitch_type %in% input$hitter_pitch_types
      )
  }) %>% bindCache(input$hitter_year, input$hitter, input$hitter_team, input$hitter_pitch_types)
  
  hitter_bb_data <- reactive({
    hitter_master() %>% filter(has_batted_ball == 1)
  }) %>% bindCache(input$hitter, input$hitter_team, input$hitter_pitch_types)
  
  output$hitter_header      <- renderText({ paste("Hitter Report:", input$hitter) })
  output$hitter_team_header <- renderText({ paste("Team:", input$hitter_team) })
  
  # ============================================================
  # PITCH OUTCOMES
  # ============================================================
  
  output$pitch_type_metric_review <- renderDT({
    d <- pitcher_master()
    total_pitches <- nrow(d)
    
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `#`          = n(),
        `%`          = n() / total_pitches,
        `%RHH`       = mean(batter_side_clean == "Right", na.rm = TRUE),
        `%LHH`       = mean(batter_side_clean == "Left",  na.rm = TRUE),
        Velo         = mean(rel_speed,  na.rm = TRUE),
        Peak         = max(rel_speed,   na.rm = TRUE),
        `1PK %`      = mean(strike[first_pitch == 1], na.rm = TRUE),
        `Strike %`   = mean(strike,     na.rm = TRUE),
        `Zone %`     = mean(in_zone,    na.rm = TRUE),
        `Edge %`     = mean(zone_bucket == "Shadow", na.rm = TRUE),
        `CSW %`      = mean(csw,        na.rm = TRUE),
        `SwStr %`    = mean(whiff,      na.rm = TRUE),
        `Whiff %`    = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
        `IZ Whiff %` = safe_rate(sum(whiff == 1 & in_zone == 1, na.rm = TRUE), sum(swing == 1 & in_zone == 1, na.rm = TRUE)),
        `Chase %`    = safe_rate(sum(chase_swing, na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
        `Foul %`     = mean(foul,       na.rm = TRUE),
        `BIP %`      = mean(bip,        na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(`%`, `%RHH`, `%LHH`, `1PK %`, `Strike %`, `Zone %`, `Edge %`,
                 `CSW %`, `SwStr %`, `Whiff %`, `IZ Whiff %`, `Chase %`, `Foul %`, `BIP %`), pct),
        across(where(is.numeric), ~ round(.x, 1))
      ) %>%
      rename(Type = pitch_type)
    
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$pitch_zone_filtered <- renderPlotly({
    d <- pitcher_master()
    if (input$zone_filter != "all") {
      d <- d %>% filter(.data[[input$zone_filter]] == 1)
    }
    p <- d %>%
      ggplot(aes(x = plate_loc_side, y = plate_loc_height, color = pitch_type,
                 text = paste0("Type: ", pitch_type, "<br>Pitch Call: ", pitch_call,
                               "<br>Result: ", play_result, "<br>Count: ", balls, "-", strikes))) +
      geom_point(size = 2.5, alpha = 0.8) +
      geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
                fill = NA, color = "black", linewidth = 1.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      scale_color_manual(values = pitch_colors) +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5.5)) +
      theme_minimal() +
      labs(x = "Plate Loc Side", y = "Plate Loc Height", color = "Type")
    ggplotly(p, tooltip = "text")
  })
  
  output$handedness_outcomes <- renderDT({
    d <- pitcher_master()
    
    make_side <- function(side_label) {
      d %>%
        filter(batter_side_clean == side_label) %>%
        group_by(pitch_type) %>%
        summarise(
          `1PK%`        = mean(strike[first_pitch == 1], na.rm = TRUE),
          `Strike%`     = mean(strike,   na.rm = TRUE),
          `Zone%`       = mean(in_zone,  na.rm = TRUE),
          `CSW%`        = mean(csw,      na.rm = TRUE),
          `SwStr%`      = mean(whiff,    na.rm = TRUE),
          `Whiff%`      = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
          `IZ Whiff %`  = safe_rate(sum(whiff == 1 & in_zone == 1, na.rm = TRUE), sum(swing == 1 & in_zone == 1, na.rm = TRUE)),
          `Chase %`     = safe_rate(sum(chase_swing, na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(across(-pitch_type, pct))
    }
    
    rhh <- make_side("Right") %>% rename_with(~ paste0(.x, " vs RHH"), -pitch_type)
    lhh <- make_side("Left")  %>% rename_with(~ paste0(.x, " vs LHH"), -pitch_type)
    
    full_join(rhh, lhh, by = "pitch_type") %>%
      rename(Type = pitch_type) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  make_sequence_data <- function(d) {
    d %>%
      arrange(date, pitch_no) %>%
      mutate(
        prev_pitch_type   = lag(pitch_type),
        prev_ball         = lag(ball,          default = 0),
        prev_called_strike = lag(called_strike, default = 0),
        prev_whiff        = lag(whiff,          default = 0),
        pitch_sequence_bucket = case_when(
          first_pitch == 1         ~ "1P",
          !is.na(prev_pitch_type)  ~ paste0("A", prev_pitch_type),
          TRUE                     ~ "Other"
        ),
        count_sequence_bucket = case_when(
          first_pitch == 1               ~ "1P",
          balls == 3 & strikes == 2      ~ "Full",
          strikes > balls                ~ "Ahead",
          balls   > strikes              ~ "Behind",
          balls   == strikes             ~ "Even",
          TRUE                           ~ "Other"
        ),
        result_sequence_bucket = case_when(
          first_pitch == 1       ~ "1P",
          prev_ball == 1         ~ "AB",
          prev_called_strike == 1 ~ "ACS",
          prev_whiff == 1        ~ "AW",
          TRUE                   ~ "Other"
        )
      )
  }
  
  output$pitch_trend_plot <- renderPlotly({
    req(input$trend_metric)
    
    metric <- input$trend_metric
    
    d <- pitcher_master() %>%
      filter(!is.na(.data[[metric]]), !is.na(pitch_type)) %>%
      arrange(date, pitch_no)
    
    validate(
      need(nrow(d) > 0, paste("No usable", metric, "data for this pitcher."))
    )
    
    d <- d %>%
      group_by(pitch_type) %>%
      mutate(
        pitch_type_pitch_no = row_number(),
        metric_start = first(.data[[metric]]),
        metric_change = .data[[metric]] - metric_start
      ) %>%
      ungroup()
    
    validate(
      need(sum(!is.na(d$metric_change)) > 0, "No usable trend data for this metric.")
    )
    
    p <- ggplot(
      d,
      aes(
        x = pitch_type_pitch_no,
        y = metric_change,
        color = pitch_type,
        group = pitch_type,
        text = paste0(
          "Type: ", pitch_type,
          "<br>Pitch Type Pitch #: ", pitch_type_pitch_no,
          "<br>Actual: ", round(.data[[metric]], 2),
          "<br>Change From Start: ", round(metric_change, 2)
        )
      )
    ) +
      geom_hline(yintercept = 0, color = "gray70", linewidth = 0.6) +
      geom_line(alpha = 0.25, linewidth = 0.4) +
      geom_point(size = 1.2, alpha = 0.55) +
      geom_smooth(se = FALSE, linewidth = 1, alpha = 0.95, span = 0.20) +
      scale_color_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = "Pitch Number Within Pitch Type",
        y = "Change From First Pitch",
        color = "Type"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$sequence_rhh <- renderDT({
    d <- pitcher_master() %>% filter(batter_side_clean == "Right")
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `All Counts`     = n(),
        `1st Pitch`      = sum(first_pitch == 1,                 na.rm = TRUE),
        `Batter Ahead`   = sum(count_bucket == "Batter Ahead",   na.rm = TRUE),
        Even             = sum(count_bucket == "Even",            na.rm = TRUE),
        `Pitcher Ahead`  = sum(count_bucket == "Pitcher Ahead",  na.rm = TRUE),
        `2 Strikes`      = sum(two_strike_count == 1,             na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(c(`All Counts`, `1st Pitch`, `Batter Ahead`, Even, `Pitcher Ahead`, `2 Strikes`),
                    ~ round(.x / sum(`All Counts`) * 100, 1))) %>%
      rename(Type = pitch_type)
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$sequence_lhh <- renderDT({
    d <- pitcher_master() %>% filter(batter_side_clean == "Left")
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `All Counts`     = n(),
        `1st Pitch`      = sum(first_pitch == 1,                 na.rm = TRUE),
        `Batter Ahead`   = sum(count_bucket == "Batter Ahead",   na.rm = TRUE),
        Even             = sum(count_bucket == "Even",            na.rm = TRUE),
        `Pitcher Ahead`  = sum(count_bucket == "Pitcher Ahead",  na.rm = TRUE),
        `2 Strikes`      = sum(two_strike_count == 1,             na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(c(`All Counts`, `1st Pitch`, `Batter Ahead`, Even, `Pitcher Ahead`, `2 Strikes`),
                    ~ round(.x / sum(`All Counts`) * 100, 1))) %>%
      rename(Type = pitch_type)
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$pitch_sequence_chart <- renderPlotly({
    d <- make_sequence_data(pitcher_master()) %>%
      count(pitch_sequence_bucket, pitch_type) %>%
      mutate(pitch_sequence_bucket = factor(pitch_sequence_bucket,
                                            levels = c("1P", paste0("A", sort(unique(pitcher_master()$pitch_type))), "Other")))
    p <- d %>%
      ggplot(aes(x = n, y = pitch_sequence_bucket, fill = pitch_type)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = pitch_colors) +
      scale_x_continuous(labels = percent) +
      theme_minimal() +
      labs(x = "Pitch Usage %", y = NULL, fill = "Type")
    ggplotly(p)
  })
  
  output$count_sequence_chart <- renderPlotly({
    d <- make_sequence_data(pitcher_master()) %>%
      count(count_sequence_bucket, pitch_type) %>%
      mutate(count_sequence_bucket = factor(count_sequence_bucket,
                                            levels = c("1P", "Ahead", "Behind", "Even", "Full", "Other")))
    p <- d %>%
      ggplot(aes(x = n, y = count_sequence_bucket, fill = pitch_type)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = pitch_colors) +
      scale_x_continuous(labels = percent) +
      theme_minimal() +
      labs(x = "Pitch Usage %", y = NULL, fill = "Type")
    ggplotly(p)
  })
  
  output$result_sequence_chart <- renderPlotly({
    d <- make_sequence_data(pitcher_master()) %>%
      count(result_sequence_bucket, pitch_type) %>%
      mutate(result_sequence_bucket = factor(result_sequence_bucket,
                                             levels = c("1P", "AB", "ACS", "AW", "Other")))
    p <- d %>%
      ggplot(aes(x = n, y = result_sequence_bucket, fill = pitch_type)) +
      geom_col(position = "fill") +
      scale_fill_manual(values = pitch_colors) +
      scale_x_continuous(labels = percent) +
      theme_minimal() +
      labs(x = "Pitch Usage %", y = NULL, fill = "Type")
    ggplotly(p)
  })
  
  output$pitch_performance_summary <- renderDT({
    d     <- pitcher_master()
    total <- nrow(d)
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `#`         = n(),
        `%`         = n() / total,
        `Inside%`   = mean(plate_loc_side < -0.33, na.rm = TRUE),
        `Low%`      = mean(plate_loc_height < 2.2,  na.rm = TRUE),
        `High%`     = mean(plate_loc_height > 2.8,  na.rm = TRUE),
        `Edge%`     = mean(zone_bucket == "Shadow",  na.rm = TRUE),
        `1PK%`      = mean(strike[first_pitch == 1], na.rm = TRUE),
        `Strike%`   = mean(strike,      na.rm = TRUE),
        `Contact%`  = safe_rate(sum(contact,     na.rm = TRUE), sum(swing, na.rm = TRUE)),
        `Zone%`     = mean(in_zone,     na.rm = TRUE),
        `Z Swing%`  = safe_rate(sum(zone_swing,  na.rm = TRUE), sum(in_zone == 1, na.rm = TRUE)),
        `Z Cont %`  = safe_rate(sum(zone_contact, na.rm = TRUE), sum(zone_swing,  na.rm = TRUE)),
        `O Swing%`  = safe_rate(sum(chase_swing,  na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
        `O Cont %`  = safe_rate(sum(chase_contact, na.rm = TRUE), sum(chase_swing, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(across(-c(pitch_type, `#`), pct)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
      rename(Type = pitch_type)
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # ============================================================
  # BATTED BALLS
  # ============================================================
  
  output$batted_ball_review <- renderDT({
    d <- bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `#`          = n(),
        EV           = mean(exit_speed,  na.rm = TRUE),
        `Peak EV`    = max(exit_speed,   na.rm = TRUE),
        LA           = mean(angle,       na.rm = TRUE),
        `Hard Hit LA` = mean(angle[hard_hit == 1], na.rm = TRUE),
        `Hard Hit %` = safe_rate(sum(hard_hit,  na.rm = TRUE), n()),
        `Damage %`   = safe_rate(sum(damage,    na.rm = TRUE), n()),
        `GB %`       = safe_rate(sum(ground_ball, na.rm = TRUE), n()),
        `LD %`       = safe_rate(sum(line_drive,  na.rm = TRUE), n()),
        `FB %`       = safe_rate(sum(fly_ball,    na.rm = TRUE), n()),
        `PU %`       = safe_rate(sum(pop_up,      na.rm = TRUE), n()),
        `GB/FB`      = safe_rate(sum(ground_ball, na.rm = TRUE), sum(fly_ball, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(`Hard Hit %`, `Damage %`, `GB %`, `LD %`, `FB %`, `PU %`), pct),
        across(where(is.numeric), ~ round(.x, 1))
      ) %>%
      rename(Type = pitch_type)
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$batted_ball_field <- renderDT({
    d <- bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `#`       = n(),
        `Pull %`  = safe_rate(sum(pull_side,    na.rm = TRUE), n()),
        `Pull EV` = mean(exit_speed[pull_side == 1],   na.rm = TRUE),
        `Pull LA` = mean(angle[pull_side == 1],        na.rm = TRUE),
        `Mid %`   = safe_rate(sum(middle_field,  na.rm = TRUE), n()),
        `Mid EV`  = mean(exit_speed[middle_field == 1], na.rm = TRUE),
        `Mid LA`  = mean(angle[middle_field == 1],       na.rm = TRUE),
        `Oppo %`  = safe_rate(sum(oppo_side,     na.rm = TRUE), n()),
        `Oppo EV` = mean(exit_speed[oppo_side == 1],   na.rm = TRUE),
        `Oppo LA` = mean(angle[oppo_side == 1],        na.rm = TRUE),
        `IFH %`   = safe_rate(sum(play_result == "Single" & distance < 120, na.rm = TRUE), n()),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(`Pull %`, `Mid %`, `Oppo %`, `IFH %`), pct),
        across(where(is.numeric), ~ round(.x, 1))
      ) %>%
      rename(Type = pitch_type)
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$point_of_contact_table <- renderDT({
    d <- bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        `#`               = n(),
        `%`               = n() / nrow(d),
        `Contact Ht`      = mean(contact_position_z, na.rm = TRUE),
        `Contact Side`    = mean(contact_position_x, na.rm = TRUE),
        `Contact Depth`   = mean(contact_position_y, na.rm = TRUE),
        `Pull Cont Ht`    = mean(contact_position_z[pull_side == 1],   na.rm = TRUE),
        `Pull Cont Side`  = mean(contact_position_x[pull_side == 1],   na.rm = TRUE),
        `Pull Cont Depth` = mean(contact_position_y[pull_side == 1],   na.rm = TRUE),
        `Mid Cont Ht`     = mean(contact_position_z[middle_field == 1], na.rm = TRUE),
        `Mid Cont Side`   = mean(contact_position_x[middle_field == 1], na.rm = TRUE),
        `Mid Cont Depth`  = mean(contact_position_y[middle_field == 1], na.rm = TRUE),
        `Oppo Cont Ht`    = mean(contact_position_z[oppo_side == 1],   na.rm = TRUE),
        `Oppo Cont Side`  = mean(contact_position_x[oppo_side == 1],   na.rm = TRUE),
        `Oppo Cont Depth` = mean(contact_position_y[oppo_side == 1],   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(`%` = pct(`%`)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      rename(Type = pitch_type)
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  make_spray <- function(d, color_var) {
    d <- prepare_spray_data(d)
    ggplot(d, aes(x = spray_x, y = spray_y, color = .data[[color_var]],
                  text = paste0("Type: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                "<br>LA: ", round(angle, 1), "<br>Bearing: ", round(spray_angle, 1),
                                "<br>Distance: ", round(distance, 1), "<br>Result: ", play_result))) +
      add_spray_field(d) +
      geom_point(size = 3, alpha = 0.9) +
      coord_fixed(xlim = c(-325, 325), ylim = c(-20, 450)) +
      theme_void() +
      theme(legend.position = "top") +
      labs(color = color_var)
  }
  
  output$spray_chart_result  <- renderPlotly({ ggplotly(make_spray(bb_data(), "play_result"),   tooltip = "text") })
  output$spray_chart_contact <- renderPlotly({
    d <- bb_data() %>% mutate(contact_type = case_when(
      ground_ball == 1 ~ "GB", line_drive == 1 ~ "LD",
      fly_ball == 1 ~ "FB", pop_up == 1 ~ "PU", TRUE ~ "Other"))
    ggplotly(make_spray(d, "contact_type"), tooltip = "text")
  })
  
  output$spray_chart_ev <- renderPlotly({
    d <- prepare_spray_data(bb_data())
    p <- ggplot(d, aes(x = spray_x, y = spray_y, color = exit_speed,
                       text = paste0("Type: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                     "<br>LA: ", round(angle, 1), "<br>Bearing: ", round(spray_angle, 1),
                                     "<br>Distance: ", round(distance, 1), "<br>Result: ", play_result))) +
      add_spray_field(d) +
      geom_point(size = 3, alpha = 0.9) +
      scale_color_gradient(low = "#3A5AFE", high = "#D62828") +
      coord_fixed(xlim = c(-325, 325), ylim = c(-20, 450)) +
      theme_void() + theme(legend.position = "top") + labs(color = "EV")
    ggplotly(p, tooltip = "text")
  })
  
  make_bb_heat <- function(d, metric) {
    x_breaks <- c(-1.83, -0.83, -0.2767, 0.2767, 0.83, 1.83)
    y_breaks <- c(0.5, 1.5, 2.1667, 2.8333, 3.5, 4.5)
    
    heat_df <- d %>%
      make_contact_type() %>%
      mutate(
        zone_x = cut(plate_loc_side,   breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
        zone_y = cut(plate_loc_height, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
      ) %>%
      filter(!is.na(zone_x), !is.na(zone_y)) %>%
      group_by(zone_x, zone_y) %>%
      summarise(
        x_mid          = mean(c(x_breaks[zone_x[1]], x_breaks[zone_x[1] + 1])),
        y_mid          = mean(c(y_breaks[zone_y[1]], y_breaks[zone_y[1] + 1])),
        width          = x_breaks[zone_x[1] + 1] - x_breaks[zone_x[1]],
        height         = y_breaks[zone_y[1] + 1] - y_breaks[zone_y[1]],
        avg_ev         = mean(exit_speed,  na.rm = TRUE),
        hard_hit_pct   = mean(hard_hit,   na.rm = TRUE) * 100,
        hard_hit_count = sum(hard_hit,    na.rm = TRUE),
        damage_pct     = mean(damage,     na.rm = TRUE) * 100,
        damage_count   = sum(damage,      na.rm = TRUE),
        avg_la         = mean(angle,      na.rm = TRUE),
        batted_balls   = n(),
        gb_pct         = mean(ground_ball, na.rm = TRUE) * 100,
        ld_pct         = mean(line_drive,  na.rm = TRUE) * 100,
        fb_pct         = mean(fly_ball,    na.rm = TRUE) * 100,
        pu_pct         = mean(pop_up,      na.rm = TRUE) * 100,
        swing_pct      = mean(swing,       na.rm = TRUE) * 100,
        swing_count    = sum(swing,        na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(value = .data[[metric]])
    
    p <- ggplot(heat_df, aes(x = x_mid, y = y_mid, fill = value,
                             text = paste0("Value: ", round(value, 1),
                                           "<br>Batted Balls: ", batted_balls,
                                           "<br>Avg EV: ", round(avg_ev, 1),
                                           "<br>Avg LA: ", round(avg_la, 1)))) +
      geom_tile(aes(width = width, height = height), color = "#91C9FF") +
      geom_text(aes(label = round(value, 1)), color = "white", fontface = "bold", size = 4, na.rm = TRUE) +
      add_strike_zone() +
      coord_fixed(xlim = c(-1.83, 1.83), ylim = c(0.5, 4.5)) +
      theme_minimal() +
      labs(x = "Plate Loc Side", y = "Plate Loc Height", fill = metric)
    
    if (metric == "avg_ev") p <- p + ev_fill_scale else p <- p + scale_fill_gradient(low = "#2525D9", high = "#B30000", na.value = "white")
    p
  }
  
  output$bb_heat_ev      <- renderPlotly({ ggplotly(make_bb_heat(bb_data(), input$bb_heat_metric_1), tooltip = "text") })
  output$bb_heat_hardhit <- renderPlotly({ ggplotly(make_bb_heat(bb_data(), input$bb_heat_metric_2), tooltip = "text") })
  output$bb_heat_damage  <- renderPlotly({ ggplotly(make_bb_heat(bb_data(), input$bb_heat_metric_3), tooltip = "text") })
  
  make_bb_color_plot <- function(d, color_var, view = "catcher") {
    d <- d %>% make_contact_type() %>% filter(has_batted_ball == 1)
    
    if (view == "catcher") {
      d <- d %>% filter(!is.na(plate_loc_side), !is.na(plate_loc_height))
      p <- ggplot(d, aes(x = plate_loc_side, y = plate_loc_height, color = .data[[color_var]],
                         text = paste0("Pitch: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                       "<br>LA: ", round(angle, 1), "<br>Contact Type: ", contact_type,
                                       "<br>Result: ", play_result,
                                       "<br>Plate Side: ", round(plate_loc_side, 2),
                                       "<br>Plate Height: ", round(plate_loc_height, 2)))) +
        geom_point(size = 2.8, alpha = 0.85) + add_strike_zone() + add_plate_front() +
        coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5.5)) + theme_minimal() +
        labs(x = "Plate Loc Side", y = "Plate Loc Height", color = "Color")
    }
    
    if (view == "top") {
      d <- d %>% filter(!is.na(contact_position_x), !is.na(contact_position_z))
      p <- ggplot(d, aes(x = contact_position_z, y = contact_position_x, color = .data[[color_var]],
                         text = paste0("Pitch: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                       "<br>LA: ", round(angle, 1), "<br>Contact Type: ", contact_type,
                                       "<br>Result: ", play_result,
                                       "<br>Contact X: ", round(contact_position_x, 2),
                                       "<br>Contact Y: ", round(contact_position_y, 2),
                                       "<br>Contact Z: ", round(contact_position_z, 2)))) +
        geom_point(size = 2.8, alpha = 0.85) + add_top_plate_reference() +
        coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-1, 5)) + theme_minimal() +
        labs(x = "Contact Position X / Side", y = "Contact Position Z / Height", color = "Color")
    }
    
    if (view == "side") {
      d <- d %>% filter(!is.na(contact_position_z), !is.na(contact_position_y))
      p <- ggplot(d, aes(x = contact_position_x, y = contact_position_y, color = .data[[color_var]],
                         text = paste0("Pitch: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                       "<br>LA: ", round(angle, 1), "<br>Contact Type: ", contact_type,
                                       "<br>Result: ", play_result,
                                       "<br>Contact X: ", round(contact_position_x, 2),
                                       "<br>Contact Y: ", round(contact_position_y, 2),
                                       "<br>Contact Z: ", round(contact_position_z, 2)))) +
        geom_point(size = 2.8, alpha = 0.85) + add_side_plate_reference() +
        coord_fixed(xlim = c(-1, 5), ylim = c(0, 4)) + theme_minimal() +
        labs(x = "Contact Position Z / Height", y = "Contact Position Y / Depth", color = "Color")
    }
    
    if (color_var == "exit_speed")  p + ev_scale
    else if (color_var == "pitch_type") p + scale_color_manual(values = pitch_colors)
    else p
  }
  
  output$bb_catcher_view <- renderPlotly({ ggplotly(make_bb_color_plot(bb_data(), input$bb_catcher_color, "catcher"), tooltip = "text") })
  output$bb_top_view     <- renderPlotly({ ggplotly(make_bb_color_plot(bb_data(), input$bb_top_color,     "top"),     tooltip = "text") })
  output$bb_side_view    <- renderPlotly({ ggplotly(make_bb_color_plot(bb_data(), input$bb_side_color,    "side"),    tooltip = "text") })
  
  output$bb_histogram <- renderPlotly({
    d <- bb_data()
    if (input$bb_hist_filter != "all") d <- d %>% filter(.data[[input$bb_hist_filter]] == 1)
    metric    <- input$bb_hist_metric
    bin_width <- as.numeric(input$bb_bin_width)
    if (input$bb_hist_color == "one") {
      p <- d %>% ggplot(aes(x = .data[[metric]])) +
        geom_histogram(binwidth = bin_width, fill = "#003F66", color = "white") +
        theme_minimal() + labs(x = metric, y = "Batted Balls")
    } else {
      p <- d %>% ggplot(aes(x = .data[[metric]], fill = .data[[input$bb_hist_color]])) +
        geom_histogram(binwidth = bin_width, color = "white") +
        theme_minimal() + labs(x = metric, y = "Batted Balls", fill = input$bb_hist_color)
    }
    ggplotly(p)
  })
  
  # ============================================================
  # BREAK & MOVEMENT
  # ============================================================
  
  convert_to_tilt <- function(spin_axis) {
    if (is.na(spin_axis)) return(NA_character_)
    total_hours <- (spin_axis %% 360) / 30
    hour        <- floor(total_hours)
    minutes     <- round((total_hours - hour) * 60)
    if (minutes == 60) { hour <- hour + 1; minutes <- 0 }
    if (hour == 0)  hour <- 12
    if (hour > 12)  hour <- hour - 12
    paste0(hour, ":", sprintf("%02d", minutes))
  }
  
  output$movement_table <- renderDT({
    pitcher_master() %>%
      group_by(pitch_type) %>%
      summarise(
        `Pitch Type` = first(pitch_type),
        Velo         = mean(rel_speed,           na.rm = TRUE),
        Spin         = mean(spin_rate,            na.rm = TRUE),
        Efficiency   = NA_real_,
        Tilt         = convert_to_tilt(mean(spin_axis, na.rm = TRUE)),
        Axis         = mean(spin_axis,            na.rm = TRUE),
        `V Break`    = mean(vert_break,           na.rm = TRUE),
        `Induced V`  = mean(induced_vert_break,   na.rm = TRUE),
        `H Break`    = mean(horz_break,           na.rm = TRUE),
        `V Approach` = mean(vert_appr_angle,      na.rm = TRUE),
        `H Approach` = mean(horz_appr_angle,      na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$hb_vb_plot <- renderPlotly({
    d <- pitcher_master()
    if (input$movement_view_1 == "Clusters") {
      d <- d %>% group_by(pitch_type) %>%
        summarise(horz_break = mean(horz_break, na.rm = TRUE), vert_break = mean(vert_break, na.rm = TRUE), n = n(), .groups = "drop")
      p <- ggplot(d, aes(x = horz_break, y = vert_break, color = pitch_type, size = n,
                         text = paste0("Type: ", pitch_type, "<br>HB: ", round(horz_break, 1), "<br>VB: ", round(vert_break, 1), "<br>Pitches: ", n))) +
        geom_point(alpha = 0.9) + scale_size(range = c(6, 14)) + scale_color_manual(values = pitch_colors)
    } else {
      p <- ggplot(d, aes(x = horz_break, y = vert_break, color = pitch_type,
                         text = paste0("Type: ", pitch_type, "<br>HB: ", round(horz_break, 1), "<br>VB: ", round(vert_break, 1), "<br>Velo: ", round(rel_speed, 1)))) +
        geom_point(alpha = 0.75, size = 2.5) + scale_color_manual(values = pitch_colors)
    }
    p <- p + geom_hline(yintercept = 0, color = "gray70") + geom_vline(xintercept = 0, color = "gray70") +
      theme_minimal() + labs(x = "H Break", y = "V Break", color = "Pitch Type")
    ggplotly(p, tooltip = "text")
  })
  
  output$hb_ivb_plot <- renderPlotly({
    d <- pitcher_master()
    if (input$movement_view_2 == "Clusters") {
      d <- d %>% group_by(pitch_type) %>%
        summarise(horz_break = mean(horz_break, na.rm = TRUE), induced_vert_break = mean(induced_vert_break, na.rm = TRUE), n = n(), .groups = "drop")
      p <- ggplot(d, aes(x = horz_break, y = induced_vert_break, color = pitch_type, size = n,
                         text = paste0("Type: ", pitch_type, "<br>HB: ", round(horz_break, 1), "<br>IVB: ", round(induced_vert_break, 1), "<br>Pitches: ", n))) +
        geom_point(alpha = 0.9) + scale_size(range = c(6, 14)) + scale_color_manual(values = pitch_colors)
    } else {
      p <- ggplot(d, aes(x = horz_break, y = induced_vert_break, color = pitch_type,
                         text = paste0("Type: ", pitch_type, "<br>HB: ", round(horz_break, 1), "<br>IVB: ", round(induced_vert_break, 1), "<br>Velo: ", round(rel_speed, 1)))) +
        geom_point(alpha = 0.75, size = 2.5) + scale_color_manual(values = pitch_colors)
    }
    p <- p + geom_hline(yintercept = 0, color = "gray70") + geom_vline(xintercept = 0, color = "gray70") +
      theme_minimal() + labs(x = "H Break", y = "Induced V Break", color = "Pitch Type")
    ggplotly(p, tooltip = "text")
  })
  
  output$spin_axis_plot <- renderPlotly({
    d <- pitcher_master() %>%
      filter(!is.na(spin_axis), !is.na(spin_rate)) %>%
      mutate(
        pitch_type        = if_else(is.na(pitch_type), "UNKNOWN", pitch_type),
        tilt_label        = if_else(is.na(tilt), "NA", as.character(tilt)),
        spin_axis_rotated = (spin_axis + 180) %% 360
      )
    validate(need(nrow(d) > 0, "No spin axis / spin rate data available for this pitcher."))
    plot_ly(data = d, type = "scatterpolar", mode = "markers",
            r = ~spin_rate, theta = ~spin_axis_rotated, color = ~pitch_type, colors = pitch_colors,
            marker = list(size = 8, opacity = 0.8),
            text = ~paste0("Type: ", pitch_type, "<br>Spin Axis: ", round(spin_axis, 1),
                           "<br>Spin Rate: ", round(spin_rate, 0), "<br>Tilt: ", tilt_label),
            hoverinfo = "text") %>%
      layout(
        polar  = list(angularaxis = list(rotation = 90, direction = "clockwise", tickmode = "array",
                                         tickvals = c(0, 90, 180, 270), ticktext = c("180°", "270°", "360°", "90°")),
                      radialaxis = list(title = "Spin Rate")),
        legend = list(orientation = "h", x = 0, y = -0.15),
        margin = list(l = 40, r = 40, t = 40, b = 80)
      )
  })
  
  make_time_plot <- function(data, metric, y_label) {
    d <- data %>% arrange(date) %>% mutate(pitch_index = row_number())
    p <- ggplot(d, aes(x = pitch_index, y = .data[[metric]], color = pitch_type, group = pitch_type,
                       text = paste0("Pitch #: ", pitch_index, "<br>Type: ", pitch_type,
                                     "<br>", y_label, ": ", round(.data[[metric]], 1)))) +
      geom_smooth(se = FALSE, linewidth = 1.3) +
      scale_color_manual(values = pitch_colors) + theme_minimal() +
      labs(x = "Pitch Sequence", y = y_label, color = "Pitch Type")
    ggplotly(p, tooltip = "text")
  }
  
  output$vbreak_time_plot  <- renderPlotly({ make_time_plot(pitcher_master(), "vert_break",         "V Break") })
  output$ivb_time_plot     <- renderPlotly({ make_time_plot(pitcher_master(), "induced_vert_break",  "Induced V Break") })
  output$hbreak_time_plot  <- renderPlotly({ make_time_plot(pitcher_master(), "horz_break",          "H Break") })
  
  # ============================================================
  # RELEASE POINT
  # ============================================================
  
  output$release_table <- renderDT({
    pitcher_master() %>%
      group_by(pitch_type) %>%
      summarise(
        `Pitch Type`      = first(pitch_type),
        Dispersion        = sd(rel_side, na.rm = TRUE) + sd(rel_height, na.rm = TRUE),
        `Avg Rel Ht`      = mean(rel_height,      na.rm = TRUE),
        `Avg Rel Side`    = mean(rel_side,         na.rm = TRUE),
        `Avg Ext`         = mean(extension,        na.rm = TRUE),
        `Avg V Rel Angle` = mean(vert_rel_angle,   na.rm = TRUE),
        `Avg H Rel Angle` = mean(horz_rel_angle,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$release_catcher_plot <- renderPlotly({
    p <- pitcher_master() %>%
      ggplot(aes(x = plate_loc_side, y = plate_loc_height, color = pitch_type,
                 text = paste0("Type: ", pitch_type,
                               "<br>Plate Side: ", round(plate_loc_side, 2),
                               "<br>Plate Height: ", round(plate_loc_height, 2),
                               "<br>Release Side: ", round(rel_side, 2),
                               "<br>Release Height: ", round(rel_height, 2)))) +
      geom_point(alpha = 0.8, size = 2.5) + add_strike_zone() + add_home_plate() +
      geom_segment(aes(x = -0.7, xend =  0,   y = 0.5,  yend = 0.85), color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0,   xend =  0.7, y = 0.85, yend = 0.5),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0.7, xend =  0.7, y = 0.5,  yend = 0.1),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0.7, xend = -0.7, y = 0.1,  yend = 0.1),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x = -0.7, xend = -0.7, y = 0.1,  yend = 0.5),  color = "black", inherit.aes = FALSE) +
      scale_color_manual(values = pitch_colors) +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5.5)) + theme_minimal() +
      labs(x = "Plate Loc Side", y = "Plate Loc Height", color = "Pitch Type")
    ggplotly(p, tooltip = "text")
  })
  
  output$release_back_plot <- renderPlotly({
    mound <- tibble(x = seq(-3.5, 3.5, length.out = 100)) %>% mutate(y = -0.12 * x^2 + 0.9)
    p <- pitcher_master() %>%
      ggplot(aes(x = rel_side, y = rel_height, color = pitch_type,
                 text = paste0("Type: ", pitch_type, "<br>Rel Side: ", round(rel_side, 2),
                               "<br>Rel Height: ", round(rel_height, 2), "<br>Extension: ", round(extension, 2)))) +
      geom_line(data = mound, aes(x = x, y = y), color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_rect(xmin = -0.5, xmax = 0.5, ymin = 0.35, ymax = 0.5,
                fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_point(alpha = 0.8, size = 2.5) + geom_vline(xintercept = 0, color = "gray70") +
      scale_color_manual(values = pitch_colors) +
      coord_fixed(xlim = c(-4, 4), ylim = c(0, 8)) + theme_minimal() +
      labs(x = "Release Side", y = "Release Height", color = "Pitch Type")
    ggplotly(p, tooltip = "text")
  })
  
  output$release_side_plot <- renderPlotly({
    mound <- tibble(x = seq(0, 8, length.out = 100)) %>% mutate(y = 0.85 - 0.12 * x)
    p <- pitcher_master() %>%
      ggplot(aes(x = extension, y = rel_height, color = pitch_type,
                 text = paste0("Type: ", pitch_type, "<br>Extension: ", round(extension, 2),
                               "<br>Rel Height: ", round(rel_height, 2), "<br>Rel Side: ", round(rel_side, 2)))) +
      geom_line(data = mound, aes(x = x, y = y), color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_rect(xmin = 0.05, xmax = 0.2, ymin = 0.35, ymax = 0.7,
                fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
      geom_point(alpha = 0.8, size = 2.5) + scale_color_manual(values = pitch_colors) +
      coord_fixed(xlim = c(0, 8), ylim = c(0, 8)) + theme_minimal() +
      labs(x = "Extension", y = "Release Height", color = "Pitch Type")
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # HEAT MAPS (pitcher + hitter) — registered with req() for lazy eval
  # ============================================================
  
  lapply(heatmap_metrics$id, function(metric_id) {
    local({
      m <- metric_id
      
      output[[paste0("pitcher_heat_", m)]] <- renderPlotly({
        req(pitcher_master())   # don't compute until data exists
        ggplotly(make_report_heatmap(pitcher_master(), m), tooltip = "text")
      })
      
      output[[paste0("hitter_heat_", m)]] <- renderPlotly({
        req(hitter_master())    # don't compute until data exists
        ggplotly(make_report_heatmap(hitter_master(), m), tooltip = "text")
      })
    })
  })
  
  # ============================================================
  # HITTER REPORTS
  # ============================================================
  
  output$hitter_batted_ball_review <- renderDT({
    d <- hitter_bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        Type         = first(pitch_type),
        `#`          = n(),
        EV           = mean(exit_speed,  na.rm = TRUE),
        `Peak EV`    = max(exit_speed,   na.rm = TRUE),
        LA           = mean(angle,       na.rm = TRUE),
        `Hard Hit LA` = mean(angle[hard_hit == 1], na.rm = TRUE),
        `Hard Hit %` = safe_rate(sum(hard_hit,   na.rm = TRUE), n()),
        `Damage %`   = safe_rate(sum(damage,     na.rm = TRUE), n()),
        `GB %`       = safe_rate(sum(ground_ball, na.rm = TRUE), n()),
        `LD %`       = safe_rate(sum(line_drive,  na.rm = TRUE), n()),
        `FB %`       = safe_rate(sum(fly_ball,    na.rm = TRUE), n()),
        `PU %`       = safe_rate(sum(pop_up,      na.rm = TRUE), n()),
        `GB/FB`      = safe_rate(sum(ground_ball, na.rm = TRUE), sum(fly_ball, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(across(c(`Hard Hit %`, `Damage %`, `GB %`, `LD %`, `FB %`, `PU %`), pct)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$hitter_batted_ball_field <- renderDT({
    d <- hitter_bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        Type      = first(pitch_type),
        `#`       = n(),
        `Pull %`  = safe_rate(sum(pull_side,    na.rm = TRUE), n()),
        `Pull EV` = mean(exit_speed[pull_side == 1],    na.rm = TRUE),
        `Pull LA` = mean(angle[pull_side == 1],         na.rm = TRUE),
        `Mid %`   = safe_rate(sum(middle_field,  na.rm = TRUE), n()),
        `Mid EV`  = mean(exit_speed[middle_field == 1], na.rm = TRUE),
        `Mid LA`  = mean(angle[middle_field == 1],       na.rm = TRUE),
        `Oppo %`  = safe_rate(sum(oppo_side,     na.rm = TRUE), n()),
        `Oppo EV` = mean(exit_speed[oppo_side == 1],   na.rm = TRUE),
        `Oppo LA` = mean(angle[oppo_side == 1],        na.rm = TRUE),
        `IFH %`   = safe_rate(sum(play_result == "Single" & distance < 120, na.rm = TRUE), n()),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(across(c(`Pull %`, `Mid %`, `Oppo %`, `IFH %`), pct)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$hitter_point_of_contact_table <- renderDT({
    d <- hitter_bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        Type              = first(pitch_type),
        `#`               = n(),
        `%`               = n() / nrow(d),
        `Contact Ht`      = mean(contact_position_z, na.rm = TRUE),
        `Contact Side`    = mean(contact_position_x, na.rm = TRUE),
        `Contact Depth`   = mean(contact_position_y, na.rm = TRUE),
        `Pull Cont Ht`    = mean(contact_position_z[pull_side == 1],    na.rm = TRUE),
        `Pull Cont Side`  = mean(contact_position_x[pull_side == 1],    na.rm = TRUE),
        `Pull Cont Depth` = mean(contact_position_y[pull_side == 1],    na.rm = TRUE),
        `Mid Cont Ht`     = mean(contact_position_z[middle_field == 1], na.rm = TRUE),
        `Mid Cont Side`   = mean(contact_position_x[middle_field == 1], na.rm = TRUE),
        `Mid Cont Depth`  = mean(contact_position_y[middle_field == 1], na.rm = TRUE),
        `Oppo Cont Ht`    = mean(contact_position_z[oppo_side == 1],    na.rm = TRUE),
        `Oppo Cont Side`  = mean(contact_position_x[oppo_side == 1],    na.rm = TRUE),
        `Oppo Cont Depth` = mean(contact_position_y[oppo_side == 1],    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(`%` = pct(`%`)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
    datatable(table, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$hitter_bb_location <- renderPlotly({
    p <- hitter_bb_data() %>%
      ggplot(aes(x = plate_loc_side, y = plate_loc_height, color = pitch_type)) +
      geom_point(size = 2.5, alpha = 0.85) +
      geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 1.2) +
      scale_color_manual(values = pitch_colors) +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5.5)) + theme_minimal() +
      labs(x = "Side at Plate", y = "Height at Plate", color = "Type")
    ggplotly(p)
  })
  
  hitter_make_spray <- function(d, color_var) {
    d <- prepare_spray_data(d)
    ggplot(d, aes(x = spray_x, y = spray_y, color = .data[[color_var]],
                  text = paste0("Pitch: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                "<br>LA: ", round(angle, 1), "<br>Bearing: ", round(spray_angle, 1),
                                "<br>Distance: ", round(distance, 1), "<br>Result: ", play_result))) +
      add_spray_field(d) + geom_point(size = 3, alpha = 0.9) +
      coord_fixed(xlim = c(-325, 325), ylim = c(-20, 450)) +
      theme_void() + theme(legend.position = "top") + labs(color = color_var)
  }
  
  output$hitter_spray_result         <- renderPlotly({ ggplotly(hitter_make_spray(hitter_bb_data(), "play_result"),   tooltip = "text") })
  output$hitter_spray_chart_result   <- renderPlotly({ ggplotly(hitter_make_spray(hitter_bb_data(), "play_result"),   tooltip = "text") })
  output$hitter_spray_chart_contact  <- renderPlotly({
    d <- hitter_bb_data() %>% mutate(contact_type = case_when(
      ground_ball == 1 ~ "GB", line_drive == 1 ~ "LD",
      fly_ball    == 1 ~ "FB", pop_up     == 1 ~ "PU", TRUE ~ "Other"))
    ggplotly(hitter_make_spray(d, "contact_type"), tooltip = "text")
  })
  
  output$hitter_spray_chart_ev <- renderPlotly({
    d <- prepare_spray_data(hitter_bb_data())
    p <- ggplot(d, aes(x = spray_x, y = spray_y, color = exit_speed,
                       text = paste0("Pitch: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                     "<br>LA: ", round(angle, 1), "<br>Bearing: ", round(spray_angle, 1),
                                     "<br>Distance: ", round(distance, 1), "<br>Result: ", play_result))) +
      add_spray_field(d) + geom_point(size = 3, alpha = 0.9) + ev_scale +
      coord_fixed(xlim = c(-325, 325), ylim = c(-20, 450)) +
      theme_void() + theme(legend.position = "top") + labs(color = "EV")
    ggplotly(p, tooltip = "text")
  })
  
  output$hitter_bb_heat_ev  <- renderPlotly({ ggplotly(make_bb_heat(hitter_bb_data(), "avg_ev"), tooltip = "text") })
  output$hitter_catcher_view <- renderPlotly({ ggplotly(make_bb_color_plot(hitter_bb_data(), "exit_speed", "catcher"), tooltip = "text") })
  output$hitter_top_view     <- renderPlotly({ ggplotly(make_bb_color_plot(hitter_bb_data(), "exit_speed", "top"),     tooltip = "text") })
  output$hitter_side_view    <- renderPlotly({ ggplotly(make_bb_color_plot(hitter_bb_data(), "exit_speed", "side"),    tooltip = "text") })
  
  output$hitter_bb_histogram <- renderPlotly({
    d <- hitter_bb_data()
    if (input$hitter_bb_hist_filter != "all") d <- d %>% filter(.data[[input$hitter_bb_hist_filter]] == 1)
    metric    <- input$hitter_bb_hist_metric
    bin_width <- as.numeric(input$hitter_bb_bin_width)
    if (input$hitter_bb_hist_color == "one") {
      p <- d %>% ggplot(aes(x = .data[[metric]])) +
        geom_histogram(binwidth = bin_width, fill = "#003F66", color = "white") +
        theme_minimal() + labs(x = metric, y = "Batted Balls")
    } else {
      p <- d %>% ggplot(aes(x = .data[[metric]], fill = .data[[input$hitter_bb_hist_color]])) +
        geom_histogram(binwidth = bin_width, color = "white") +
        theme_minimal() + labs(x = metric, y = "Batted Balls", fill = input$hitter_bb_hist_color)
    }
    ggplotly(p)
  })
  
  # ============================================================
  # PLATE DISCIPLINE
  # ============================================================
  
  output$hitter_poc_location <- renderPlotly({
    p <- hitter_master() %>%
      ggplot(aes(x = plate_loc_side, y = plate_loc_height, color = pitch_type,
                 text = paste0("Pitch: ", pitch_type, "<br>Call: ", pitch_call,
                               "<br>Result: ", play_result, "<br>Count: ", balls, "-", strikes))) +
      geom_point(size = 2.4, alpha = 0.75) + add_strike_zone() + add_home_plate() +
      geom_segment(aes(x = -0.7, xend =  0,   y = 0.5,  yend = 0.85), color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0,   xend =  0.7, y = 0.85, yend = 0.5),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0.7, xend =  0.7, y = 0.5,  yend = 0.1),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0.7, xend = -0.7, y = 0.1,  yend = 0.1),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x = -0.7, xend = -0.7, y = 0.1,  yend = 0.5),  color = "black", inherit.aes = FALSE) +
      scale_color_manual(values = pitch_colors) +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5.5)) + theme_minimal() +
      labs(x = "Plate Loc Side", y = "Plate Loc Height", color = "Type")
    ggplotly(p, tooltip = "text")
  })
  
  output$hitter_poc_spray <- renderPlotly({ ggplotly(hitter_make_spray(hitter_bb_data(), "play_result"), tooltip = "text") })
  
  output$hitter_poc_contour <- renderPlot({
    d <- hitter_master()
    if (input$hitter_poc_contour_filter != "all") d <- d %>% filter(.data[[input$hitter_poc_contour_filter]] == 1)
    ggplot(d, aes(x = plate_loc_side, y = plate_loc_height)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.65, contour = TRUE) +
      add_strike_zone() + add_home_plate() +
      geom_segment(aes(x = -0.7, xend =  0,   y = 0.5,  yend = 0.85), color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0,   xend =  0.7, y = 0.85, yend = 0.5),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0.7, xend =  0.7, y = 0.5,  yend = 0.1),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x =  0.7, xend = -0.7, y = 0.1,  yend = 0.1),  color = "black", inherit.aes = FALSE) +
      geom_segment(aes(x = -0.7, xend = -0.7, y = 0.1,  yend = 0.5),  color = "black", inherit.aes = FALSE) +
      scale_fill_gradient(low = "#D7E7FF", high = "#D62828") +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5.5)) + theme_minimal() +
      theme(legend.position = "none") + labs(x = "Plate Loc Side", y = "Plate Loc Height")
  })
  
  output$hitter_plate_discipline_table <- renderDT({
    d <- hitter_master()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        Type              = first(pitch_type),
        `#`               = n(),
        `Swing %`         = mean(swing,  na.rm = TRUE),
        `Swinging Strike %` = mean(whiff, na.rm = TRUE),
        `Whiff %`         = safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
        `Zone Swing %`    = safe_rate(sum(zone_swing,    na.rm = TRUE), sum(in_zone == 1, na.rm = TRUE)),
        `Zone Contact %`  = safe_rate(sum(zone_contact,  na.rm = TRUE), sum(zone_swing,   na.rm = TRUE)),
        `Chase Swing %`   = safe_rate(sum(chase_swing,   na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE)),
        `Chase Contact %` = safe_rate(sum(chase_contact, na.rm = TRUE), sum(chase_swing,  na.rm = TRUE)),
        `Edge %`          = mean(zone_bucket == "Shadow", na.rm = TRUE),
        `Strike %`        = mean(strike, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(across(c(`Swing %`, `Swinging Strike %`, `Whiff %`, `Zone Swing %`, `Zone Contact %`,
                      `Chase Swing %`, `Chase Contact %`, `Edge %`, `Strike %`), pct)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
    
    player_row <- d %>%
      summarise(
        Type              = "Player",
        `#`               = n(),
        `Swing %`         = pct(mean(swing,  na.rm = TRUE)),
        `Swinging Strike %` = pct(mean(whiff, na.rm = TRUE)),
        `Whiff %`         = pct(safe_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE))),
        `Zone Swing %`    = pct(safe_rate(sum(zone_swing,    na.rm = TRUE), sum(in_zone == 1, na.rm = TRUE))),
        `Zone Contact %`  = pct(safe_rate(sum(zone_contact,  na.rm = TRUE), sum(zone_swing,   na.rm = TRUE))),
        `Chase Swing %`   = pct(safe_rate(sum(chase_swing,   na.rm = TRUE), sum(in_zone == 0, na.rm = TRUE))),
        `Chase Contact %` = pct(safe_rate(sum(chase_contact, na.rm = TRUE), sum(chase_swing,  na.rm = TRUE))),
        `Edge %`          = pct(mean(zone_bucket == "Shadow", na.rm = TRUE)),
        `Strike %`        = pct(mean(strike, na.rm = TRUE))
      )
    
    bind_rows(table, player_row) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
  })
  
  # ============================================================
  # POINT OF CONTACT
  # ============================================================
  
  output$hitter_poc_catcher_view <- renderPlotly({ ggplotly(make_bb_color_plot(hitter_bb_data(), input$hitter_poc_catcher_color, "catcher"), tooltip = "text") })
  output$hitter_poc_top_view     <- renderPlotly({ ggplotly(make_bb_color_plot(hitter_bb_data(), input$hitter_poc_top_color,     "top"),     tooltip = "text") })
  output$hitter_poc_side_view    <- renderPlotly({ ggplotly(make_bb_color_plot(hitter_bb_data(), input$hitter_poc_side_color,    "side"),    tooltip = "text") })
  output$hitter_poc_spray_result  <- renderPlotly({ ggplotly(hitter_make_spray(hitter_bb_data(), "play_result"),   tooltip = "text") })
  output$hitter_poc_spray_contact <- renderPlotly({
    d <- hitter_bb_data() %>% mutate(contact_type = case_when(
      ground_ball == 1 ~ "GB", line_drive == 1 ~ "LD",
      fly_ball    == 1 ~ "FB", pop_up     == 1 ~ "PU", TRUE ~ "Other"))
    ggplotly(hitter_make_spray(d, "contact_type"), tooltip = "text")
  })
  
  output$hitter_poc_spray_ev <- renderPlotly({
    d <- prepare_spray_data(hitter_bb_data())
    p <- ggplot(d, aes(x = spray_x, y = spray_y, color = exit_speed,
                       text = paste0("Pitch: ", pitch_type, "<br>EV: ", round(exit_speed, 1),
                                     "<br>LA: ", round(angle, 1), "<br>Bearing: ", round(spray_angle, 1),
                                     "<br>Distance: ", round(distance, 1), "<br>Result: ", play_result))) +
      add_spray_field(d) + geom_point(size = 3, alpha = 0.9) + ev_scale +
      coord_fixed(xlim = c(-325, 325), ylim = c(-20, 450)) +
      theme_void() + theme(legend.position = "top") + labs(color = "EV")
    ggplotly(p, tooltip = "text")
  })
  
  make_hitter_poc_table <- function(d) {
    d %>%
      group_by(pitch_type) %>%
      summarise(
        Type           = first(pitch_type),
        `#`            = n(),
        `%`            = n() / nrow(d),
        `Contact Ht`   = mean(contact_position_z, na.rm = TRUE),
        `Contact Side` = mean(contact_position_x, na.rm = TRUE),
        `Contact Depth` = mean(contact_position_y, na.rm = TRUE),
        EV             = mean(exit_speed,  na.rm = TRUE),
        LA             = mean(angle,       na.rm = TRUE),
        Distance       = mean(distance,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(`%` = pct(`%`), across(where(is.numeric), ~ round(.x, 2)))
  }
  
  output$hitter_poc_main_table   <- renderDT({ datatable(make_hitter_poc_table(hitter_bb_data()),                              options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) })
  output$hitter_poc_pull_table   <- renderDT({ datatable(make_hitter_poc_table(hitter_bb_data() %>% filter(pull_side == 1)),   options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) })
  output$hitter_poc_middle_table <- renderDT({ datatable(make_hitter_poc_table(hitter_bb_data() %>% filter(middle_field == 1)), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) })
  output$hitter_poc_oppo_table   <- renderDT({ datatable(make_hitter_poc_table(hitter_bb_data() %>% filter(oppo_side == 1)),   options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) })
  
  # ============================================================
  # BLAST MOTION
  # ============================================================
  
  output$hitter_blast_profile_table <- renderDT({
    d <- hitter_bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        Type         = first(pitch_type),
        `Avg EV`     = mean(exit_speed,  na.rm = TRUE),
        `Peak EV`    = max(exit_speed,   na.rm = TRUE),
        `Avg LA`     = mean(angle,       na.rm = TRUE),
        `Peak Dist`  = max(distance,     na.rm = TRUE),
        `Hard Hit %` = safe_rate(sum(hard_hit,   na.rm = TRUE), n()),
        `Damage %`   = safe_rate(sum(damage,     na.rm = TRUE), n()),
        `GB %`       = safe_rate(sum(ground_ball, na.rm = TRUE), n()),
        `LD %`       = safe_rate(sum(line_drive,  na.rm = TRUE), n()),
        `FB %`       = safe_rate(sum(fly_ball,    na.rm = TRUE), n()),
        .groups = "drop"
      ) %>% select(-pitch_type)
    
    total_row <- d %>%
      summarise(
        Type         = "Total",
        `Avg EV`     = mean(exit_speed,  na.rm = TRUE),
        `Peak EV`    = max(exit_speed,   na.rm = TRUE),
        `Avg LA`     = mean(angle,       na.rm = TRUE),
        `Peak Dist`  = max(distance,     na.rm = TRUE),
        `Hard Hit %` = safe_rate(sum(hard_hit,   na.rm = TRUE), n()),
        `Damage %`   = safe_rate(sum(damage,     na.rm = TRUE), n()),
        `GB %`       = safe_rate(sum(ground_ball, na.rm = TRUE), n()),
        `LD %`       = safe_rate(sum(line_drive,  na.rm = TRUE), n()),
        `FB %`       = safe_rate(sum(fly_ball,    na.rm = TRUE), n())
      )
    
    bind_rows(table, total_row) %>%
      mutate(across(c(`Hard Hit %`, `Damage %`, `GB %`, `LD %`, `FB %`), pct),
             across(where(is.numeric), ~ round(.x, 1))) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$hitter_blast_field_table <- renderDT({
    d <- hitter_bb_data()
    table <- d %>%
      group_by(pitch_type) %>%
      summarise(
        Type       = first(pitch_type),
        `Pull %`   = safe_rate(sum(pull_side,    na.rm = TRUE), n()),
        `AEV Pull` = mean(exit_speed[pull_side == 1],    na.rm = TRUE),
        `ALA Pull` = mean(angle[pull_side == 1],         na.rm = TRUE),
        `Mid %`    = safe_rate(sum(middle_field,  na.rm = TRUE), n()),
        `AEV Mid`  = mean(exit_speed[middle_field == 1], na.rm = TRUE),
        `ALA Mid`  = mean(angle[middle_field == 1],       na.rm = TRUE),
        `Oppo %`   = safe_rate(sum(oppo_side,     na.rm = TRUE), n()),
        `AEV Oppo` = mean(exit_speed[oppo_side == 1],   na.rm = TRUE),
        `ALA Oppo` = mean(angle[oppo_side == 1],        na.rm = TRUE),
        .groups = "drop"
      ) %>% select(-pitch_type)
    
    total_row <- d %>%
      summarise(
        Type       = "Total",
        `Pull %`   = safe_rate(sum(pull_side,    na.rm = TRUE), n()),
        `AEV Pull` = mean(exit_speed[pull_side == 1],    na.rm = TRUE),
        `ALA Pull` = mean(angle[pull_side == 1],         na.rm = TRUE),
        `Mid %`    = safe_rate(sum(middle_field,  na.rm = TRUE), n()),
        `AEV Mid`  = mean(exit_speed[middle_field == 1], na.rm = TRUE),
        `ALA Mid`  = mean(angle[middle_field == 1],       na.rm = TRUE),
        `Oppo %`   = safe_rate(sum(oppo_side,     na.rm = TRUE), n()),
        `AEV Oppo` = mean(exit_speed[oppo_side == 1],   na.rm = TRUE),
        `ALA Oppo` = mean(angle[oppo_side == 1],        na.rm = TRUE)
      )
    
    bind_rows(table, total_row) %>%
      mutate(across(c(`Pull %`, `Mid %`, `Oppo %`), pct),
             across(where(is.numeric), ~ round(.x, 1))) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # ============================================================
  # GAME STRATEGY
  # ============================================================
  
  output$strategy_team_selector <- renderUI({
    req(input$strategy_year)
    
    teams <- get_master_for_year(input$strategy_year) %>%
      summarise(team = list(sort(unique(c(pitcher_team_name, batter_team_name))))) %>%
      pull(team) %>%
      unlist()
    
    selectInput("strategy_team", "Opponent / Team:",
                choices = teams,
                selected = teams[1])
  })
  
  output$strategy_pitcher_selector <- renderUI({
    req(input$strategy_year, input$strategy_team)
    
    pitchers <- get_master_for_year(input$strategy_year) %>%
      filter(
        pitcher_team_name == input$strategy_team
      ) %>%
      distinct(pitcher_name) %>%
      arrange(pitcher_name) %>%
      pull(pitcher_name)
    
    selectizeInput(
      "strategy_pitchers",
      "Pitchers:",
      choices = pitchers,
      selected = pitchers,
      multiple = TRUE
    )
  })
  
  output$strategy_hitter_selector <- renderUI({
    req(input$strategy_year, input$strategy_team)
    
    hitters <- get_master_for_year(input$strategy_year) %>%
      filter(
        batter_team_name == input$strategy_team
      ) %>%
      distinct(batter_name) %>%
      arrange(batter_name) %>%
      pull(batter_name)
    
    selectizeInput(
      "strategy_hitters",
      "Hitters:",
      choices = hitters,
      selected = hitters,
      multiple = TRUE
    )
  })
  
    make_pitcher_strategy_table <- function(pitcher_name_selected) {
      d <- get_master_for_year(input$strategy_year) %>%
        filter(
          pitcher_team_name == input$strategy_team,
          pitcher_name == pitcher_name_selected
        )
    
    total_pitches <- nrow(d)
    
    d %>%
      group_by(pitch_type) %>%
      summarise(
        `Pitch Type`     = first(pitch_type),
        `Total Usage %`  = n() / total_pitches,
        
        `LHH Usage %`    = safe_rate(
          sum(batter_side_clean == "Left", na.rm = TRUE),
          sum(d$batter_side_clean == "Left", na.rm = TRUE)
        ),
        
        `RHH Usage %`    = safe_rate(
          sum(batter_side_clean == "Right", na.rm = TRUE),
          sum(d$batter_side_clean == "Right", na.rm = TRUE)
        ),
        
        Velo             = mean(rel_speed, na.rm = TRUE),
        `Spin Rate`      = mean(spin_rate, na.rm = TRUE),
        HB               = mean(horz_break, na.rm = TRUE),
        VB               = mean(vert_break, na.rm = TRUE),
        IVB              = mean(induced_vert_break, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(-pitch_type) %>%
      mutate(
        across(c(`Total Usage %`, `LHH Usage %`, `RHH Usage %`), pct),
        across(where(is.numeric), ~ round(.x, 1))
      )
  }
  
    make_pitcher_fps <- function(pitcher_name_selected) {
      d <- get_master_for_year(input$strategy_year) %>%
        filter(
          pitcher_team_name == input$strategy_team,
          pitcher_name == pitcher_name_selected,
          first_pitch == 1
        )
      
      pct(mean(d$strike, na.rm = TRUE))
    }
  
  strategy_report_inputs <- eventReactive(input$run_strategy_reports, {
    list(
      year = input$strategy_year,
      team = input$strategy_team,
      pitchers = input$strategy_pitchers,
      hitters = input$strategy_hitters
    )
  })
  
  output$pitcher_strategy_report <- renderUI({
    strategy_inputs <- strategy_report_inputs()
    req(strategy_inputs$pitchers)
    
    div(class = "pitcher-print-grid",
        lapply(strategy_inputs$pitchers, function(p) {
          table_id <- paste0("strategy_pitcher_table_", make.names(p))
          output[[table_id]] <- renderDT({
            datatable(make_pitcher_strategy_table(p),
                      class = "compact stripe strategy-table",
                      options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = FALSE,
                                     autoWidth = FALSE, columnDefs = list(list(className = "dt-center", targets = "_all"))),
                      rownames = FALSE)
          })
          div(class = "pitcher-print-card",
              div(class = "report-page strategy-report-card",
                  div(class = "report-player-title", p),
                  div(class = "report-subtitle", paste0("First Pitch Strike %: ", make_pitcher_fps(p), "%")),
                  DTOutput(table_id)
              )
          )
        })
    )
  })
  
  make_hitter_first_pitch_swing <- function(hitter_name_selected) {
    d <- get_master_for_year(input$strategy_year) %>%
      filter(
        batter_team_name == input$strategy_team,
        batter_name == hitter_name_selected,
        first_pitch == 1
      )

    pct(mean(d$swing, na.rm = TRUE))
}
  
  make_strategy_spray_plot <- function(hitter_name_selected) {
    d <- get_master_for_year(input$strategy_year) %>%
      filter(
        batter_team_name == input$strategy_team,
        batter_name == hitter_name_selected,
        has_batted_ball == 1
      ) %>%
      mutate(pitch_family = get_pitch_family(pitch_type)) %>%
      prepare_spray_data()
    
    ggplot(d, aes(x = spray_x, y = spray_y, color = pitch_family)) +
      add_spray_field(d) +
      geom_point(size = 3, alpha = 0.85) +
      scale_color_manual(values = pitch_family_colors) +
      coord_fixed(xlim = c(-325, 325), ylim = c(-20, 450)) +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)
      ) +
      labs(color = "Pitch Group")
  }
  
  output$hitter_strategy_report <- renderUI({
    strategy_inputs <- strategy_report_inputs()
    req(strategy_inputs$hitters)
    fluidRow(
      lapply(strategy_inputs$hitters, function(h) {
        plot_id <- paste0("strategy_hitter_spray_", make.names(h))
        output[[plot_id]] <- renderPlot({ make_strategy_spray_plot(h) })
        column(width = 3, class = "hitter-strategy-report-col",
               div(class = "report-page strategy-report-card",
                   div(class = "report-player-title", h),
                   div(class = "report-subtitle", paste0("First Pitch Swing %: ", make_hitter_first_pitch_swing(h), "%")),
                   plotOutput(plot_id, height = "300px")
               )
        )
      })
    )
  })
  
  output$strategy_print_team <- renderText({ paste("Game Strategy Report:", input$strategy_team) })
  
  # ============================================================
  # DEVELOPMENT PLANS
  # ============================================================
  
  output$dev_team_selector <- renderUI({
    req(input$dev_year, input$dev_player_type)
    
    if (input$dev_player_type == "Pitcher") {
      teams <- get_master_for_year(input$dev_year) %>%
        distinct(pitcher_team_name) %>%
        arrange(pitcher_team_name) %>%
        pull(pitcher_team_name)
      
    } else {
      
      teams <- get_master_for_year(input$dev_year) %>%
        distinct(batter_team_name) %>%
        arrange(batter_team_name) %>%
        pull(batter_team_name)
    }
    
    selectInput(
      "dev_team",
      "Team:",
      choices = teams,
      selected = teams[1]
    )
  })
  
  output$dev_player_selector <- renderUI({
    req(input$dev_year, input$dev_player_type, input$dev_team)
    
    if (input$dev_player_type == "Pitcher") {
      
      players <- get_master_for_year(input$dev_year) %>%
        filter(
          pitcher_team_name == input$dev_team
        ) %>%
        distinct(pitcher_name) %>%
        arrange(pitcher_name) %>%
        pull(pitcher_name)
      
    } else {
      
      players <- get_master_for_year(input$dev_year) %>%
        filter(
          batter_team_name == input$dev_team
        ) %>%
        distinct(batter_name) %>%
        arrange(batter_name) %>%
        pull(batter_name)
      
    }
    
    selectInput(
      "dev_player",
      "Player:",
      choices = players,
      selected = players[1]
    )
  })
  
  dev_player_data <- reactive({
    req(input$dev_year, input$dev_player, input$dev_team, input$dev_player_type)
    
    if (input$dev_player_type == "Pitcher") {
      get_master_for_year(input$dev_year) %>%
        filter(
          pitcher_team_name == input$dev_team,
          pitcher_name == input$dev_player
        )
    } else {
      get_master_for_year(input$dev_year) %>%
        filter(
          batter_team_name == input$dev_team,
          batter_name == input$dev_player
        )
    }
  }) %>% bindCache(input$dev_year, input$dev_player, input$dev_team, input$dev_player_type)
  
  output$dev_metric_selector <- renderUI({
    pitcher_metrics <- c("Velocity" = "rel_speed", "Spin Rate" = "spin_rate", "Horizontal Break" = "horz_break",
                         "Vertical Break" = "vert_break", "Induced Vertical Break" = "induced_vert_break",
                         "Strike %" = "strike", "Zone %" = "in_zone", "Whiff %" = "whiff", "Chase Swing %" = "chase_swing")
    hitter_metrics  <- c("Exit Velocity" = "exit_speed", "Launch Angle" = "angle", "Hard Hit %" = "hard_hit",
                         "Damage %" = "damage", "Swing %" = "swing", "Whiff %" = "whiff",
                         "Chase Swing %" = "chase_swing", "Zone Contact %" = "zone_contact")
    choices <- if (input$dev_player_type == "Pitcher") pitcher_metrics else hitter_metrics
    selectInput("dev_metric", "Metric:", choices = choices, selected = choices[1])
  })
  
  observeEvent(input$save_dev_note, {
    req(input$dev_player, input$dev_team)
    current_notes <- read_csv(notes_path, show_col_types = FALSE)
    new_note <- tibble(
      note_id         = paste0("note_", as.numeric(Sys.time())),
      date            = input$dev_note_date,
      player_type     = input$dev_player_type,
      team            = input$dev_team,
      player          = input$dev_player,
      focus_area      = input$dev_focus_area,
      priority        = input$dev_priority,
      status          = input$dev_status,
      observation     = input$dev_observation,
      adjustment_plan = input$dev_adjustment,
      follow_up       = input$dev_follow_up
    )
    write_csv(bind_rows(current_notes, new_note), notes_path)
    showNotification("Development note saved.", type = "message")
  })
  
  dev_notes <- reactive({
    input$save_dev_note
    read_csv(notes_path, show_col_types = FALSE) %>% mutate(date = as.Date(date))
  })
  
  output$dev_player_notes_table <- renderDT({
    req(input$dev_player)
    dev_notes() %>%
      filter(player_type == input$dev_player_type, team == input$dev_team, player == input$dev_player) %>%
      arrange(desc(date)) %>%
      select(Date = date, `Focus Area` = focus_area, Priority = priority, Status = status,
             Observation = observation, `Adjustment Plan` = adjustment_plan, `Follow-Up` = follow_up) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 8), rownames = FALSE)
  })
  
  output$dev_all_notes_table <- renderDT({
    dev_notes() %>%
      arrange(desc(date)) %>%
      select(Date = date, Type = player_type, Team = team, Player = player, `Focus Area` = focus_area,
             Priority = priority, Status = status, Observation = observation,
             `Adjustment Plan` = adjustment_plan, `Follow-Up` = follow_up) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
  })
  
  make_dev_metric_daily <- reactive({
    req(input$dev_metric)
    dev_player_data() %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(date), !is.na(.data[[input$dev_metric]])) %>%
      group_by(date) %>%
      summarise(metric_value = mean(.data[[input$dev_metric]], na.rm = TRUE), pitches = n(), .groups = "drop") %>%
      arrange(date) %>%
      mutate(game_number = row_number(), rolling_avg = zoo::rollmean(metric_value, k = 3, fill = NA, align = "right"))
  }) %>% bindCache(input$dev_player, input$dev_team, input$dev_player_type, input$dev_metric)
  
  output$dev_metric_forecast_plot <- renderPlotly({
    req(input$dev_metric)
    daily <- make_dev_metric_daily()
    validate(need(nrow(daily) >= 3, "Need at least 3 dates/games to create a trend and forecast."))
    
    forecast_games <- as.numeric(input$dev_forecast_games)
    model          <- lm(metric_value ~ game_number, data = daily)
    future_df      <- tibble(game_number = max(daily$game_number) + seq_len(forecast_games))
    future_pred    <- predict(model, newdata = future_df, interval = "confidence") %>% as_tibble()
    forecast_df    <- bind_cols(future_df, future_pred) %>%
      mutate(date = max(daily$date) + seq_len(forecast_games), type = "Forecast")
    
    metric_label <- names(which(c(
      "Velocity" = "rel_speed", "Spin Rate" = "spin_rate", "Horizontal Break" = "horz_break",
      "Vertical Break" = "vert_break", "Induced Vertical Break" = "induced_vert_break",
      "Strike %" = "strike", "Zone %" = "in_zone", "Whiff %" = "whiff", "Chase Swing %" = "chase_swing",
      "Exit Velocity" = "exit_speed", "Launch Angle" = "angle", "Hard Hit %" = "hard_hit",
      "Damage %" = "damage", "Zone Contact %" = "zone_contact"
    ) == input$dev_metric))[1]
    
    p <- ggplot() +
      geom_point(data = daily, aes(x = game_number, y = metric_value,
                                   text = paste0("Date: ", date, "<br>Value: ", round(metric_value, 2), "<br>Pitches: ", pitches)),
                 size = 3, alpha = 0.85, color = dark_teal) +
      geom_line(data = daily,      aes(x = game_number, y = metric_value),      linewidth = 1,   color = dark_teal) +
      geom_line(data = daily,      aes(x = game_number, y = rolling_avg),       linewidth = 1.1, linetype = "dashed", color = baseball_red, na.rm = TRUE) +
      geom_ribbon(data = forecast_df, aes(x = game_number, ymin = lwr, ymax = upr), alpha = 0.18, fill = "#00A3A3") +
      geom_line(data = forecast_df,   aes(x = game_number, y = fit),            linewidth = 1.2, linetype = "dashed", color = "#003F66") +
      geom_point(data = forecast_df,  aes(x = game_number, y = fit,
                                          text = paste0("Forecast Date: ", date, "<br>Forecast: ", round(fit, 2),
                                                        "<br>Lower: ", round(lwr, 2), "<br>Upper: ", round(upr, 2))),
                 size = 3, color = "#003F66") +
      geom_vline(xintercept = max(daily$game_number[daily$date <= as.Date(input$dev_adjustment_date)], na.rm = TRUE),
                 linetype = "dotted", linewidth = 1, color = "black") +
      theme_minimal() +
      labs(title   = paste(input$dev_player, "-", metric_label, "Progress + Forecast"),
           x       = "Game / Date Order",
           y       = metric_label,
           caption = "Dashed red = 3-game rolling average | Dashed blue = forecast")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$dev_before_after_table <- renderDT({
    req(input$dev_metric, input$dev_adjustment_date)
    daily  <- make_dev_metric_daily()
    before <- daily %>% filter(date <  as.Date(input$dev_adjustment_date))
    after  <- daily %>% filter(date >= as.Date(input$dev_adjustment_date))
    
    before_avg  <- mean(before$metric_value, na.rm = TRUE)
    after_avg   <- mean(after$metric_value,  na.rm = TRUE)
    change      <- after_avg - before_avg
    pct_change  <- safe_rate(change, before_avg)
    trend_model <- lm(metric_value ~ game_number, data = daily)
    slope       <- coef(trend_model)[["game_number"]]
    trend_label <- case_when(slope > 0 ~ "Increasing", slope < 0 ~ "Decreasing", TRUE ~ "Flat")
    
    tibble(
      Metric                = input$dev_metric,
      `Adjustment Date`     = as.Date(input$dev_adjustment_date),
      `Before Avg`          = before_avg,
      `After Avg`           = after_avg,
      Change                = change,
      `% Change`            = pct_change,
      `Trend Slope Per Game` = slope,
      `Trend Direction`     = trend_label
    ) %>%
      mutate(`% Change` = pct(`% Change`),
             across(c(`Before Avg`, `After Avg`, Change, `Trend Slope Per Game`), ~ round(.x, 2))) %>%
      datatable(options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
}

# -----------------------------
# RUN APP
# -----------------------------

shinyApp(ui, server)
