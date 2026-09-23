library(dplyr)
library(tidyr)

all_inplay_clean <- all_inplay %>%
  select(
    Batter,
    BatterSide,
    AutoPitchType,
    SpinRate,
    ExitSpeed,
    Distance,
    Angle,
    Date
  )

all_inplay_clean <- all_inplay_clean %>%
  rename(
    batter = Batter,
    handedness = BatterSide,
    pitch_type = AutoPitchType,
    spin_rate = SpinRate,
    exit_speed = ExitSpeed,
    hit_distance = Distance,
    launch_angle = Angle,
    date = Date
  )

## Handiness ##
all_inplay_clean$handedness <- ifelse(
  all_inplay_clean$handedness == "Right",
  "RHH",
  "LHH"
)

all_inplay_clean$handedness <- factor(all_inplay_clean$handedness)

## Spin Tier ##
all_inplay_clean$spin_tier <- cut(
  all_inplay_clean$spin_rate,
  breaks = quantile(
    all_inplay_clean$spin_rate,
    probs = c(0, 1/3, 2/3, 1),
    na.rm = TRUE
  ),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

write.csv(all_inplay_clean, "all_inplay_clean.csv", row.names = FALSE)

