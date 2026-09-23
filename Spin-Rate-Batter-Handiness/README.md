# Effects of Spin Rate and Batter Handedness on Exit Velocity

**ANOVA | Statistical Research | Baseball Analytics | Pioneer League**

## Project Overview

This project investigates whether pitch spin rate is associated with batted-ball exit velocity and whether that relationship differs between left-handed and right-handed hitters.

Using pitch-tracking data from the 2025 Pioneer League season, I analyzed more than 31,000 batted balls to evaluate the relationship between spin-rate tiers, batter handedness, and exit velocity.

The project uses both one-way and repeated-measures ANOVA techniques to distinguish statistical significance from practical baseball significance.

## Research Questions

This analysis focused on three primary questions:

1. Does mean exit velocity differ across low, medium, and high spin-rate tiers?
2. Does mean exit velocity differ between left-handed and right-handed hitters?
3. Does the relationship between spin rate and exit velocity depend on batter handedness?

## Dataset

The analysis used Pioneer League pitch-tracking data from the 2025 season collected using TrackMan V3 stadium systems.

Game-level CSV files from approximately 480 games were combined and cleaned to retain batted-ball observations relevant to the analysis.

After data preparation, the primary dataset contained **31,503 usable observations**.

Key variables included:

- **Spin Rate** — pitch spin rate measured in RPM
- **Spin Tier** — Low, Medium, or High based on spin-rate quantiles
- **Batter Handedness** — Left-handed (LHH) or right-handed (RHH)
- **Exit Velocity** — speed of the batted ball in mph
- **Batter ID** — used to identify repeated observations by hitter

> **Note:** The underlying Pioneer League tracking dataset is not included in this repository.

## Data Preparation

The raw pitch-tracking data was cleaned and transformed in R.

The preparation process included:

- Selecting relevant pitch and batted-ball variables
- Standardizing variable names
- Converting batter handedness into LHH and RHH categories
- Creating Low, Medium, and High spin-rate tiers using quantile-based cutoffs
- Removing observations without the necessary batted-ball information
- Creating analysis-ready datasets for the ANOVA procedures

For the repeated-measures analysis, the dataset was restricted to hitters with observations in each spin-rate tier.

## Statistical Analysis

### One-Way ANOVA

A one-way ANOVA was used to test whether mean exit velocity differed across the three spin-rate tiers.

The analysis found a statistically significant difference:

**F(2, 31,500) = 12.13, p < .001**

Post-hoc comparisons using Tukey's HSD indicated that the medium-spin group produced higher mean exit velocity than the high-spin group, while the observed differences between groups remained relatively small.

### Repeated-Measures ANOVA

A repeated-measures two-way ANOVA was used to examine:

- Batter handedness
- Spin-rate tier
- The interaction between handedness and spin tier

The model treated spin tier as a within-subject factor and handedness as a between-subject factor.

The analysis found no statistically significant main effect of handedness or spin tier within the repeated-measures sample.

However, the **Spin Tier × Batter Handedness interaction was statistically significant**:

**F(2, 650) = 4.66, p = .0098**

This indicates that the relationship between spin tier and exit velocity differed between left-handed and right-handed hitters within the analyzed sample.

### Assumption Testing

Mauchly's test indicated a violation of the sphericity assumption.

Greenhouse-Geisser and Huynh-Feldt corrections were therefore examined. The Spin Tier × Handedness interaction remained statistically significant after correction.

## Key Findings

The analysis produced two important findings:

- Exit velocity differed statistically across spin-rate tiers in the full dataset.
- Batter handedness interacted with spin-rate tier in the repeated-measures analysis.

However, the magnitude of the observed exit-velocity differences was small.

Mean exit velocities across groups generally differed by less than approximately **1 mph**, suggesting that statistically significant results did not necessarily translate into meaningful differences in game performance.

This distinction between **statistical significance and practical significance** was an important part of the analysis.

## Baseball Application

Spin rate is widely used in pitch design and pitcher development, but these results suggest that spin rate should not be evaluated in isolation when considering contact quality.

The observed effects on exit velocity were relatively small, and additional variables such as pitch type, pitch location, pitcher characteristics, and hitter skill could provide greater context.

From a player-development perspective, the analysis demonstrates why statistically significant tracking-data relationships should also be evaluated for their practical baseball impact before being translated into coaching recommendations.

## Limitations

Several limitations should be considered when interpreting the results:

- The data represents one Pioneer League season.
- Pitch type was not separated in the primary analysis.
- Pitch location was not incorporated.
- Hitter skill differences were not explicitly modeled.
- Not every hitter faced pitches from all three spin-rate tiers.
- Restricting the repeated-measures analysis to hitters represented in every spin tier substantially reduced the available sample.

These limitations prevent the results from being generalized beyond the analyzed sample without additional validation.

## Future Research

Future versions of the analysis could incorporate:

- Pitch type
- Pitch location
- Pitch velocity
- Horizontal and vertical movement
- Pitcher characteristics
- Hitter quality
- Launch angle
- Barrel rate
- MLB tracking data

A mixed-effects modeling framework could also be expanded to account for repeated observations from individual pitchers and hitters while incorporating additional pitch-level characteristics.

## Tools & Methods

- **R**
- `dplyr`
- `tidyr`
- `ggplot2`
- ANOVA
- Repeated-Measures ANOVA
- Tukey HSD
- Mauchly's Test
- Greenhouse-Geisser Correction
- Mixed-Effects Modeling
- Statistical Inference
- Data Visualization
- TrackMan Pitch-Tracking Data
