# Predicting Pitcher ERA Using Advanced Metrics

## Overview

This project examines how underlying pitching performance metrics relate to ERA among Major League Baseball pitchers. Using 2025 Statcast data, I applied multiple linear regression to evaluate whether advanced metrics can provide additional insight into pitcher performance beyond traditional outcome statistics.

The analysis focused on expected weighted on-base average (xwOBA), strikeout rate (K%), home run rate (HR/9), and pitcher handedness. Model comparison, statistical inference, and regression diagnostics were used to evaluate the relationships between these variables and ERA.

## Research Question

**To what extent do underlying pitching performance metrics explain variation in ERA among MLB pitchers?**

## Data

The analysis used 2025 Statcast pitching data for 69 MLB pitchers.

Variables evaluated included:

- ERA
- xwOBA
- Strikeout Rate (K%)
- Home Run Rate (HR/9)
- Pitcher Handedness
- Walk Rate (BB%)
- Average Fastball Velocity
- WHIP
- Ground Ball Rate (GB%)

## Methodology

### Exploratory Data Analysis

I first examined the distributions and summary statistics of the pitching metrics to better understand the structure of the dataset.

Correlation analysis was then used to evaluate relationships among the variables.

### Multicollinearity Analysis

Variance Inflation Factors (VIF) were calculated to evaluate potential multicollinearity among predictors.

Although some predictors were correlated, all VIF values remained below 5, indicating that multicollinearity was not severe enough to prevent model interpretation.

### Multiple Linear Regression

Multiple regression models were constructed using different combinations of:

- xwOBA
- K%
- HR/9
- Handedness

Models were compared using both R² and adjusted R² to evaluate explanatory power while accounting for model complexity.

The model containing **xwOBA and HR/9** produced the highest adjusted R² among the candidate models.

### Statistical Inference

Regression coefficients, p-values, and confidence intervals were evaluated to determine which variables provided statistically meaningful information about ERA.

xwOBA emerged as the strongest predictor in the analysis and demonstrated a statistically significant positive relationship with ERA.

### Model Diagnostics

Regression assumptions were evaluated using:

- Residuals vs. Fitted plots
- Scale-Location plots
- Q-Q plots
- Residual distributions
- Cook's Distance
- Residuals vs. Leverage

These diagnostics were used to assess linearity, constant variance, residual normality, and influential observations.

## Key Findings

The analysis showed that xwOBA had the strongest relationship with ERA among the variables examined.

The model comparison also demonstrated that adding additional predictors did not necessarily improve the model after accounting for model complexity. This highlights the importance of balancing explanatory power with model simplicity.

From a baseball perspective, the results reinforce the value of evaluating the quality of contact a pitcher allows rather than relying exclusively on traditional results such as ERA.

## Tools & Skills

**Language:** R

**Libraries:** dplyr, tidyr, ggplot2, corrplot, car, broom, knitr

**Techniques:**
- Multiple Linear Regression
- Exploratory Data Analysis
- Correlation Analysis
- Multicollinearity Diagnostics
- Model Comparison
- Statistical Inference
- Confidence Intervals
- Regression Diagnostics
- Baseball Analytics

## Project Files

### Technical Analysis

The full analysis contains the R code, statistical output, model comparison, inference, visualizations, and regression diagnostics.

[`analysis/predicting_pitcher_era_analysis.pdf`](analysis/predicting_pitcher_era_analysis.pdf)

### Presentation

A condensed presentation of the research question, methodology, results, and baseball applications.

[`presentation/predicting_pitcher_era_presentation.pdf`](presentation/predicting_pitcher_era_presentation.pdf)

## Future Improvements

Future versions of this project could expand the pitcher sample, incorporate additional seasons, and evaluate alternative outcome metrics such as FIP or xERA. Additional validation techniques could also be used to evaluate how well the models generalize to unseen pitchers.

## Author

**Hannah Dean**

Sports Analytics | Data Science  
California Baptist University
