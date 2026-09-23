# Pitch Strike Probability Modeling

**Predictive Analytics | Logistic Regression | Baseball Analytics**

## Project Overview

This project examines how pitch characteristics and count context relate to binary pitch outcomes using professional pitch-tracking data.

A logistic regression model was developed using pitch type, extension, and count context as predictors. The project combines statistical inference with predictive modeling by first examining model coefficients and odds ratios and then evaluating predictive performance using a train/test framework.

The goal was to better understand which pitch characteristics were associated with pitch outcomes while also evaluating how effectively those characteristics could be used for classification.

## Research Question

Can pitch characteristics and count context be used to model and predict whether a pitch results in a ball or strike?

## Data

The original dataset contained pitch-level tracking information for an individual professional pitcher.

Variables used in the final model included:

- **Pitch Type:** Four-Seam, Sinker, Changeup, and Slider
- **Extension:** Pitcher's release extension toward home plate
- **Balls:** Number of balls in the count
- **Strikes:** Number of strikes in the count
- **Pitch Call:** Binary response variable representing pitch outcome

Pitch outcomes that did not fit the binary classification objective, such as balls put into play and foul balls, were excluded from the modeling dataset.

> **Note:** The original dataset is not included in this repository.

## Methodology

### 1. Data Preparation

The pitch-level dataset was cleaned and filtered to retain observations relevant to the binary classification problem. Variables were selected to represent pitch characteristics and game context.

### 2. Logistic Regression

A logistic regression model was used to examine the relationship between the selected predictors and pitch outcome.

Model coefficients, p-values, odds ratios, and confidence intervals were examined to understand the direction and uncertainty of the estimated relationships.

Extension showed the strongest statistical evidence of an association with pitch outcome in the full-data model.

### 3. Train/Test Evaluation

To evaluate predictive performance on unseen observations, the data was divided into training and testing sets.

The model was fit using the training data and then used to generate predicted probabilities for the held-out test observations.

### 4. Model Evaluation

Predictive performance was evaluated using:

- Confusion Matrix
- Accuracy
- Sensitivity
- Specificity
- ROC Curve
- Area Under the ROC Curve (AUC)

The held-out test evaluation produced an accuracy of approximately **81.8%** and an ROC AUC of approximately **0.902**.

Because the test sample was relatively small, these results should be interpreted as an initial evaluation rather than evidence of performance across a broader population of pitchers.

## Key Takeaways

This project demonstrates the use of logistic regression for both statistical analysis and binary classification in a baseball setting.

The analysis provided experience with:

- Pitch-level baseball data
- Data cleaning and feature selection
- Logistic regression
- Odds ratios and confidence intervals
- Statistical significance
- Binary classification
- Train/test validation
- Confusion matrices
- ROC curves and AUC

The project also highlights the importance of separating model interpretation from predictive evaluation and evaluating predictive models using observations that were not used during model fitting.

## Future Improvements

With access to a larger pitch-level dataset, this analysis could be expanded through:

- Repeated cross-validation
- Additional pitch characteristics such as velocity and movement
- Pitch location features
- Batter handedness
- Pitcher/batter matchup information
- Alternative classification algorithms
- Larger out-of-sample validation sets

These additions would allow for a more robust evaluation of model generalizability and potentially improve predictive performance.

## Tools & Technologies

- **R**
- Logistic Regression
- `dplyr`
- `caret`
- `pROC`
- Statistical Modeling
- Binary Classification
- ROC/AUC Analysis

## Project Files

`pitch_outcome_modeling.html` — Full rendered analysis containing the statistical models, model output, visualizations, and interpretation.

## Authors

Hannah Dean, etc.

This project was originally completed as a collaborative statistical modeling project and has been revised for portfolio presentation.
