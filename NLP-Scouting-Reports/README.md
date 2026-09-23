# NLP in Scouting Reports

## Overview

This project applies **Natural Language Processing (NLP) and machine learning** to baseball scouting reports to examine whether qualitative scouting language can be transformed into quantitative features associated with player evaluation.

Using **543 scouting reports**, I combined TF-IDF representations of scouting text with baseball-specific engineered features to model:

- **Player Risk:** Low, Medium, or High
- **Future Value (FV):** Numerical prospect FV grade

The strongest model was the Future Value model, with Ridge Regression achieving **R² = 0.659** and **RMSE = 3.32 FV points** on held-out test data.

---

## Research Questions

- Can scouting report language be used to model assigned Future Value?
- Can NLP classify player risk levels?
- How can baseball-specific terminology be incorporated into NLP models?

---

## Methodology

### Text Preprocessing

Scouting reports were processed using:

- Player-name masking
- Tokenization
- Custom stop-word removal
- Lemmatization
- Negation handling
- Preservation of baseball-specific terminology

### Feature Engineering

TF-IDF features were combined with baseball-specific features capturing:

- Upside and risk terminology
- Projected role terminology
- Pitch and tool terminology
- Premium positions
- Velocity ranges
- Tool grades
- Development-level references
- Report length
- Pitcher vs. position-player classification

### Models

An **80/20 train-test split** was used to evaluate three models:

| Model | Target |
|---|---|
| Logistic Regression | Risk Classification |
| Linear SVM | Risk Classification |
| Ridge Regression | Future Value |

---

## Results

| Model | Performance |
|---|---:|
| Logistic Regression — Risk | **44.0% Accuracy** |
| Linear SVM — Risk | **45.0% Accuracy** |
| Ridge Regression — FV | **R² = 0.659** |
| Ridge Regression — FV | **RMSE = 3.32** |

Risk classification proved difficult, with both classification models achieving approximately 44–45% accuracy.

Future Value prediction performed substantially better. The Ridge Regression model explained approximately **66% of the variation in assigned FV grades within the held-out test sample**.

These results suggest that the combined text and engineered features contained substantially more recoverable information about assigned Future Value than the three risk categories.

---

## Baseball Applications

- Scouting analytics
- Prospect evaluation
- Quantifying qualitative scouting information
- Future Value modeling
- Player risk analysis
- Automated scouting-report processing

This project demonstrates how NLP can help transform qualitative scouting information into features that can be incorporated into quantitative baseball analysis.

---

## Technologies

**Python:** Pandas, NumPy, SciPy  
**NLP:** NLTK, TF-IDF, tokenization, lemmatization, n-grams  
**Machine Learning:** scikit-learn, Logistic Regression, Linear SVM, Ridge Regression  
**Evaluation:** Accuracy, Precision, Recall, F1, RMSE, R²

---

## Limitations & Future Work

- Dataset limited to 543 scouting reports
- Risk classifications were difficult to distinguish using the current features
- Engineered features were not tested against a TF-IDF-only baseline
- Future work could incorporate cross-validation and hyperparameter tuning
- Separate pitcher and position-player models could be explored
- Word embeddings and transformer-based NLP models could be tested
- Future research could examine whether scouting-derived features relate to actual professional performance

> **Note:** The model predicts assigned scouting Future Value grades, not realized future MLB performance.

---
Sports Analytics & Data Science | California Baptist University

**Interests:** Baseball R&D, Machine Learning, NLP, Player Evaluation, and Player Development
