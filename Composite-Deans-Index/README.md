# Composite Dean's Index (CDI)

## Overview

The **Composite Dean's Index (CDI)** is a skill-based pitcher evaluation framework designed to measure underlying pitching talent rather than relying solely on traditional outcome statistics.

Traditional metrics such as ERA and FIP describe what happened on the field, but pitchers with similar results can arrive at those results through very different skill profiles. CDI was developed to evaluate the underlying abilities that contribute to pitcher performance and provide a more complete picture of how a pitcher generates results.

The framework evaluates pitchers across four primary skill domains:

- **Stuff** — physical pitch quality
- **Command** — location and execution
- **Miss Ability** — ability to generate non-contact outcomes
- **Contact Suppression** — ability to limit damage when contact occurs

These skill domains are stabilized, standardized, and combined into a composite pitcher evaluation framework.

> **Research Status:** This research has been presented at professional baseball research conferences and is currently being prepared for publication. Full implementation code and certain methodological details are not publicly available at this time.

---

## Research Objective

The primary goal of CDI is to answer a different question than traditional pitching statistics:

**What underlying skills make a pitcher effective, and can those skills provide a more stable framework for evaluating and predicting pitching performance?**

The project investigates:

- How individual pitching skills contribute to different outcomes
- Whether pitcher-controlled skills can separate underlying ability from realized results
- How multiple pitching dimensions can be combined into a single interpretable framework
- Whether current skill profiles can predict future pitching performance
- How the framework can support player evaluation and player development

---

## Research Process

The CDI framework follows a multi-stage analytical pipeline:

```text
Pitch-Level Data
       ↓
Feature Engineering
       ↓
Skill-Domain Construction
       ↓
Empirical Bayes Stabilization
       ↓
Reliability Weighting
       ↓
Standardization
       ↓
Composite Index
       ↓
Statistical Validation
       ↓
Out-of-Sample Prediction
```

Each stage is designed to move from raw pitch-level information toward a more stable representation of underlying pitcher skill.

---

## Skill Domains

### Stuff

The Stuff component evaluates the **physical quality of a pitcher's arsenal**.

Pitch characteristics are modeled at the pitch-type level so that different pitch types can be evaluated according to their own physical profiles.

The process considers characteristics related to areas such as:

- Velocity
- Movement
- Spin
- Release characteristics
- Pitch shape
- Arsenal composition

Pitch-level evaluations are then aggregated to create a pitcher-level representation of overall pitch quality.

---

### Command

The Command component evaluates a pitcher's ability to **locate and execute pitches**.

Rather than measuring command through a single outcome, the framework considers multiple aspects of location and strike-zone management.

The goal is to distinguish pitchers who consistently execute locations, control the strike zone, limit noncompetitive pitches, and effectively expand the zone.

---

### Miss Ability

Miss Ability measures a pitcher's ability to **generate non-contact outcomes**.

The domain captures multiple aspects of bat-missing and strike-finishing ability rather than relying on one statistic alone.

This provides a broader representation of a pitcher's ability to:

- Generate swings and misses
- Create strikes
- Finish plate appearances
- Produce strikeouts

---

### Contact Suppression

Contact Suppression evaluates what happens **when hitters put the ball in play**.

The objective is to measure a pitcher's ability to prevent damaging contact by incorporating multiple indicators of contact quality.

This separates the ability to avoid contact entirely from the ability to limit damage after contact occurs.

---

## Statistical Methodology

A major component of CDI is accounting for the reliability of baseball statistics.

Pitching metrics can become unstable when pitchers have different sample sizes, pitch usage patterns, or opportunities. The framework therefore applies several statistical techniques before constructing the final skill scores.

### Empirical Bayes Stabilization

Raw pitcher estimates are stabilized toward league-average performance.

Conceptually:

```text
Small Sample
     ↓
More Shrinkage Toward League Average

Large Sample
     ↓
More Weight on Observed Performance
```

This reduces the influence of extreme results produced from limited opportunities and helps prevent overreaction to small samples.

### Reliability Weighting

Not every component of a skill domain contains the same amount of information.

Reliability weighting allows more stable measures to contribute more heavily while preventing noisy or low-sample metrics from dominating a skill score.

### Standardization

After stabilization, variables are transformed onto comparable standardized scales.

This allows metrics measured in different units to be combined while preserving their direction of interpretation.

Lower-is-better measures are appropriately transformed so that the final skill scores maintain a consistent interpretation:

**Higher score = stronger underlying skill.**

---

## Statistical Validation

The framework was evaluated using multiple statistical approaches rather than relying on the composite score alone.

### Multiple Linear Regression

Regression models were used to examine how each skill domain relates to pitching outcomes while controlling for the other skills.

Conceptually:

```text
Pitching Outcome =
    Stuff
  + Command
  + Miss Ability
  + Contact Suppression
  + Error
```

This allows the analysis to estimate the unique relationship between each skill domain and different measures of pitcher performance.

The research found that **different pitching skills contribute differently depending on the outcome being modeled**, supporting the use of a multidimensional framework rather than evaluating pitchers through one underlying characteristic.

### Correlation Analysis

Correlation analysis was used to evaluate the relationship between CDI and multiple measures of pitcher performance.

This helped determine whether stronger CDI profiles were associated with stronger performance across both skill-based and traditional pitching outcomes.

### Cross-Validation

Cross-validation was used to evaluate how well the framework generalizes beyond the data used to fit the models.

This provides a stronger test of predictive performance than evaluating model fit on the same observations used during development.

### Model Diagnostics

Diagnostic analysis was also performed to identify unusual observations and determine whether individual pitchers were exerting excessive influence on fitted models.

Methods included:

- Statistical leverage
- Cook's Distance
- Skill-profile analysis
- Principal Component Analysis (PCA)

These diagnostics help verify that model conclusions are not simply being driven by a small number of extreme pitcher profiles.

---

## Out-of-Sample Prediction

An important part of the research evaluates whether current pitcher skills contain information about **future performance**.

The predictive workflow follows a year-to-year validation structure:

```text
Previous-Season Skill Data
          ↓
      Fit Model
          ↓
   Freeze Model
          ↓
Apply to Following Season
          ↓
Predict Future Performance
          ↓
Compare Prediction vs. Actual
```

Model parameters are estimated using the training season and then held fixed when generating predictions for the following season.

This creates an out-of-sample test of whether the underlying skill framework generalizes to future pitcher performance rather than simply describing the season on which it was developed.

---

## Key Findings

The analysis produced several broader findings:

- Pitchers with similar traditional results can have substantially different underlying skill profiles.
- Different pitching skills contribute differently across pitching outcomes.
- A multidimensional framework provides information that is not visible from ERA or FIP alone.
- CDI showed stronger relationships with several skill-based and defense-independent outcomes than with traditional ERA.
- Year-to-year validation demonstrated that current skill profiles contain predictive information about future pitching performance.
- Diagnostic testing indicated that fitted relationships were not being driven solely by a small number of unusual pitcher observations.

These results support evaluating pitchers through their underlying skill profiles in addition to their realized outcomes.

---

## Baseball Applications

### Player Evaluation

CDI can provide an additional perspective when evaluating pitcher talent by separating **underlying skills from realized results**.

Potential applications include:

- Identifying pitchers whose underlying skills differ from their traditional statistics
- Comparing pitchers with similar results but different skill profiles
- Supporting scouting and acquisition research

### Player Development

Because CDI is divided into individual skill domains, the framework can identify **where a pitcher is strong and where development opportunities exist**.

This can support:

- Individual development plans
- Skill-specific training priorities
- Tracking changes in pitcher skill profiles over time

### Pitch Design

The Stuff and skill-domain framework can help connect pitch characteristics with broader pitcher outcomes.

Potential applications include:

- Evaluating arsenal quality
- Identifying opportunities to improve bat-missing ability
- Examining pitch characteristics associated with contact suppression
- Connecting pitch design decisions with overall pitcher development

### Baseball Operations

The framework can also support broader decision-making by providing another quantitative perspective for:

- Player evaluation
- Roster decisions
- Trade-target research
- Bullpen-role evaluation
- Player-development planning

---

## Skills Demonstrated

### Statistical Modeling
- Multiple Linear Regression
- Predictive Modeling
- Cross-Validation
- Correlation Analysis
- Model Diagnostics
- Out-of-Sample Validation

### Advanced Statistics
- Empirical Bayes Stabilization
- Reliability Weighting
- Z-Score Standardization
- Shrinkage Estimation
- Principal Component Analysis
- Leverage Analysis
- Cook's Distance

### Data Science
- Feature Engineering
- Pitch-Level Data Processing
- Model Development
- Model Validation
- High-Dimensional Baseball Data Analysis

### Baseball Analytics
- Pitcher Evaluation
- Pitch Quality Analysis
- Command Evaluation
- Contact Quality Analysis
- Swing-and-Miss Analysis
- Player Development
- Pitch Design
- Advanced Pitching Metrics

### Programming & Visualization
- R
- Python
- Data Visualization
- Statistical Computing
- Baseball Data Visualization

---

## Limitations & Future Work

Future development of CDI will focus on:

- Validating the framework across additional MLB seasons
- Testing year-to-year stability
- Improving skill weighting through cross-validation
- Examining differences by pitcher role and pitch mix
- Incorporating additional game context
- Increasing model granularity
- Developing an interactive analytics tool for pitcher comparison and player development

The long-term goal is to continue refining the framework while translating the research into practical tools for baseball decision-making.

---

## Research & Publication

This project was developed by:

**Hannah Dean**  
with **Dr. Esther Lee** and **Dr. Anthony Corso**

The research has been presented at professional baseball research conferences and is currently being prepared for publication.

### Code Availability

Because this research is currently being prepared for publication, the complete CDI implementation is **not publicly available**.

This repository is intended to demonstrate the project's:

- Research design
- Statistical methodology
- Modeling process
- Validation framework
- Baseball applications
- Technical skills

Selected visualizations, presentation materials, and non-sensitive supporting materials may be included without releasing the complete implementation of the metric.
