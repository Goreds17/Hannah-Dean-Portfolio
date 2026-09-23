# Cincinnati Reds Transaction Forecasting

## Overview

This project analyzes Cincinnati Reds 40-man roster transaction activity using time-series forecasting methods. Monthly transaction data was examined to identify long-term trends, recurring seasonal patterns, and autocorrelation within roster activity.

Multiple forecasting approaches were evaluated, including Simple Exponential Smoothing, Holt's Linear Trend Method, Holt-Winters exponential smoothing, and Seasonal ARIMA. Model performance and residual diagnostics were used to evaluate how effectively each method captured the structure of the transaction series.

## Research Question

**Can historical Cincinnati Reds transaction activity be used to identify seasonal patterns and forecast future roster movement?**

## Data

The dataset contains monthly Cincinnati Reds 40-man roster transaction counts.

Transactions include roster activity such as:

- Signings
- Player activations
- Trades
- Injured List placements
- Options
- Other roster transactions

The monthly structure allows transaction activity to be analyzed as a time series with a seasonal frequency of 12 months.

## Methodology

### Time-Series Exploration

The transaction series was first visualized to examine overall patterns in roster activity.

The data displayed recurring yearly fluctuations, suggesting that transaction activity is influenced by the structure of the MLB calendar.

### Time-Series Decomposition

Additive decomposition was used to separate the series into:

- Trend
- Seasonal
- Random components

The decomposition revealed a strong recurring seasonal component along with a slight long-term decline in transaction frequency.

### Autocorrelation Analysis

The Autocorrelation Function (ACF) was used to examine relationships between observations across different time lags.

Strong autocorrelation at lag 12 provided evidence of recurring annual seasonality in Cincinnati's transaction activity.

### Stationarity Testing

An Augmented Dickey-Fuller (ADF) test was used to evaluate whether the time series was stationary.

The test produced a p-value of **0.01**, providing evidence against the null hypothesis of non-stationarity.

### Forecasting Models

Four forecasting approaches were evaluated.

#### Simple Exponential Smoothing

Simple Exponential Smoothing (SES) was used as a baseline forecasting approach.

Because SES models the level of a series without explicitly incorporating trend or seasonality, it was unable to fully capture the recurring yearly patterns in the transaction data.

#### Holt's Linear Trend Method

Holt's method extended exponential smoothing by incorporating a trend component.

Although this better represented the long-term movement of the series, the model still did not explicitly account for yearly seasonality.

#### Holt-Winters Exponential Smoothing

Holt-Winters incorporated level, trend, and seasonal components simultaneously.

This allowed the model to reproduce the recurring transaction cycles more effectively than the simpler exponential-smoothing approaches.

#### Seasonal ARIMA

The `auto.arima()` procedure selected:

**ARIMA(1,0,0)(0,1,1)[12] with drift**

The model incorporated short-term autoregressive behavior, seasonal differencing, a seasonal moving-average component, and drift.

## Model Evaluation

Forecasting models were evaluated using metrics including:

- Root Mean Squared Error (RMSE)
- Mean Absolute Error (MAE)
- Mean Absolute Scaled Error (MASE)
- Residual autocorrelation

Among the exponential-smoothing models, Holt-Winters produced the strongest training-set error metrics.

The Seasonal ARIMA model produced:

- **RMSE:** 16.24
- **MAE:** 11.69
- **MASE:** 0.68
- **Residual ACF1:** approximately 0

These results indicated that the Seasonal ARIMA model captured much of the trend, seasonal, and autocorrelation structure present in the transaction series.

## Model Diagnostics

Residual diagnostics were performed on the Seasonal ARIMA model using `checkresiduals()`.

The Ljung-Box test produced:

**p = 0.9281**

This provided no evidence of significant remaining residual autocorrelation, indicating that the model accounted for the primary time-dependent structure in the series.

## Key Findings

Cincinnati Reds transaction activity displayed strong annual seasonality, demonstrating that roster movement follows recurring patterns throughout the baseball calendar.

Models that explicitly accounted for seasonality performed better than simpler smoothing approaches. Holt-Winters substantially improved upon SES and Holt's method, while Seasonal ARIMA provided a strong combination of forecast accuracy, seasonal representation, and residual behavior.

The project demonstrates how time-series methods can be applied to baseball operations data to identify organizational patterns and forecast future roster activity.

## Tools & Skills

**Language:** R

**Libraries:** forecast, dplyr, tseries, readr

**Techniques:**
- Time-Series Analysis
- Time-Series Decomposition
- Autocorrelation Analysis
- Augmented Dickey-Fuller Testing
- Simple Exponential Smoothing
- Holt's Linear Trend Method
- Holt-Winters Exponential Smoothing
- Seasonal ARIMA
- Forecasting
- Model Comparison
- Residual Diagnostics
- Ljung-Box Testing

## Future Improvements

Future versions of this project could evaluate forecasting performance using a dedicated holdout period rather than relying primarily on training-set error metrics. Additional seasons could also be incorporated to evaluate whether transaction patterns remain consistent over time.

The analysis could also be expanded by separating transactions into categories such as trades, Injured List moves, options, and signings. This would allow different types of roster activity to be modeled individually and could provide more detailed insight into organizational behavior throughout the MLB calendar.

## Author

**Hannah Dean**

Sports Analytics | Data Science  
California Baptist University
