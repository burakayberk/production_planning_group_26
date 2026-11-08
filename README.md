# production_planning_group0
## Test Datasets

The `test_data/` directory contains datasets used to validate the forecasting and aggregate planning modules of the Shiny application.

### Forecasting Modules

| Dataset | Modules |
|---|---|
| `production_planning_test_data.xlsx` | Moving Average, Weighted Moving Average, Simple Exponential Smoothing, Holt's Method, Seasonal Decomposition, Winters' Method |
| `ar_test_data.csv` | Autoregressive (AR) |
| `ma_test_data.csv` | Moving Average Error Model (MA) |
| `arma_test_data.csv` | Autoregressive Moving Average (ARMA) |
| `arima_test_data.csv` | Autoregressive Integrated Moving Average (ARIMA) |

### Aggregate Planning

| Dataset | Purpose |
|---|---|
| `aggregate_planning_test_data.csv` | Demand forecasts and working days for aggregate planning methods |
| `production_planning_test_data.xlsx` | Additional forecasting and aggregate planning test scenarios |

# How to Use

1. Open the Shiny application.
2. Select the forecasting or planning module.
3. Upload the corresponding dataset from the `test_data/` directory.
4. Select the appropriate data column and model parameters.
5. Run the model and verify the forecasts, plots, and error measures.

**Note:** All datasets are synthetic and intended for testing and validation purposes only.
