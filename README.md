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


# How to Use

1. Open the Shiny application.
2. Select the forecasting or planning module.
3. Upload the corresponding dataset from the `test_data/` directory.
4. Select the appropriate data column and model parameters.
5. Run the model and verify the forecasts, plots, and error measures.

**Note:** All datasets are synthetic and intended for testing and validation purposes only.

## Aggregate Planning

Implemented modules: Chase, Level, Mixed, Linear Programming, Transportation, Disaggregation.

Upload `test_data/aggregate_planning_test_data.csv`, or use the built-in example.
Excel uploads support sheet selection. Map period, demand and working-day columns.
Regular cost is a wage per worker-day; overtime/subcontract costs are per unit.
No backorders are allowed. Ending inventory is a minimum target. Holding cost
is charged on period-end stock. K = units / (workers * working days).

The level plan chooses the smallest constant workforce satisfying every cumulative
requirement. Mixed plans use the same rule within user-selected workforce blocks.
Rounded plans recalculate production and inventory. The rounded LP fixes workforce
at the ceilings of the continuous solution and re-optimizes with idle time allowed.
Transportation holds workforce fixed and compares feasible north-west and cheapest
cell starting solutions with the LP optimum. Its initial solutions check residual
feasibility; cheapest cell first does not guarantee an optimum.

### Run locally

Install R packages `shiny`, `readxl`, and `lpSolve`, then from this directory run:

```r
shiny::runApp('.')
```

### Validation

Run `Rscript tests/test_planning.R` and `Rscript tests/test_interface.R`.
Checks cover stock balances, costs, rounding, infeasibility, and the equivalence
of optimal transportation and LP with fixed workforce.

Before publishing, regenerate `manifest.json` using `rsconnect::writeManifest('.')`.

## Disaggregation

Upload a downloaded aggregate plan, select a period, and upload family data using
`test_data/disaggregation_test_data.csv` as a template. All quantities are in the
same aggregate-equivalent units. The implemented square-root rule is
Q_i = max(Demand_i - OpeningInventory_i, lambda * sqrt(SetupCost_i * Demand_i)),
with lambda chosen so sum(Q_i) equals the selected aggregate production.
It minimizes sum(SetupCost_i * Demand_i / Q_i) subject to the total and minimum
net-demand constraints. Equal setup costs reduce the weights to sqrt(Demand_i).
Zero weights receive no discretionary allocation unless all weights are zero,
when residual production is split equally. Insufficient aggregate production
is reported as infeasible. Use ending family stocks as next-period opening stocks.
The assignment does not supply its course formula; this implementation states
the chosen convention explicitly. It was not verified against a PPC textbook.

Run `Rscript tests/test_disaggregation.R` to check allocation conservation,
net-demand constraints, the square-root ratio, and the Shiny interface.
