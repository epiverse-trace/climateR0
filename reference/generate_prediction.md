# Generate predictions using GAM-based forecasting

Generate predictions using GAM-based forecasting

## Usage

``` r
generate_prediction(
  model_data,
  input_data,
  prediction_start,
  horizon,
  pop,
  rep_prop,
  n_sim = 1,
  model_in = NULL,
  fix_all_covariates = FALSE
)
```

## Arguments

- model_data:

  Processed model data

- input_data:

  Original input data

- prediction_start:

  Start date for prediction

- horizon:

  Forecast horizon in weeks

- pop:

  Population size

- rep_prop:

  Reported fraction of cases

- n_sim:

  Number of simulations

- model_in:

  Optional pre-fitted GAM model

- fix_all_covariates:

  Whether to fix all covariates (SIR-like model)

## Value

A list containing:

- prediction_out:

  Array of predictions

- model_out:

  Fitted GAM model
