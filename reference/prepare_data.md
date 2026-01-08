# Prepare data for GAM-based forecasting

Prepare data for GAM-based forecasting

## Usage

``` r
prepare_data(input_data, pop, rep_prop)
```

## Arguments

- input_data:

  A data frame containing date, cases and av_temp columns

- pop:

  Assumed population size

- rep_prop:

  Assumed proportion of infections reported as cases

## Value

A list containing:

- model_data:

  Processed data frame with additional columns for modelling

- prediction_dates:

  Vector of dates for prediction
