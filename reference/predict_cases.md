# Predict cases using a fitted GAM model

Predict cases using a fitted GAM model

## Usage

``` r
predict_cases(
  train_data,
  train_test_data,
  prediction_bootstrap,
  gam_mod,
  pop,
  rep_prop,
  n_sim = 100,
  fix_all_covariates
)
```

## Arguments

- train_data:

  Training data

- train_test_data:

  Combined training and test data

- prediction_bootstrap:

  Array for storing predictions

- gam_mod:

  Fitted GAM model

- pop:

  Population size

- rep_prop:

  Reported fraction of cases

- n_sim:

  Number of simulations

- fix_all_covariates:

  Whether to fix all covariates (SIR-like model)

## Value

Array of predictions
