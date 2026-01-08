#' Define prediction start dates
#'
#' @param prediction_dates Vector of dates in the dataset
#' @param horizon Forecast horizon in weeks
#' @param n_start Number of start dates to choose
#'
#' @return Vector of prediction start dates
#' @export
define_prediction_starts <- function(prediction_dates, horizon, n_start) {
  # Define possible prediction start points (has to be <= horizon)
  if (length(prediction_dates) < horizon) {
    stop(
      "Need horizon to be smaller than max available prediction window",
      call. = FALSE
    )
  }

  # Select start dates evenly spread across prediction_dates
  pick_indices <- function(vec, x) {
    if (x > length(vec)) {
      stop(
        "Number of chosen start dates can't be larger than possible set of options.",
        call. = FALSE
      )
    }
    indices <- round(seq(1, length(vec), length.out = (x + 1)))
    indices <- indices[-1] # Remove 1st option as less reliable for fitting
    vec[indices]
  }

  # Choose range of indices to define the end of the real-time fitting period,
  # and start of prediction period
  prediction_range <- prediction_dates[1:(length(prediction_dates) - horizon)]
  prediction_vals <- pick_indices(prediction_range, n_start)

  # Check prediction values value
  if (sum(is.na(prediction_vals)) > 0) {
    stop("Need smaller horizon or stop predictions earlier", call. = FALSE)
  }

  prediction_vals
}

#' Predict cases using a fitted GAM model
#'
#' @param train_data Training data
#' @param train_test_data Combined training and test data
#' @param prediction_bootstrap Array for storing predictions
#' @param gam_mod Fitted GAM model
#' @param pop Population size
#' @param rep_prop Reported fraction of cases
#' @param n_sim Number of simulations
#' @param fix_all_covariates Whether to fix all covariates (SIR-like model)
#'
#' @return Array of predictions
#' @export
predict_cases <- function(train_data,
                         train_test_data,
                         prediction_bootstrap,
                         gam_mod,
                         pop,
                         rep_prop,
                         n_sim = 100,
                         fix_all_covariates) {

  # Define prediction window
  horizon_start <- nrow(train_data[, , 1]) + 1
  horizon_end <- nrow(train_test_data[, , 1])

  # Set up progress bar
  pb <- utils::txtProgressBar(
    horizon_start, horizon_end, style = 3, title = "Simulating: "
  )

  for (kk in horizon_start:horizon_end) {

    # Iterate over simulation particles
    for (jj in 1:n_sim) {

      # Generate draw from bootstrap dataset
      index_sample <- sample(1:n_sim, 1)
      train_test_data_kk <- data.frame(prediction_bootstrap[1:kk, , index_sample])

      # Predicted means based on original model
      original_means <- predict(
        gam_mod,
        newdata = train_test_data_kk,
        type = "link"
      )

      # Get the variance-covariance matrix of the model coefficients
      vcov_matrix <- vcov(gam_mod)

      # Extract the coefficients of the model
      coef_mean <- coef(gam_mod)

      # Draw a new set of coefficients from the multivariate normal distribution
      coef_sim <- MASS::mvrnorm(1, mu = coef_mean, Sigma = vcov_matrix)

      # Compute the linear predictor with the new coefficients
      design_matrix <- model.matrix(gam_mod, newdata = train_test_data_kk)

      # Specify prediction depending on whether susceptible term has a coefficient
      if (fix_all_covariates == TRUE) {
        linear_predictor <- design_matrix %*% coef_sim +
                          train_test_data_kk$log_rR0 +
                          train_test_data_kk$log_weighted_lagged_cases +
                          train_test_data_kk$log_pop_susceptible
      } else {
        linear_predictor <- design_matrix %*% coef_sim +
                          train_test_data_kk$log_rR0 +
                          train_test_data_kk$log_pop_susceptible
      }

      # Transform to the response scale (mean counts) using the inverse log-link
      predicted_means <- exp(linear_predictor)

      # Generate simulated counts from the expectation
      # Note: we use expectation because interested in underlying dynamics
      new_simulation <- predicted_means #exp(original_means)

      # Record full range of simulated cases
      if (kk == horizon_start) {
        prediction_bootstrap[1:kk, "cases_sim", jj] <- new_simulation
      } else {
        prediction_bootstrap[kk, "cases_sim", jj] <- tail(new_simulation, 1)
      }

      # Record new step simulated cases only
      prediction_bootstrap[kk, "cases", jj] <- tail(new_simulation, 1)

      # Update other outputs
      train_test_new <- data.frame(prediction_bootstrap[, , jj])

      cases_all_na <- train_test_new$cases[!is.na(train_test_new$cases)]
      cumulative_cases <- sum(cases_all_na)
      pop_susceptible <- 1 - pmin(1, (cumulative_cases / pop) / rep_prop)

      # Add padding to calculate
      weighted_lagged_cases <- tail(
        weight_cases(c(train_test_new$cases[1:kk], 0)), 1
      ) 
      log_weighted_lagged_cases <- log(weighted_lagged_cases + 0.0001)
      log_pop_susceptible <- log(pop_susceptible + 0.0001)

      if (kk < horizon_end) {
        prediction_bootstrap[kk + 1, "log_weighted_lagged_cases", jj] <- tail(log_weighted_lagged_cases, 1)
        prediction_bootstrap[kk + 1, "log_pop_susceptible", jj] <- tail(log_pop_susceptible, 1)
      }
    }

    # Display progress
    utils::setTxtProgressBar(pb, kk)
  }

  close(pb) # Close bar

  prediction_bootstrap
}
