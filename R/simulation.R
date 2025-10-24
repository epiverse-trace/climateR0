#' Generate predictions using GAM-based forecasting
#'
#' @param model_data Processed model data
#' @param input_data Original input data
#' @param prediction_start Start date for prediction
#' @param horizon Forecast horizon in weeks
#' @param pop Population size
#' @param rep_prop Reported fraction of cases
#' @param n_sim Number of simulations
#' @param model_in Optional pre-fitted GAM model
#' @param fix_all_covariates Whether to fix all covariates (SIR-like model)
#'
#' @return A list containing:
#'   \item{prediction_out}{Array of predictions}
#'   \item{model_out}{Fitted GAM model}
#' @export
generate_prediction <- function(model_data,
                              input_data,
                              prediction_start,
                              horizon,
                              pop,
                              rep_prop,
                              n_sim = 1,
                              model_in = NULL,
                              fix_all_covariates = FALSE) {
  
  message(paste0("Generating forecasts for prediction week starting: "),
          as.Date(as.numeric(prediction_start), origin = "1970-01-01"))
  
  train_data <- model_data |> 
    dplyr::filter(date <= prediction_start)
  
  if(nrow(train_data) < 10) {
    warning("Fewer than 10 weeks of training data, results may be unstable")
  }
  
  # Run GAM model, or use pre-existing input model
  if(is.null(model_in)) {
    # Check whether susceptible term is fixed (i.e. SIR-like) or flexible
    if(fix_all_covariates == TRUE) {
      gam_mod <- mgcv::gam(
        formula = cases ~ offset(log_rR0 + 
                          log_weighted_lagged_cases +
                          log_pop_susceptible),
        family = mgcv::nb(link = "log"),
        data = train_data)
    } else {
      gam_mod <- mgcv::gam(
        formula = cases ~ offset(log_rR0 + 
                          log_pop_susceptible) +
                          log_weighted_lagged_cases,
        family = mgcv::nb(link = "log"),
        data = train_data)
    }
  } else {
    gam_mod <- model_in
  }

  # DEBUG: test_pred <- data.frame(log_rR0 = log(0.5), log_weighted_lagged_cases=log(100), log_pop_susceptible=log(1)); predict(gam_mod,test_pred, type = "response")

  # Define rR0 for forecast period
  get_rR0_forecast <- model_data$rR0[model_data$date > as.Date(prediction_start)][1:horizon]
   
  # Set up testing rows
  prediction_rows <- data.frame(
    date = as.Date(prediction_start, origin = "1970-01-01") + seq(7, 7*horizon, by = 7),
    date_numeric = max(train_data$date_numeric) + seq(7, 7*horizon, by = 7),
    rR0 = get_rR0_forecast,
    log_rR0 = log(get_rR0_forecast + 0.0001),
    log_pop_susceptible = rep(tail(train_data$log_pop_susceptible, 1), horizon),
    log_weighted_lagged_cases = rep(tail(train_data$log_weighted_lagged_cases, 1), horizon),
    cases = rep(NA, horizon)
  )
   
  # Set up variables for first week of forecast
  cumulative_cases <- sum(train_data$cases)
  pop_susceptible <- 1 - pmax(0, (cumulative_cases/pop)/rep_prop)
  prediction_rows$log_pop_susceptible <- log(pop_susceptible + 0.0001)
  
  weighted_lagged_cases = tail(weight_cases(c(train_data$cases, 0)), 1)
  prediction_rows$log_weighted_lagged_cases <- log(weighted_lagged_cases + 0.0001)

  # Define training and test data
  train_test_data <-
    rbind(train_data |> dplyr::select(date, date_numeric, rR0, log_rR0,
                                     log_pop_susceptible, log_weighted_lagged_cases, cases),
          prediction_rows)
  
  # Add simulation column
  train_test_data$cases_sim <- NA
  
  # Set up array for storage
  prediction_bootstrap <- replicate(n_sim, train_test_data, simplify = FALSE)
  
  prediction_bootstrap <- lapply(prediction_bootstrap,
                               function(df) {
                                 data.frame(lapply(df, as.numeric))
                               })
  
  prediction_bootstrap <- abind::abind(prediction_bootstrap, along = 3)
  
  prediction_out <- predict_cases(train_data, # Training data
                                train_test_data, # Training + test window
                                prediction_bootstrap, # Array for storage
                                gam_mod, # Fitted GAM
                                pop, # Population size
                                rep_prop, # Reported fraction
                                n_sim = n_sim, # Number of simulations
                                fix_all_covariates # SIR-like model = T/F
  )
  
  # Return prediction and model
  list(prediction_out = prediction_out, model_out = gam_mod)
} 