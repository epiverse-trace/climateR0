#' Prepare data for GAM-based forecasting
#'
#' @param input_data A data frame containing date, cases and av_temp columns
#' @param pop Assumed population size
#' @param rep_prop Assumed proportion of infections reported as cases
#'
#' @return A list containing:
#'   \item{model_data}{Processed data frame with additional columns for
#'   modelling}
#'   \item{prediction_dates}{Vector of dates for prediction}
#'
#' @export
prepare_data <- function(input_data, pop, rep_prop) {
  # Check that reporting assumption does not result in more infections than
  # pop size
  if (sum(input_data$cases, na.rm = TRUE) / rep_prop > pop) {
    stop(
      "Assumed reporting too low, implies more infections than population size",
      call. = FALSE
    )
  }

  # Add relative R0 column
  input_data$rR0 <- temperature_r0(input_data$av_temp, "AeaeDENV")

  # Add columns tracking recent lagged cases (for serial interval)
  # and cumulative cases (for estimating immunity)
  model_data <- input_data |>
    dplyr::mutate(date_numeric = as.numeric(date - min(date) + 1)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(log_rR0 = log(rR0 + 0.0001),
           cumulative_cases = dplyr::lag(cumsum(cases), 1),
           pop_susceptible = 1 - pmax(0, (cumulative_cases / pop) / rep_prop),
           weighted_lagged_cases = weight_cases(cases),
           log_weighted_lagged_cases = log(weighted_lagged_cases + 0.0001),
           log_pop_susceptible = log(pop_susceptible + 0.0001))

  # Set cumulative cases to zero initially
  model_data$cumulative_cases[1] <- 0
  model_data$pop_susceptible[1] <- 1
  model_data$log_pop_susceptible[1] <- log(1 + 0.0001)

  # Note the below is constrained to match the temperature date range available
  # It also includes a buffer of 5 weeks to account for serial interval
  # calculation above
  prediction_dates <- seq(from = (min(input_data$date) + 5 * 7),
                         to = max(input_data$date), by = 7)

  list(model_data = model_data, prediction_dates = prediction_dates)
}

#' Weight past cases by dengue serial interval
#'
#' @param x Vector of case counts
#'
#' @return Vector of weighted case counts based on serial interval
#' @export
weight_cases <- function(x) {
  dplyr::lag(x, 2) * 0.2 + dplyr::lag(x, 3) * 0.425 + dplyr::lag(x, 4) * 0.375
}

#' Find closest matching day of the year
#'
#' @param vec1 Vector of dates to match
#' @param vec2 Vector of reference dates
#'
#' @return Vector of indices in vec2 that are closest to each date in vec1
#' @export
find_closest_days <- function(vec1, vec2) {
  doy1 <- as.numeric(format(vec1, "%j"))
  doy2 <- as.numeric(format(vec2, "%j"))

  sapply(doy1, function(doy) {
    closest_index <- which.min(abs(doy2 - doy))
    closest_index
  })
}
