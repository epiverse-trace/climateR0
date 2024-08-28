#' Estimate relative R0 from mean temperature

#' @description This function takes an input series of mean temperature values in °C
#' and returns estimated relative R0 values for a specified vector and pathogen.
#' @importFrom rlang .data 
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#' @param data A numeric vector of mean temperature values in °C
#' @param vector_pathogen Character vector specifying the vector and pathogen of interest
#' This can be one of the following:
#' AeaeDENV; AeaeZIKV; AealDENV; AetaRVFV; AetaSINV; AetrEEEV; AngaPfal; CxanMVEx; CxanRRVx; CxpiSINV;
#' CxpiWNVxl; CxtaWEEV; CxtaWNVx; CxunWNVx
#'@return Numeric vector of relative R0 values corresponding to input temperature values
#'@references
#'
#' Mordecai, E. A., Caldwell, J. M., Grossman, M. K., Lippi, C. A., Johnson, L. R., Neira, 
#' M., Rohr, J. R., Ryan, S. J., Savage, V., Shocket, M. S., Sippy, R., Stewart Ibarra, A. M.,
#' Thomas, M. B., Villena O. (2019). Thermal biology of mosquito-borne disease. Ecol Lett. 22(10)
#' \doi{10.1111/ele.13335}
#' 
#' @examples
#' # Return relative R0 for Aedeas aegypti and dengue virus for mean temperatures from 18-36°C
#' temperature_r0(c(18,20,22,24,26,28,30,32,34,36), "AeaeDENV")
#'
#'@export
temperature_r0 <- function(data, vector_pathogen){
  
  tryCatch(
    {
      match.arg(vector_pathogen, choices = vector_reference$vector_pathogen)
    },
    error = function(e) {
      stop("Vector-pathogen code is not recognised. This should be one of: 
    AeaeDENV; AeaeZIKV; AealDENV; AetaRVFV; AetaSINV; AetrEEEV; AngaPfal; CxanMVEx; 
    CxanRRVx; CxpiSINV; CxpiWNVxl; CxtaWEEV; CxtaWNVx; CxunWNVx.", call. = FALSE)
    }
  )

  message(paste0("Returning relative R0 for vector: " , vector_reference$vector_name[vector_reference$vector_pathogen == vector_pathogen], 
                   " and pathogen: ", vector_reference$pathogen_name[vector_reference$vector_pathogen == vector_pathogen]))
  
  
  # Extract temperature - R0 posteriors
  
  r0_posteriors <- read.csv(system.file("extdata", paste0(vector_pathogen, ".R0.csv"), package = "climateR0", mustWork = TRUE))
  constant_temps <- read.csv(system.file("extdata", paste0(vector_pathogen, ".T.csv"), package = "climateR0", mustWork = TRUE))
  
  rownames(r0_posteriors) <- constant_temps[,1]
  
  # Wrangle posteriors
  
  tr0 <- r0_posteriors %>% 
    tibble::rownames_to_column(var = "temp") %>% 
    tidyr::pivot_longer(cols = -.data$temp, names_to = "iteration", names_prefix = "X0.", values_to = "r0") %>% 
    dplyr::mutate(iteration = gsub("[^0-9.-]", "", .data$iteration), iteration = as.integer(.data$iteration)) %>% 
    dplyr::mutate(temp = as.numeric(.data$temp)) %>% 
    dplyr::group_by(.data$temp) %>% 
    dplyr::summarise(median_r0 = stats::median(.data$r0)) %>% 
    # rescale to get relative r0 between 0 and 1
    dplyr::mutate(median_r0 = .data$median_r0/max(.data$median_r0))
  
  opt_temp <- tr0$temp[tr0$median_r0 == max(tr0$median_r0)]
  
  r0_fun <- cinterpolate::interpolation_function(tr0$temp, tr0$median_r0, type = "spline")
  r0_out <- r0_fun(data)
  
  return(r0_out)
}
