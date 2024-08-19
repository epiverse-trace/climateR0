#' Estimate relative R0 from mean temperature

#' @description This function takes an input series of mean temperature values in °C
#' and returns estimated relative R0 values according to temperature-R0 curves in
#' from Mordecai et al 2019 https://doi.org/10.1111/ele.13335
#' @importFrom rlang .data
#' @param data a numeric vector of mean temperature values in °C
#' @param vector_pathogen character vector specifying the vector and pathogen of interest
#' This can be one of the following:
#' AeaeDENV; AeaeZIKV; AealDENV; AetaRVFV; AetaSINV; AetrEEEV; AngaPfal; CxanMVEx; CxanRRVx; CxpiSINV;
#' CxpiWNVxl; CxtaWEEV; CxtaWNVx; CxunWNVx
#'@return numeric vector of relative R0 values corresponding to input temperature values
#'@export
temperature_r0 <- function(data, vector_pathogen){
  
  if(!vector_pathogen %in% vector_reference$vector_pathogen){
    message("Vector-pathogen code is not recognised. This should be one of: 
    AeaeDENV; AeaeZIKV; AealDENV; AetaRVFV; 
    AetaSINV; AetrEEEV; AngaPfal; CxanMVEx; 
    CxanRRVx; CxpiSINV; CxpiWNVxl; CxtaWEEV; 
    CxtaWNVx; CxunWNVx.")
  } else {
    message(paste0("Returning relative R0 for vector: " , vector_reference$vector_name[vector_reference$vector_pathogen == vector_pathogen], 
                   " and pathogen: ", vector_reference$pathogen_name[vector_reference$vector_pathogen == vector_pathogen]))
  }
  
  # Extract temperature - R0 posteriors
  
  r0_posteriors <- eval(parse(text = paste0(vector_pathogen, "_R0")))
  constant_temps <- eval(parse(text = paste0(vector_pathogen, "_T")))
  
  rownames(r0_posteriors) <- constant_temps[,1]
  
  # Wrangle posteriors
  
  tr0 <- r0_posteriors |> 
    tibble::rownames_to_column(var = "temp") |> 
    tidyr::pivot_longer(cols = -.data$temp, names_to = "iteration", names_prefix = "X0.", values_to = "r0") |> 
    dplyr::mutate(iteration = gsub("[^0-9.-]", "", .data$iteration), iteration = as.integer(.data$iteration)) |> 
    dplyr::mutate(temp = as.numeric(.data$temp)) |> 
    dplyr::group_by(.data$temp) |> 
    dplyr::summarise(median_r0 = stats::median(.data$r0)) |> 
    # rescale to get relative r0 between 0 and 1
    dplyr::mutate(median_r0 = .data$median_r0/max(.data$median_r0))
  
  opt_temp <- tr0$temp[tr0$median_r0 == max(tr0$median_r0)]
  
  r0_fun <- cinterpolate::interpolation_function(tr0$temp, tr0$median_r0, type = "spline")
  r0_out <- r0_fun(data)
  
  return(r0_out)
}
