require(tidyr)
require(zoo)
#Functions from Siraj et al (2017) for temperature-dependent GI
mortalityRT <- function(temp,fldcxn=fieldcorxn) {
  dd<-seq(0,120,length.out=(120*24+2))
  nwdd<-  data.frame(Days=dd,Temperature=rep(temp, (120*24+2)), Study_number=5, Feed_B=2, Feed_S=1) ## uses algam_85re.Rdata
  nwdd<-  cbind(nwdd,logDay=log(nwdd$Days+1), logTemp=log(nwdd$Temperature+1))  ## +1 avoids log(0) 
  prediction <-as.vector(unlist(stats::predict(algam,newdata = (nwdd), se.fit = TRUE, type = "response")$fit))
  prediction<- prediction[-1]
  prediction[1:24]<- prediction[1:24]/prediction[1]
  prediction[which(prediction>1)]<-1
  prediction[which(prediction<=0.001)]<-0
  diffDeath<- -diff(prediction)
  diffDeath<- diffDeath/sum(diffDeath)
  return(1/sum(dd[2:length(prediction)]*diffDeath) + fldcxn) 
}

#Intrinsic incubation period
iip_func <- function(iip_params = iip_params, 
                     age,
                     temp){
  iip_mu <- exp(exp(iip_params[1] + iip_params[2]*temp))
  iip_tau <- iip_params[3]
  tmp_iipdf = dlnorm(age, meanlog=log(iip_mu), sdlog=(1/sqrt(iip_tau)))
  if(age == 0){tmp_iipdf <- 0}
  return(tmp_iipdf)
}

#Human-to-mosquito period
hmtp_func <- function(age){
  barer = hinfectiousness(1)$histo ## histogram in Nishuara & Halstead (we are using 0:6, actually is -2:4 relative)
  stmle = mle.norm(barer)
  stpdf = dnorm(age,stmle[1],stmle[2])
  if(age == 0){stpdf <- 0}
  return(stpdf)
}


#Extrinsic incubation period
eip_func <- function(eip_params,
                     age,
                     temp){
  
  if(age != 0)
  {
    eip_mu <- exp(exp(eip_params[1] + eip_params[2]*temp))
    eip_tau <- eip_params[3]
    tmp_eipdf = dlnorm(age, meanlog=log(eip_mu), sdlog=(1/sqrt(eip_tau)))
  }
  else{
    tmp_eipdf <- 0
  }
  return(tmp_eipdf)
}

#Mosquito-to-human period
mhtp_function <- function(age,
                          temp){
  tmp_mortp <- mortalityRT(temp, fldcxn = fieldcorxn)
  mortp <- tmp_mortp
  tmp_mortdf = dexp(age, mortp)
  
  if(age == 0){tmp_mortdf <- 0}
  return(tmp_mortdf)
}

#Default baseline parameters from Siraj et al(2017)
set_siraj_2017_params <- function(){
  siraj_2017_params <- c(0.56, 0, 13.7, 
              2.9, -0.08, 4.9) 
  return(siraj_2017_params)
}



#Random number generator functions for distributions from
  # Siraj et al (2017)
rand_iip_func <- function(iip_params = iip_params, 
                          temp){
  iip_mu <- exp(exp(iip_params[1] + iip_params[2]*temp))
  iip_tau <- iip_params[3]
  sampled_iip = rlnorm(1, meanlog=log(iip_mu), sdlog=(1/sqrt(iip_tau)))
  return(sampled_iip)
}

rand_hmtp_func <- function(){
  barer = hinfectiousness(1)$histo ## histogram in Nishuara & Halstead (we are using 0:6, actually is -2:4 relative)
  stmle = mle.norm(barer)
  sampled_hmtp = rnorm(1,stmle[1],stmle[2])
  return(sampled_hmtp)
}

rand_eip_func <- function(eip_params,
                          temp){
  eip_mu <- exp(exp(eip_params[1] + eip_params[2]*temp))
  eip_tau <- eip_params[3]
  sampled_eip = rlnorm(1, meanlog=log(eip_mu), sdlog=(1/sqrt(eip_tau)))
  return(sampled_eip)
}

rand_mhtp_function <- function(temp){
  tmp_mortp <- mortalityRT(temp, fldcxn = fieldcorxn)
  sampled_mhtp = rexp(1, tmp_mortp)
  return(sampled_mhtp)
}


#Function to generate random samples of GI (which we're using for SI)
time_varying_monte_carlo_generator_function_temps <- function(number_samples,
                                                              fixed_t,
                                                              temps_dt,
                                                              params){
  #Inputs: number of MC samples, fixed_t = calendar time t, temperature data.table,
    # parameters for assumed distributions between stages
  iip_params <- params[c(1:3)]
  eip_params <- params[c(4:6)]
  tau <- rep(NA, number_samples)
  
  #These are aV values at time t
  aV_samples <- replicate(number_samples, 
                          rand_mhtp_function(temps_dt[which(TIME == fixed_t)]$av_temp))
  #Empty vectors to store samples + densities
  aW_samples <- rep(NA, number_samples)
  aI_samples <- rep(NA, number_samples)
  aE_samples <- rep(NA, number_samples)

  for(i in 1:number_samples){
    if(i %% 2500 ==0){gc()} #Memory, if asking for a lot of samples
    tmp_aV <- aV_samples[i] #This age will be used downstream
    #Rounding as only have temperature data at integer time points
    aW_samples[i] <- rand_eip_func(eip_params, 
                                   temps_dt[which(TIME == fixed_t-round(tmp_aV))]$av_temp)
    if(aW_samples[i] > (fixed_t - tmp_aV)){ #recall, we assume t>>a_W
      aW_samples[i] <- rand_eip_func(eip_params, 
                                     temps_dt[which(TIME == fixed_t-round(tmp_aV))]$av_temp)
    }
    tmp_aW <-  aW_samples[i]
    aI_samples[i] <- rand_hmtp_func()
    if(aI_samples[i] > (fixed_t - tmp_aV - tmp_aW)){
      aI_samples[i] <- rand_hmtp_func()
    }
    tmp_aI <- c(aI_samples[i])
    aE_samples[i] <-
      rand_iip_func(iip_params = iip_params,
                    temps_dt[which(TIME == max(round(fixed_t - tmp_aV - tmp_aW - tmp_aI), 1))]$av_temp)
    if(aE_samples[i] > (fixed_t - tmp_aV - tmp_aW - tmp_aI)){
      aE_samples[i]<-  rand_iip_func(iip_params = iip_params, 
                                     temps_dt[which(TIME == max(round(fixed_t - tmp_aV - tmp_aW - tmp_aI), 1))]$av_temp)
      
    }
    
    tmp_aE <- c(aE_samples[i])
    
    
    #Total delay tau = sample from w(t, tau)
    tau[i] <- tmp_aE + tmp_aW + tmp_aV + tmp_aI
  }
  tau < c(tau)
  return(tau)
}
weekly_to_daily_temps_function <- function(weekly_data){
    #Interpolate weekly temperature data to daily data
    #Ideally, would use daily data
    daily_df <- weekly_data %>% complete(date = seq(min(date), max(date), by = "day")) %>%
        mutate(EVALUATE = ifelse(date %in% weekly_data$date, TRUE, FALSE),
                av_temp = na.approx(av_temp, rule = 2))  # Interpolate missing values
    daily_df$TIME <- seq(1, nrow(daily_df))
    return(daily_df)
}

fitting_data_table_function <- function(all_data){
    # Current setup: 80 days of data to estimate the SI distribution
    # Will adapt when we get more data
    fitting_data <- subset(all_data, TIME >= 80)
    fitting_data$FITTING_TIME <-  seq(1, nrow(fitting_data))
    return(fitting_data)
}
run_time_varying_monte_carlo <- 
  function(number_mc_samples,
           all_data,
           fitting_data,
           siraj_2017_params){
            
    #all_data includes historical temperatures relevant for estimating current GI
    all_data <- data.table(all_data)
    fitting_data <- data.table(fitting_data)
    tau_samples_dt <- NULL
    all_evaluation_times_dt <- subset(fitting_data, EVALUATE == TRUE) #All time points at which we are evaluating the GI distribution
    all_evaluation_times <- all_evaluation_times_dt$TIME 
    for(i in 1:length(fitting_data$FITTING_TIME)){
      if(i %% 100 == 0){
        print(paste0("Fitting time: ", i, " of ", length(fitting_data$FITTING_TIME)))
      }
      #FITTING_TIME is index from 1 to number of rows of fitting data.table
      #Loop over calendar time t at which estimate GI
      tmp_fixed_t <- fitting_data$TIME[i] 
      evaluate_at_fixed_t <- fitting_data$EVALUATE[i] #Binary Indicator
      #Subset relevant temperatures      
      all_data[, TIME:= as.numeric(TIME)]
      fitting_data[, TIME:= as.numeric(TIME)]

      #Subset temperature up to current time t
      tmp_time_varying_temps_dt <- 
        subset(all_data, 
               TIME <= tmp_fixed_t)
      if(evaluate_at_fixed_t == TRUE){
        tau_samples <- 
          time_varying_monte_carlo_generator_function_temps(number_mc_samples,
                                                          tmp_fixed_t,
                                                          tmp_time_varying_temps_dt,
                                                          siraj_2017_params)
        tau_samples_dt <- cbind(tau_samples_dt, tau_samples)
        # print(ncol(tau_samples_dt))
      }

    }
    tau_dt <- as.data.table(tau_samples_dt)
    setnames(tau_dt, paste0("TIME_", all_evaluation_times))
    tau_dt[, SAMPLE:= seq(1, nrow(tau_dt))] #SAMPLE = Index for MC samples
    tau_dt <- melt(tau_dt, id.var = "SAMPLE")
    tau_dt[, variable:= gsub("TIME_", "", variable)] 
    tau_dt[, variable:= as.numeric(variable)]
    setnames(tau_dt, "variable", "TIME") #TIME = Date at which we are evaluating gen time distribution
    return(tau_dt)
}


weekly_si_from_daily_si_function <- function(tau_dt){
    #Convert weekly SI to daily SI
    #Currently a bit inefficient....
    weekly_si_dt <- tau_dt[, list(FRAC_IN_WEEK_1 = length(which(value <= 7))/length(value),   
                                 FRAC_IN_WEEK_2 = length(which(value > 7 & value <= 14))/length(value),
                                 FRAC_IN_WEEK_3 = length(which(value > 14 & value <= 21))/length(value),
                                 FRAC_IN_WEEK_4 = length(which(value > 21 & value <= 28))/length(value),
                                 FRAC_IN_WEEK_5 = length(which(value > 28 & value <= 35))/length(value),
                                 FRAC_IN_WEEK_6 = length(which(value > 35 & value <= 42))/length(value),
                                 FRAC_IN_WEEK_7 = length(which(value > 42 & value <= 49))/length(value),
                                 FRAC_IN_WEEK_8 = length(which(value > 49 & value <= 56))/length(value),
                                 FRAC_IN_WEEK_9 = length(which(value > 56 & value <= 63))/length(value),
                                 FRAC_IN_WEEK_10 = length(which(value > 63 & value <= 70))/length(value),
                                 FRAC_IN_WEEK_11 = length(which(value > 70 & value <= 77))/length(value),
                                 FRAC_IN_WEEK_12 = length(which(value > 77 & value <= 84))/length(value),
                                 FRAC_IN_WEEK_13 = length(which(value > 84 & value <= 91))/length(value),
                                 FRAC_IN_WEEK_14 = length(which(value > 91 & value <= 98))/length(value),
                                 FRAC_IN_WEEK_15 = length(which(value > 98 & value <= 105))/length(value),
                                 FRAC_IN_WEEK_16 = length(which(value > 105 & value <= 112))/length(value),
                                 FRAC_IN_WEEK_17 = length(which(value > 112 & value <= 119))/length(value),
                                 BEGINNING = length(which(value <= 0))), by= c("TIME")]
    return(weekly_si_dt)
}
#Create a function to weight past cases by the SI distribution using weekly_si_dt 
weight_cases <- function(weekly_si_dt, cases_dt){
  #Inputs: weekly_si_dt, cases_dt (weekly cases)
  #Outputs: weighted cases
  si_cases_dt <- merge(cases_dt, weekly_si_dt, by = "TIME", all.x = TRUE)

  #Probably a more efficient way to do this, but this was the most natural to me
  si_cases_dt[, weighted_lagged_cases := FRAC_IN_WEEK_1 * lag(cases, 1, default = 0) + FRAC_IN_WEEK_2 * lag(cases, 2, default = 0) + 
                FRAC_IN_WEEK_3 * lag(cases, 3, default = 0) + FRAC_IN_WEEK_4 * lag(cases, 4, default = 0) + FRAC_IN_WEEK_5 * lag(cases, 5, default = 0) + 
                FRAC_IN_WEEK_6 * lag(cases, 6, default = 0) + FRAC_IN_WEEK_7 * lag(cases, 7, default = 0) + FRAC_IN_WEEK_8 * lag(cases, 8, default = 0) + 
                FRAC_IN_WEEK_9 * lag(cases, 9, default = 0) + FRAC_IN_WEEK_10 * lag(cases, 10, default = 0) + FRAC_IN_WEEK_11 * lag(cases, 11, default = 0) + 
                FRAC_IN_WEEK_12 * lag(cases, 12, default = 0) + FRAC_IN_WEEK_13 * lag(cases, 13, default = 0) + FRAC_IN_WEEK_14 * lag(cases, 14, default = 0) + FRAC_IN_WEEK_15 * lag(cases, 15, default = 0)+
                FRAC_IN_WEEK_16 * lag(cases, 16, default = 0) + FRAC_IN_WEEK_17 * lag(cases, 17, default = 0)]
    return(si_cases_dt)
  
}
