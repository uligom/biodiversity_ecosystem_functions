### Function -------------------------------------------------------------------
calc_WUEt <- function(data, site, year,
                      QCfilt = c(0, 1), GSfilt = 0.3, Pfilt = 0.1, Pfilt_time = 24, SWfilt = 200, USfilt = 0.2
                      )
{
  
  
  ## Utilities ----
  require(bigleaf)
  require(dplyr)
  require(tidyr)
  
  
  ## Quote & settings ----
  if (is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!is_empty(year)) {
    site_year <- paste0("site-year ", site, "-", as.character(year))
  }
  
  
  ### Processing ----
  print(glue::glue("....computing WUEt for {site_year}."))
  
  
  
  ## Filtering ----
  # Quality, Growing Season and Precipitation filter
  data <- bigleaf::filter.data(
    data.frame(data), quality.control = T, filter.growseas = T, filter.precip = T,
    GPP = "GPP", doy = "DOY", year = "YEAR", tGPP = GSfilt,
    precip = "Precip", tprecip = Pfilt, precip.hours = Pfilt_time, records.per.hour = 2,
    vars.qc = c("TA", "H", "LE", "NEE", "RH"), quality.ext = "_QC", good.quality = QCfilt)
  
  # Radiation and u* filter
  data <- data %>% dplyr::filter(SW_IN > SWfilt & USTAR > USfilt)
  
  # Using only measured data: removing NA for wind speed
  data <- data %>% tidyr::drop_na(WS)
  
  # Subset for calculations and omit NAs
  data_subset <- data %>% 
    dplyr::select(DATETIME, YEAR, DOY, GPP, TEA_T_NT_75, ET, ratio_T_outlier_IMP, ratio_T_outlier_NEG) %>% 
    tidyr::drop_na()
  
  
  ## Calculate EFP ----
  if (nrow(data_subset) == 0) { # if no data is available after filtering
    warning(glue::glue("The {site_year} was skipped because of empty data."))
    output <- NA_real_
    
    
  } else if (nrow(data_subset) != 0) { # if dataframe is not empty
    ## Exclude sites with low TEA model performance
    # T > ET for long periods (model did not learn properly)
    # T is often negative (GPP partitioning probably did not work properly --> GPP_QC?)
    if (unique(data$ratio_T_outlier_IMP) > 0.2 | unique(data$ratio_T_outlier_NEG) > 0.2) {
      warning(glue::glue("The {site_year} was skipped because evaluation of the transpiration partitioning did not satisfy quality conditions (too many T values above ET and/or too many negative T values)."))
      output <- NA_real_
      
      
    } else { # if TEA data is valid
      ## Calculate daily WUEt ----
      dat_daily <- data_subset %>% 
        dplyr::group_by(YEAR, DOY) %>%
        dplyr::mutate(WUEt_daily = GPP / TEA_T_NT_75) %>% # daily WUEt for outlier filtering and further calculation of integrated measure
        dplyr::ungroup()
      
      
      ## Filter outliers ----
      dat_filt <- dat_daily %>% 
        dplyr::mutate(WUEt_daily_mean = mean(WUEt_daily, na.rm = T),
                      WUEt_daily_sd = sd(WUEt_daily, na.rm = T),
                      WUEt_upper = WUEt_daily_mean + 3 * WUEt_daily_sd, # threshold = mean + 5 or 3 * std
                      WUEt_lower = WUEt_daily_mean - 3 * WUEt_daily_sd
        ) %>%
        dplyr::filter(WUEt_daily < WUEt_upper & WUEt_daily > WUEt_lower)
      
      
      ## Calculate WUEt ----
      output <- dat_filt %>% 
        dplyr::mutate(T_sum = sum(TEA_T_NT_75, na.rm = T), # sum of T for reference and to avoid dividing by 0
                      WUEt = if_else(T_sum != 0, sum(GPP, na.rm = T) / sum(TEA_T_NT_75, na.rm = T), NA_real_) # calculate cumulative WUEt while avoiding dividing by 0 when no T data is available for the site # WUEt around peak of APAR
                      # i.e. ratio of sums; equivalent to ratio of means, while mean of ratios results in a biased aggregated metric.
        ) %>%
        dplyr::summarise(WUEt = unique(WUEt), # cumulative WUEt
                         WUEet = mean(GPP/ET), # ET-based WUE for reference
                         # GPP_NT_sum = sum(GPP, na.rm = T),   # sum of GPP for reference
                         # GPP_NT_mean = mean(GPP, na.rm = T), # mean of GPP for reference
                         T_sum = unique(T_sum),   # sum of T for reference
                         # T_mean = mean(ET_T_NT, na.rm = T), # mean of T for reference
                         # WUEtmax = quantile(WUEt_daily, 0.90, na.rm = T) # maximum WUEt
        ) %>%
        # glimpse() %>%
        dplyr::pull(WUEt)
      
    } # end of condition of invalid TEA data
  } # end condition of empty data
  
  ### Output ----
  return(output)
}



### Debug ----------------------------------------------------------------------
# debugonce(calc_WUEt)