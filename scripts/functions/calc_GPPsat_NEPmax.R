### Function -------------------------------------------------------------------
calc_GPPsat_NEPmax <- function(data, site, year,
                               QCfilt = c(0, 1), GSfilt = 0.3, SWfilt = 100, USfilt = 0.2, GPPsatfilt = 60
                               )
  {
  
  
  ## Utilities ----
  require(bigleaf)
  require(dplyr)
  require(purrr)
  require(rlang)
  require(tidyr)
  source("scripts/functions/myLRC.R")
  
  ## Quote & settings ----
  if (is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!is_empty(year)) {
    site_year <- paste0("site-year ", site, "-", as.character(year))
  }
  
  
  ### Processing ----
  print(glue::glue("....computing LRC parameters for {site_year}."))
  
  
  ## Filtering ----
  # Quality, Growing Season and Precipitation filter
  data <- bigleaf::filter.data(
    data.frame(data), quality.control = T, filter.growseas = T, filter.precip = T,
    GPP = "GPP", doy = "DOY", year = "YEAR", tGPP = GSfilt,
    precip = "Precip", tprecip = Pfilt, precip.hours = Pfilt_time, records.per.hour = 2,
    vars.qc = c("TA", "H", "LE", "NEE", "RH"), quality.ext = "_QC", good.quality = QCfilt)
  
  # Filter daytime for GPP & u*
  data <- data %>% dplyr::filter(SW_IN > SWfilt & USTAR > USfilt) # filter instead of replacing NA only for GPP (as in previous script), but moved generation of 5-days windows before
  
  # Subset for calculations and omit NAs
  data_subset <- data %>% 
    dplyr::select(DATETIME, NEE, PPFD, RECO, FiveDaySeq) %>% 
    tidyr::drop_na()
  
  
  ## Calculate EFPs ----
  if (nrow(data_subset) == 0) { # if no data is available after filtering
    warning(glue::glue("The {site_year} was skipped because of empty data."))
    output <- tibble(GPPsat = NA_real_, NEP95 = NA_real_, NEP99 = NA_real_)
    
    
  } else if (nrow(data_subset) != 0) { # if dataframe is not empty
    output <- data_subset %>% 
      dplyr::group_by(FiveDaySeq) %>% 
      tidyr::nest(data5days = c(DATETIME, NEE, PPFD, RECO)) %>% 
      dplyr::mutate(GPPsat = purrr::map_dbl(.x = data5days, .f = myLRC), # calculate GPPsat at 5 days windows
                    GPPsat = if_else(GPPsat < GPPsatfilt, GPPsat, NA_real_) # remove outliers
                    ) %>% 
      ungroup() %>% 
      unnest(cols = data5days) %>% 
      mutate(NEP = -NEE) %>% # take opposite; additional daytime filter not necessary because already filtered: if_else(PPFD > SWfilt * 2 * 2.11, -NEE, NA_real_)) %>% # exclude night values for daily NEP (NB: stricter (x2) than for GPPsat! is this correct?)
      summarise(GPPsat = quantile(GPPsat, 0.95, na.rm = T), # 95th GPPsat quantile (by years if selected)
                NEP95 = quantile(NEP, 0.95, na.rm = T), # 95th NEP quantile
                NEP99 = quantile(NEP, 0.99, na.rm = T) # 99th NEP quantile
      )
  } 
  
  
  ### Output ---------
  return(output)
}


### Debug ----------------------------------------------------------------------
# debugonce(calc_GPPsat_NEPmax)