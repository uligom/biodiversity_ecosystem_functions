#### Function for filtering, uWUE calculation (Mirco Migliavacca)
# reorganized and improved, added additional information in output (Ulisse Gomarasca)

## Inputs:
# data    = dataframe containing fluxnet data for single sites
# site    = character, name of the site
# QCfilt  = Integer or vector of integers from 0 to 4 corresponding to Quality
#           Checks to be retained. E.g. GQfilt = c(0, 1) filters out bad quality
#           data with quality flag set to 2, 3, or 4.
# GSfilt  = Number between 0 and 1, corresponding to Growing Season filter, i.e.
#           the relative minimum threshold (e.g. 0.3 = 30%) to be excluded from the
#           range of the GPP data.
# Pfilt   = Precipitation threshold used to identify a precipitation event (mm) (from filter.data function).
# SWfilt  = Value of minimum threshold for ShortWave filter, e.g. to exclude for
#           nighttime data below 20, 100 or 200.
# USfilt  = Value of minimum threshold for USTAR filter.


### Function
calc_WUE <- function(data, site, year,
                     QCfilt = c(0, 1), GSfilt = 0.3, Pfilt = 0.1, Pfilt_time = 24, SWfilt = 200, USfilt = 0.2
                     )
{
  
  
  ## Utilities ----
  require(bigleaf)
  require(dplyr)
  require(rlang)
  require(tidyr)
  
  
  ## Quote & settings ----
  if (is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!is_empty(year)) {
    site_year <- paste0("site-year ", site, "-", as.character(year))
  }
  
  
  ### Processing ----
  print('....computing WUE Metrics for {site_year}.')
  
  
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
    dplyr::select(DATETIME, GPP, NEE, LE, VPD, TA) %>% 
    tidyr::drop_na()
  
  
  ## Convert VPD to kPa ----
  data_subset <- data_subset %>% 
    dplyr::mutate(VPD_kPa = VPD / 10) # convert units of VPD from [hPa] to [kPa]
  
  message("NB: converted VPD units from hPa to kPa.")
  
  
  ## Calculate EFPs ----
  if (nrow(data_subset) == 0) { # if no data is available after filtering
    warning(glue::glue("The {site_year} was skipped because of empty data."))
    output <- tibble(
      WUE = NA_real_,
      WUE_NEE = NA_real_,
      IWUE = NA_real_,
      uWUE = NA_real_
    )
    
    
  } else if (nrow(data_subset) != 0) { # if dataframe is not empty
    wue_metrics <- data_subset %>% 
      ## Calculate WUE metrics
      WUE.metrics(GPP = "GPP", NEE = "NEE", LE = "LE", VPD = "VPD", Tair = "TA",
                  constants = bigleaf.constants())
    
    output <- tibble(var = names(wue_metrics), value = wue_metrics) %>% # convert to tibble
      tidyr::pivot_wider(names_from = var, values_from = value)
  }
  
  
  ### Output ----
  return(output)
}
# debugonce(calc_WUE)