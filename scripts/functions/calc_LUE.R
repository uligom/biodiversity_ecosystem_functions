### Function -------------------------------------------------------------------
calc_LUE <- function(data, site, year, Pfilt = 0.1, Pfilt_time = 24, SWfilt = 50)
{
  ## Utilities ----
  require(dplyr)
  require(purrr)
  require(quantreg) # quantile regression
  require(tidyr)
  
  
  ## Quote & settings ----
  if (is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!is_empty(year)) {
    site_year <- paste0("site-year ", site, "-", as.character(year))
  }
  
  
  
  ### Processing ----
  print(glue::glue("....computing LUE for {site_year}."))
  
  
  ## Filtering ----
  # Precipitation filter
  data <- bigleaf::filter.data(
    data.frame(data), quality.control = F, filter.growseas = F, filter.precip = T,
    GPP = "GPP", doy = "DOY", year = "YEAR", tGPP = GSfilt,
    precip = "Precip", tprecip = Pfilt, precip.hours = Pfilt_time, records.per.hour = 2,
    vars.qc = c("TA", "H", "LE", "NEE", "RH"), quality.ext = "_QC", good.quality = QCfilt)
  
  # Filter daytime for GPP
  data <- data %>% 
    dplyr::filter(SW_IN > SWfilt) # filter instead of replacing NA only for GPP
  
  # Subset for calculations and omit NAs
  data <- data %>% 
    dplyr::select(PAR, GPP) %>% 
    tidyr::drop_na() # important to remove NA for quantile regression
  
  
  ## Calculate EFPs ----
  if (nrow(data) == 0) { # if no data is available after filtering
    warning(glue::glue("The {site_year} was skipped because of empty data."))
    output <- NA_real_
    
    
  } else if (nrow(data) != 0) { # if dataframe is not empty
    ## Calculate LUE
    output <- data %>%
      # group_by(!!year) %>% 
      mutate(LUE = GPP / PAR, # Yan, P., Fernández-Martínez, M., Van Meerbeek, K., Yu, G., Migliavacca, M., & He, N. (2023). The essential role of biodiversity in the key axes of ecosystem function. Global Change Biology, 29(16), 4569–4585. https://doi.org/10.1111/gcb.16666
                .groups = "drop" # in case any grouping is inherited
                ) %>% 
      summarise(LUE = mean(LUE, na.rm = T)) %>% 
      pull()
    
    ## Output ----
    return(output)
  } # end condition of empty data
}



### Debug ----------------------------------------------------------------------
# debugonce(calc_LUE)