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
# partit  = unused, character, either "GPP_NT" or "GPP_DT"
#         NB: RECO_DT??? instead of RECO_NT?


calc_uWUE <- function(data, site,
                      QCfilt = c(0, 1), GSfilt = 0.3, Pfilt = 0.1, SWfilt = 200, USfilt = 0.2)
{
  ## Packages:
  require(bigleaf)
  require(dplyr)
  require(lubridate)
  require(rlang)
  require(tidyr)
  
  print('....computing uWUE Metrics for the site')
  
  
  ## Filter data
  # Qui il problema principale e’ che nel vecchio dataset LaThuile non tutti i siti avevano precipitazione
  # e per alcuni siti il pluviometro dava precipitazione costsnte, che non ha senso (probabilmente qualche gap filling).
  # Per quei siti non ho usato il filtro della precipitazione. In teoria possiamo anche levarli ma per non rimuovere
  # troppi siti li ho tenuti. Farei lo stesso. Va ovviamente messa poi una linea nei metodi.
  # i siti sono tutti quelli che hanno problemi nel dataset che ho usato, potrebbero mancarne alcuni. Cit. Mirco
  if(is.na(summary(data$P)[4]) | (summary(data$P)[4] == 0) | 
     (site == "AU-Tum") | (site == "BW-Ghg") | (site == "AU-Whr") |
     (site == "US-MMS") | (site == "US-Cop") | (site == "US-FR2") | 
     (site == "US-Ha1") | (site == "US-Ha2") | (site == "US-LPH") | 
     (site == "US-Ne1") | (site == "US-Ne2") | (site == "US-Ne3") | 
     (site == "US-UMB")){
    print("##### Warning: no Precipitation data!!! No precipitation Filter")
    dat.filtered <- filter.data(data.frame(data), quality.control = TRUE, filter.growseas = TRUE, filter.precip = FALSE, 
                                GPP = "GPP_NT", doy = "DOY", year = "YEAR", tGPP = GSfilt,
                                precip = "P", tprecip = Pfilt, precip.hours = 24, records.per.hour = 2,
                                vars.qc = c("TA", "H", "LE", "NEE", "VPD"), quality.ext = "_QC", good.quality = QCfilt)
    precipFilter <- "no"
  } else if (!is.na(summary(data$P)[4]) & (summary(data$P)[4] != 0) & 
     (site != "AU-Tum") & (site != "BW-Ghg") & (site != "AU-Whr") &
     (site != "US-MMS") & (site != "US-Cop") & (site != "US-FR2") & 
     (site != "US-Ha1") & (site != "US-Ha2") & (site != "US-LPH") & 
     (site != "US-Ne1") & (site != "US-Ne2") & (site != "US-Ne3") &  
     (site != "US-UMB")){
    print("#### Precipitation data available!!! Precipitation Filter Active")
    #browser()
    # if (site == "AU-Tum") record.per.hour <- 1
    # if (site != "AU-Tum") record.per.hour <- 2
    record.per.hour <- 2 # set to 2 for every data input, since import was modified to handle hourly data as well
    
    dat.filtered <- filter.data(data.frame(data), quality.control = TRUE, filter.growseas = TRUE, filter.precip = TRUE, 
                                GPP = "GPP_NT", doy = "DOY", year = "YEAR", tGPP = GSfilt,
                                precip = "P", tprecip = Pfilt, precip.hours = 24, records.per.hour = record.per.hour,
                                vars.qc = c("TA", "H", "LE", "NEE", "VPD"), quality.ext = "_QC", good.quality = QCfilt)
    precipFilter <- "yes"
  }
  
  
  # For G1 and WUE I remove also data with low u*
  aaa <- subset(dat.filtered, SW_IN > SWfilt & USTAR > USfilt)
  
  if (nrow(aaa) != 0) {
    uWUE_out <- WUE.metrics(aaa, GPP = "GPP_NT", NEE = "NEE", LE = "LE", VPD = "VPD_kPa",
                           Tair = "TA", constants = bigleaf.constants()) # calculate WUE metrics
    uWUE_out <- tibble(var = names(uWUE_out), value = uWUE_out) %>% 
      tidyr::pivot_wider(names_from = var, values_from = value)     # convert to tibble
    uWUE_out$ET_95 <- quantile(aaa$ET, 0.95, na.rm = T) %>% unname() # append 95th quantile of ET
    
    uWUE_out$precipFilter <- precipFilter # append precipitation filter option
    
    uWUE_out <- uWUE_out %>% mutate(SITE_ID = site, .before = uWUE)
    
    ## Output ----
    return(uWUE_out)
  } else {
    return(tibble(SITE_ID     = site,
                  WUE         = NA_real_,
                  WUE_NEE     = NA_real_,
                  IWUE        = NA_real_,
                  uWUE        = NA_real_,
                  ET_95       = NA_real_,
                  precipFilter = NA_character_)
    )
  }
}
# debugonce(calc_uWUE)