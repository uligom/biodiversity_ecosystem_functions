#### EXTRACT CLIMATE

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {
  vers_out <- "v01"
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_calculate_EFPs_{vers_out}.txt")
}



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plots
# library(purrr)      # map functions
library(readr)      # read table format files
# library(REddyProc)  # eddy covariance functions
# library(rlang)      # quoting inside functions
# library(tictoc)     # measure time
library(tidyr)      # clean and reshape tidy data



### Data -----------------------------------------------------------------------
load(file = "data/input/NEON/fluxes/NCAR-NEON_fluxes_expanded.RData") # load NCAR-NEON fluxes ('dat')
dat %>% glimpse()

sites <- dat %>% pull(SITE_ID) %>% unique()



### Calculate mean climate -----------------------------------------------------
# Calculate mean climatic variables on each site (and year if specified)
## Define computation grouping strategy ----
while (T) {
  group_year <- F#as.logical(readline(prompt = "Extract climate for site-years (T) or whole sites (F)? T/F:")) # ask if sites or site-years
  if (group_year) {
    txt <- "==> Extracting mean climatic variables for each site-year."
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    break
  } else if (!group_year) {
    # grouping_var <- NULL
    txt <- "==> Extracting mean climatic variables for each site."
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    break
  } else {
    print("Invalid input. Specify 'TRUE' or 'FALSE'")
    next
  }
}


## Calculations ----
# Filter thresholds
QCfilt <- c(0, 1) # quality filter

dat_clim <- dat %>% 
  dplyr::group_by(SITE_ID, YEAR) %>%
  summarise( # calculate yearly means
    Precip = if_else(Precip_QC %in% QCfilt, Precip, NA_real_) %>% # filter quality
      sum(na.rm = T) * 60 * 30, # convert [mm/s] to cumulative [mm/half-hour]   # cumulative annual precipitation [mm/y]
    Temp = if_else(TA_QC %in% QCfilt, TA, NA_real_) %>% mean(na.rm = T),        # mean temperature [°C]
    SWin = if_else(SW_IN_QC %in% QCfilt, SW_IN, NA_real_) %>% mean(na.rm = T),  # mean incident shortwave radiation (incl. night time) [W/m^2]
    VPD = mean(VPD, na.rm = T), # NB: no quality flag available for VPD!        # mean vapor pressure deficit [hPa]
    .groups = "drop"
  ) %>%
  glimpse()

hist(dat_clim$Precip)
hist(dat_clim$Temp)
hist(dat_clim$SWin)
hist(dat_clim$VPD)

if (!group_year) {
  dat_clim <- dat_clim %>% 
    dplyr::group_by(SITE_ID) %>%
    dplyr::summarise( # mean of annual values
      Precip = mean(Precip, na.rm = T),
      Temp = mean(Temp, na.rm = T),
      SWin = mean(SWin, na.rm = T),
      VPD = mean(VPD, na.rm = T)
    ) %>% 
    glimpse()
  
  hist(dat_clim$Precip)
  hist(dat_clim$Temp)
  hist(dat_clim$SWin)
  hist(dat_clim$VPD)
}



### Save -----------------------------------------------------------------------
if (savedata) {
  if (!group_year) {groupin <- ""} else if (group_year) {groupin <- "_ByYears"}
  write_csv(x = dat_clim, file = glue::glue("data/inter/climate_neon{groupin}.csv"))
}