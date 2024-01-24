#### EXTRACT AND SAVE THE NCAR-NEON PROCESSED FLUX DATA

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Start
library(tictoc)
tic("Script run time.")

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plots
library(lubridate)  # dates
library(ncdf4)      # netcdf files
library(readr)
library(REddyProc)  # eddy covariance functions
library(stringr)    # tidy string manipulation
library(tidyr)      # clean and reshape tidy data

## Functions
source("C:/Users/ugomar/Desktop/B_EF/scripts/functions/plot_timeseries.R")



### Data -----------------------------------------------------------------------
## Sites
sites <- str_extract(list.files("//minerva/BGI/data/FLUXCOM/data_vault/NEON_prelim_202212/"),
                     pattern = "[:upper:]+")


## Extract data
dat <- tibble() # initialize

for (h in 1:length(sites)) {
  dat_netcdf <- ncdf4::nc_open(filename = glue::glue("//minerva/BGI/data/FLUXCOM/data_vault/NEON_prelim_202212/{sites[h]}.nc")) # (thanks Jake); raw (thanks Zayd): "//minerva/BGI/scratch/zmhamdi/for_ulisse/ABBY_atm_2018-01.nc")
  
  names(dat_netcdf$var)
  
  longnames <- c() # initialize
  unitss <- c()
  for (i in 1:length(names(dat_netcdf$var))) {
    varname <- glue::glue("{names(dat_netcdf$var)}")[i] %>% rlang::sym()
    longnames[i] <- dat_netcdf$var[[varname]][["longname"]]
    unitss[i] <- dat_netcdf$var[[varname]][["units"]]
  }
  
  # NCAR-NEON variables
  vars_neon <- tibble(varname = names(dat_netcdf$var),
                      longname = longnames,
                      unit = unitss) %>% 
    add_row(varname = dat_netcdf$dim$time$name, unit = dat_netcdf$dim$time$units, .before = 1)
  
  rm(i, longnames, varname, unitss) # clean environment
  
  timestart <- str_extract(vars_neon[1,3], "20[:digit:]{2}-[:digit:]{2}-[:digit:]{2}")
  
  
  ### Extract variables --------------------------------------------------------
  dat <- bind_rows(
    dat,
    tibble(
      ## Site:
      SITE_ID = sites[h],
      ## Time:
      TIME = dat_netcdf$dim$time$vals, # time dimension (minutes since YYYY-MM-DD 00:00:00)
      ## Coordinates:
      LATITUDE = ncvar_get(dat_netcdf, varid = "lat"),  # latitude (degrees north)
      LONGITUDE = ncvar_get(dat_netcdf, varid = "lon"), # latitude (degrees east)
      ## Fluxes:
      GPP = ncvar_get(dat_netcdf, varid = "GPP"),        # gross primary productivity (umolm-2s-1)
      H = ncvar_get(dat_netcdf, varid = "FSH"),          # sensible heat flux (Wm-2)
      LE = ncvar_get(dat_netcdf, varid = "EFLX_LH_TOT"), # latent heat flux (Wm-2)
      NEE = ncvar_get(dat_netcdf, varid = "NEE"),        # net ecosystem exchange (umolm-2s-1)
      ## Atmospheric:
      LW_IN = ncvar_get(dat_netcdf, varid = "FLDS"),  # incident longwave (W/m^2)
      Precip = ncvar_get(dat_netcdf, varid = "PRECTmms"),  # precipitation (mm/s)
      PA = ncvar_get(dat_netcdf, varid = "PSRF"),     # pressure at the lowest atmospheric level (Pa)
      NETRAD = ncvar_get(dat_netcdf, varid = "Rnet"), # net radiation (W/m^2)
      RH = ncvar_get(dat_netcdf, varid = "RH"),       # relative humidity at lowest atm level (%)
      SW_IN = ncvar_get(dat_netcdf, varid = "FSDS"),  # incident shortwave (W/m^2)
      TA = ncvar_get(dat_netcdf, varid = "TBOT"),     # temperature at lowest atm level (K)
      USTAR = ncvar_get(dat_netcdf, varid = "Ustar"), # friction velocity (m/s)
      WS = ncvar_get(dat_netcdf, varid = "WIND"),     # wind at lowest atm level (m/s)
      ## Quality flags:
      GPP_QC = ncvar_get(dat_netcdf, varid = "GPP_fqc"),        # gross primary productivity gap-filling flag
      H_QC = ncvar_get(dat_netcdf, varid = "FSH_fqc"),          # sensible heat flux gap-filling flag
      LE_QC = ncvar_get(dat_netcdf, varid = "EFLX_LH_TOT_fqc"), # latent heat flux gap-filling flag
      LW_IN_QC = ncvar_get(dat_netcdf, varid = "FLDS_fqc"),     # incident longwave (FLDS) gap-filling flag
      NEE_QC = ncvar_get(dat_netcdf, varid = "NEE_fqc"),        # net ecosystem exchange gap-filling flag
      # NEE_QC = ncvar_get(dat_netcdf, varid = "qfNEE"),          # net ecosystem exchange final quality flag, 0 = good data and 1 = flagged data
      NETRAD_QC = ncvar_get(dat_netcdf, varid = "Rnet_fqc"),    # net radiation gap-filling flag
      Precip_QC = ncvar_get(dat_netcdf, varid = "PRECTmms_fqc"),# precipitation gap-filling flag
      PA_QC = ncvar_get(dat_netcdf, varid = "PSRF_fqc"),        # pressure at the lowest atmospheric level gap-filling flag
      RH_QC = ncvar_get(dat_netcdf, varid = "RH_fqc"),          # relative humidity at lowest atm level gap-filling flag
      SW_IN_QC = ncvar_get(dat_netcdf, varid = "FSDS_fqc"),     # incident shortwave (FSDS) gap-filling flag
      TA_QC = ncvar_get(dat_netcdf, varid = "TBOT_fqc"),        # temperature at lowest atm level gap-filling flag
      USTAR_QC = ncvar_get(dat_netcdf, varid = "Ustar_fqc"),    # friction velocity gap-filling flag
      WS_QC = ncvar_get(dat_netcdf, varid = "WIND_fqc")         # wind at lowest atm level gap-filling flag
    ) %>% 
      mutate(across(.cols = everything(), .fns = as.vector)) %>% # convert every column type (array) to vector
      mutate(TIME = TIME * 60,                                 # convert time from 'minutes from' to 'seconds from'
             DATETIME = as_datetime(TIME, origin = timestart), # generate date column from correct start for each site
      )
  )
}
dat %>% glimpse()



### Convert units format -------------------------------------------------------
dat <- dat %>%
  mutate(TA = TA - 273.15 # 'Kelvin' to 'Celsius'
  ) %>%
  relocate(DATETIME, .before = TIME) %>% 
  select(-TIME) %>%
  glimpse()



### Calculate missing variables ------------------------------------------------
nStepsPerDay <- 48

dat <- dat %>%
  ## Time:
  mutate(YEAR = year(DATETIME),
         DOY = yday(DATETIME),
         HOUR_decimal = (hour(DATETIME) * 60 + minute(DATETIME) + second(DATETIME)) / 60, # extract hours, minutes & seconds, and convert to decimal hour
         .after = DATETIME
  ) %>%
  ## Fluxes:
  mutate(RECO = GPP + NEE, # GPP = RECO - NEE # ecosystem respiration [umolm-2s-1]
         ET = REddyProc::fCalcETfromLE(LE = LE, Tair = TA), # evapotranspiration [mmol H20 m-2 s-1]
         # ET = LE.to.ET(dat$LE, dat$TA) * 1800 # Conversion from W/m2 to mmH20
         # ET = ET * 18.015 * 60 * 60 * 24 / nStepsPerDay / 1e6, # convert [mmol H20 m-2 s-1] to mm/timestep [mm / half-hour]
         .after = GPP
         ) %>%
  ## Climate:
  mutate(VPD = fCalcVPDfromRHandTair(rH = RH, Tair = TA),
         VPD_kPa = VPD / 10, # convert units of VPD from [hPa] to [kPa]
         PAR = SW_IN * 2.11, # calculation of PAR [umol m^-2 s^-1] from SW_IN [W m^-2]
         PPFD = SW_IN * 2.11, # photosynthetic photon flux density (PPFD) calculated as SW_IN * 2.11
         SW_IN_POT = fCalcPotRadiation(DoY = DOY, Hour = HOUR_decimal,
                                       LatDeg = LATITUDE, LongDeg = LONGITUDE, TimeZone = 0), # Timezone is UTC for all sites for the NEON-NCAR product (0)
         .after = USTAR
  ) %>%
  glimpse()



### Include partitioned Transpiration and Evaporation --------------------------
dat <- dat %>%
  left_join(
    read_csv("data/input/NEON/fluxes/TEA_transpiration_partitioned_fromJake.csv", show_col_types = F),
    # TEA_T_NT_75: transpiration [mm/half-hour]
    # TEA_E_NT_75: evaporation [mm/half-hour]
    by = c("SITE_ID", "DATETIME")
  ) %>%
  mutate(TEA_T_NT_75 = TEA_T_NT_75 / 18.015 / 60 / 60 / 24 * nStepsPerDay * 1e6, # convert to [mmol H20 m-2 s-1]
         TEA_E_NT_75 = TEA_E_NT_75 / 18.015 / 60 / 60 / 24 * nStepsPerDay * 1e6 # convert to [mmol H20 m-2 s-1]
  ) %>% 
  glimpse()


## Plot ----
p_daily <- dat %>% 
  select(SITE_ID, DATETIME, TEA_T_NT_75, ET) %>% 
  mutate(DATE = as_date(DATETIME)) %>% 
  group_by(SITE_ID, DATE) %>% 
  summarise(TEA_T_NT_75 = mean(TEA_T_NT_75, na.rm = T),
            ET = mean(ET, na.rm = T),
            .groups = "drop"
  ) %>% 
  pivot_longer(cols = c(TEA_T_NT_75, ET)) %>% 
  glimpse() %>% 
  ggplot(aes(x = DATE, y = value, color = name), alpha = 0.5) +
  geom_line() +
  facet_wrap(. ~ SITE_ID) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 13),
        strip.background = element_blank()
  ) +
  NULL
p_daily

if (savedata) {
  ggsave(filename = "daily_T_vs_ET.jpg",
         plot = p_daily, device = "jpeg", path = "results/timeseries",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
}


## Add custom quality flags ----
dat <- dat %>% 
  group_by(SITE_ID) %>% 
  mutate(T_outlier_IMP = if_else(condition = TEA_T_NT_75 > ET, true = T, false = F),
         T_outlier_NEG = if_else(condition = TEA_T_NT_75 < 0, true = T, false = F),
         ratio_T_outlier_IMP = sum(T_outlier_IMP, na.rm = T) / n(), # ratio [0-1] of impossible T values (higher than ET)
         ratio_T_outlier_NEG = sum(T_outlier_NEG, na.rm = T) / n(), # ratio [0-1] of negative T values (below 0)
         ) %>% 
  ungroup() %>% 
  select(-T_outlier_IMP, -T_outlier_NEG) %>% 
  glimpse()



### Check data with plots ------------------------------------------------------
if (savedata) {savepath <- "results/timeseries"} else {savepath <- NA}

# dat <- dat %>% 
#   dplyr::filter(SITE_ID %in% c("GUAN", "LAJA", "PUUM", "SOAP")) %>% 
#   glimpse()

# dat %>% plot_timeseries(y = "NEE", color = "NEE_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "GPP", color = "GPP_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "RECO", color = "GPP_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "H", color = "H_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "LE", color = "LE_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "LW_IN", color = "LW_IN_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "SW_IN", color = "SW_IN_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "SW_IN_POT", color = "SW_IN_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "Precip", color = "P_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "PA", color = "PA_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "RH", color = "RH_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "NETRAD", color = "NETRAD_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "TA", color = "TA_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "USTAR", color = "USTAR_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "WS", color = "WS_QC", facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "VPD", color = NA, facet = "SITE_ID", savepath = savepath)
# dat %>% plot_timeseries(y = "ET", color = NA, facet = "SITE_ID", savepath = savepath)
dat %>% plot_timeseries(y = "TEA_T_NT_75", color = NA, facet = "SITE_ID", savepath = savepath)
dat %>% plot_timeseries(y = "TEA_E_NT_75", color = NA, facet = "SITE_ID", savepath = savepath)
# 
# # ## Everything in one plot
# # dat %>% 
# #   mutate(id = row_number(), .before = DATETIME) %>%
# #   glimpse() %>%
# #   pivot_longer(cols = !c(id, DATETIME, DOY, YEAR),# & !ends_with("_QC"),
# #                names_to = c("VARIABLE", ".value", "FLAG", ".value"),
# #                names_pattern = "(\\w+(_QC){0})(\\w+(_QC)+)",
# #                ) %>%
# #   glimpse() %>% 
# #   # pivot_longer(cols = ends_with("_QC"),
# #   #                     names_to = "FLAGNAME", values_to = "FLAGVALUE") %>%
# #   ggplot(aes(x = DATETIME, y = )) +
# #   NULL
#
#
#
### Save -----------------------------------------------------------------------
if (savedata) {
  save(dat, file = "data/input/NEON/fluxes/NCAR-NEON_fluxes_expanded.RData")
  write_csv(dat, file = "data/input/NEON/fluxes/NCAR-NEON_fluxes_expanded.csv")
}


### End ------------------------------------------------------------------------
toc()