#### EXTRACT AND MERGE DATA FOR MULTIMODEL INFERENCE AND FURTHER ANALYSES

### Authors: Ulisse Gomarasca
### Version History ------------------------------------------------------------
# v02, 19.09.23   # Updated RaoQ calculation; included LAI estimates
# v03, 18.10.23   # Updated ground biodiversity calculation (1 m2 low fraction coverage filtered)
# v04, 19.10.23   # Updated ground biodiversity calculation
# v05, Nov.2023   # Corrected flux growing season filter



### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_in <- "v06" # EFP input version
biodiv_in <- "v06"
vers_out <- paste0(vers_in, ".", stringr::str_extract(biodiv_in, "[:digit:]{2}"))



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(janitor)      # clean names
library(readr)        # read table format files
library(stringr)      # string manipulation
library(tidyr)        # clean and reshape tidy data

## Functions
source("scripts/functions/extract_biodiv_metrics.R")



### Data -----------------------------------------------------------------------
## EFPs
by_years <- F #as.logical(readline(prompt = "Run analysis by years? T/F:")) # ask if analysis should run on whole sites' timeseries or on single site-years
if (by_years) {groupin <- paste0("_ByYears")} else {groupin <- ""}
efps <- read_csv(glue::glue("data/inter/efps_neon{groupin}_{vers_in}.csv"), show_col_types = F) %>%
  dplyr::select(!contains("pval")) %>%  # exclude p-values of CUEeco metrics
  glimpse()

## Biodiversity
biodiv <- extract_biodiv_metrics(biodiv_indices = T, phenology = F, raoq = T, biodiv_in = biodiv_in) %>% # NB: RaoQ metrics are here quality filtered
  select(-ground_phen_std) %>% 
  glimpse()

## Climate
clim <- read_csv("data/inter/climate_neon.csv", show_col_types = F) %>%
  janitor::clean_names() %>% dplyr::rename(SITE_ID = site_id, SW_in = s_win, VPD = vpd) %>% 
  glimpse()

## Soil
soil <- read_csv("data/input/NEON/NEON_soil/mean_swc_sites.csv", show_col_types = F) %>% 
  dplyr::rename(SITE_ID = NEON_ID) %>% glimpse()

## Structure
struct <- read_csv("data/input/NEON/NEON_struct-plant/NEONstructure_extracted_v03.csv", show_col_types = F) %>% # from 'extract_structure_NEON.R' script
  dplyr::select(-measurementHeight) %>% # remove meta-measurement
  janitor::clean_names() %>% dplyr::rename(SITE_ID = site_id) %>% 
  glimpse()

## LAI
lai <- read_csv("data/input/NEON/LAI/LAI_GBOV_NEON_extracted.csv", show_col_types = F) %>% # from 'extract_LAI_GBOV_NEON.R' script
  select(SITE_ID, LAImax_Miller_down) %>% # underestimates LAI as only green tissues are considered (GAI = Green Area Index)
  dplyr::rename(LAImax = LAImax_Miller_down) %>% 
  glimpse()

## NDVImax
vimax <- read_csv("data/input/Sentinel2/RaoQ/raoQ_S2_L2A_filtered_averaged.csv", show_col_types = F) %>%
  select(SITE_ID, NDVImax, NIRvmax) %>% 
  glimpse()

## IGBP class
igbp <- read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F)


# ## Check site availability
# bind_rows(
#   clim %>% select(SITE_ID) %>% mutate(category = "climate"),
#   biodiv %>% filter(!stringr::str_detect(SITE_ID, "-")) %>% select(SITE_ID) %>% mutate(category = "biodiversity"),
#   efps %>% select(SITE_ID) %>% mutate(category = "EFPs"),
#   struct %>% select(SITE_ID) %>% mutate(category = "structure"),
#   soil %>% select(SITE_ID) %>% mutate(category = "soil"),
# ) %>% 
#   glimpse() %>% 
#   ggplot(aes(x = category, y = SITE_ID)) +
#   geom_point(size = 2) +
#   xlab("") +
#   theme_bw()



### Process data ---------------------------------------------------------------
## Merge data ----
dat <- biodiv %>% # biodiversity metrics
  left_join(efps, by = "SITE_ID") %>% # Ecosystem Functional Properties from Eddy covariance measurements
  left_join(clim, by = "SITE_ID") %>% # Climatic variables from Eddy covariance stations
  left_join(soil, by = "SITE_ID") %>% # Soil water content from Eddy covariance stations
  left_join(struct, by = "SITE_ID") %>% # Structural variables from NEON sites
  left_join(lai, by = "SITE_ID") %>% # LAImax from NEON sites
  left_join(vimax, by = "SITE_ID") %>% # NDVImax from NEON sites (max of dates, median of pixels per date)
  left_join(igbp, by = "SITE_ID") %>% # IGBP pft class
  relocate(IGBP, .after = SITE_ID) %>% 
  glimpse()


## Filter sites ----
dat <- dat %>% 
  dplyr::filter(str_detect(SITE_ID, "[:upper:]{4}")) %>% # select NEON sites
  glimpse()


## Clean variable names ----
dat <- dat %>% 
  dplyr::rename(
    Rao_Q_S2 = RaoQ_S2_median,
    Rao_Q_NDVI = RaoQ_NDVI_S2_median,
    Rao_Q_NIRv = RaoQ_NIRv_S2_median,
    GPP_sat = GPPsat,
    NEP_95 = NEP95,
    NEP_99 = NEP99,
    Gs_max = Gsmax,
    CUE_eco_10 = CUEeco10,
    CUE_eco_50 = CUEeco50,
    CUE_eco_90 = CUEeco90,
    # I_WUE = IWUE,
    # u_WUE = uWUE,
    # WUE_t = WUEt,
    LAI_max = LAImax,
    NDVI_max = NDVImax,
    NIRv_max = NIRvmax
  ) %>% 
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  # Data
  write_csv(dat, glue::glue("data/inter/data_efps_clim_struct_biodiv_{vers_out}.csv"))
}



### Vector names ---------------------------------------------------------------
efps_names <- c("GPP_sat", "NEP_95", "NEP_99", "Gs_max", "CUE_eco_10",
                "CUE_eco_50", "CUE_eco_90", "IWUE", "uWUE", "WUE", "WUEt") %>% sort()
clim_names <- c("precip", "temp", "SW_in", "VPD", "SWC") %>% sort()
struct_names <- dat %>% dplyr::select(any_of(struct %>% dplyr::select(-SITE_ID) %>% names())) %>% names() %>% c("LAI_max", "NDVI_max", "NIRv_max") %>% sort()
ground_biodiv_names <- c("n_spp", "shannon", "pielou_evenness", "simpson_diversity", "bray_curtis") %>% sort()
sat_biodiv_names <- dat %>% names() %>% str_subset("Rao_Q") %>% sort() %>% c("sat_phen_std")
biodiv_names <- c(ground_biodiv_names, sat_biodiv_names) %>% sort()


## Save ----
if (savedata) {
  save(efps_names, file = glue::glue("data/inter/efps_names_{vers_out}.RData"))
  save(clim_names, file = glue::glue("data/inter/clim_names_{vers_out}.RData"))
  save(struct_names, file = glue::glue("data/inter/struct_names_{vers_out}.RData"))
  save(ground_biodiv_names, file = glue::glue("data/inter/ground_biodiv_names_{vers_out}.RData"))
  save(sat_biodiv_names, file = glue::glue("data/inter/sat_biodiv_names_{vers_out}.RData"))
  save(biodiv_names, file = glue::glue("data/inter/biodiv_names_{vers_out}.RData"))
}