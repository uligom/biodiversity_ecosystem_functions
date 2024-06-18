#### EXTRACT SITE STRUCTURE FOR NEON SITES
# Canopy height, LAImax, ...

# SOURCE DATA: 

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
# Data settings
savedata = as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

vers_out <- "v03"
# v02, 19.07.2023:  filtered measurements at tower plots; corrected aggregation
#                   of 'height'; improved step-wise aggregation for all variables;
#                   included non-woody vegetation
# v03, 28.07.2023:  mean height of singular individuals x events, then maximum.



### Utilities ------------------------------------------------------------------
library(dplyr); options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)
library(neonUtilities)
library(readr)
library(tictoc)
library(tidyr)



### Data -----------------------------------------------------------------------
# # Unzip data (only first time)
# neonUtilities::stackByTable(filepath = "data/input/NEON/NEON_struct-plant.zip",
#                             savepath = "data/input/NEON/NEON_struct-plant")
vst_apparentindividual <- read_csv("data/input/NEON/NEON_struct-plant/stackedFiles/vst_apparentindividual.csv", show_col_types = F) %>%
  # dplyr::filter(siteID == "BART") %>% # filter to reduce computing time for testing
  glimpse()

vst_non_woody <- read_csv("data/input/NEON/NEON_struct-plant/stackedFiles/vst_non-woody.csv",
                          col_types = "ccDcccciicccccccccccciddddddddddddddciiiidccccTc",
                          show_col_types = F) %>%
  # dplyr::filter(siteID == "BART") %>% # filter to reduce computing time for testing
  glimpse()

vst_shrubgroup <- read_csv("data/input/NEON/NEON_struct-plant/stackedFiles/vst_shrubgroup.csv", show_col_types = F) %>%
  # dplyr::filter(siteID == "BART") %>% # filter to reduce computing time for testing
  glimpse()

vst_perplotperyear <- read_csv("data/input/NEON/NEON_struct-plant/stackedFiles/vst_perplotperyear.csv", show_col_types = F) %>%
  dplyr::select(-uid) %>% unique() %>% # remove unique identifier to remove duplicate aberrations
  group_by(siteID, plotID, eventID) %>%
  filter(date == max(date)) %>% # remove duplicated rows with updated entries
  ungroup() %>% 
  distinct(siteID, plotID, eventID, publicationDate, .keep_all = T) %>% # remove duplicated rows with no updated date
  # dplyr::filter(siteID == "BART") %>% # filter to reduce computing time for testing
  glimpse()


## Merge tables
df <- vst_apparentindividual %>% # include woody individuals
  dplyr::bind_rows(vst_shrubgroup, # include shrub group measurements
                   vst_non_woody) %>% # include measurements on non-herbaceous perennials
  dplyr::left_join(vst_perplotperyear %>% # include plot type (tower plot vs distributed plot) and sampling event description
                     dplyr::select(siteID, plotID, eventID, eventType, plotType),
                   by = c("siteID", "plotID", "eventID")) %>%
  glimpse()


## Clean memory
rm(vst_apparentindividual, vst_non_woody, vst_shrubgroup, vst_perplotperyear)
gc()



### Check differences in data subgroups ----------------------------------------
## Plant status at measurement ----
df %>% select(plantStatus) %>% arrange(plantStatus) %>% unique() # plant status options

# Plot
df %>% 
  ggplot(aes(x = plantStatus, y = height)) +
  geom_boxplot() +
  geom_point(alpha = 0.2, color = "gray50") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270))



### Filter data ----------------------------------------------------------------
df_filt <- df %>% 
  dplyr::filter(plotType == "tower") %>% # keep measurement within tower airshed
  # dplyr::filter(plantStatus %in% c("Live", "Live,  other damage")) %>% # keep only measurement of live plants?
  glimpse()

sites_in <- unique(df$siteID); sites_out <- df_filt$siteID
miss_sites <- setdiff(sites_in, sites_out) # check excluded sites

df_filt_distributed <- df %>% 
  dplyr::filter(siteID %in% miss_sites) %>% 
  # dplyr::filter(plotType == "distributed") %>% # keep measurement outside tower airshed
  glimpse()

df_filt <- bind_rows(df_filt, df_filt_distributed) %>% arrange(siteID) # add measurements from distributed plots when no tower plot measurement is available for the site



### NEON structure data availability -------------------------------------------
neon_avail <- df_filt %>% 
  group_by(siteID) %>%
  summarise(
    Structure_start = min(date, na.rm = T),
    Structure_end = max(date, na.rm = T)
  ) %>% 
  ungroup() %>% 
  dplyr::rename(SITE_ID = siteID)

## Save
if (savedata) {
  write_csv(neon_avail, "data/input/NEON/NEON_struct-plant/structure_data_availability.csv")
}



### Aggregate and extract variables --------------------------------------------
## 1) Aggregate to unique measurement events for each individual ----
tic("Event aggregation")
df_event <- df_filt %>% 
  select(-subplotID, -nestedSubplotID) %>% 
  group_by(siteID, plotID, individualID, eventID) %>% # eventID for measurements events, which can span several dates
  dplyr::select(where(is.numeric)) %>%
  summarise(across(.cols = c(where(is.double)), .fns = ~ mean(.x, na.rm = T)), # average
            across(.cols = c(where(is.double)), .fns = ~ if_else(.x, condition = is.nan(.x), true = NA_real_, false = .x)), # convert NaN to NA
            # take mean height of measurements in case of multiple measurements for each individual
            .groups = "drop") %>%
  glimpse()
toc()

## 2) Aggregate to individual values ----
tic("Individual aggregation")
df_individual <- df_event %>% 
  dplyr::select(-eventID) %>% 
  group_by(siteID, plotID, individualID) %>%
  summarise(across(.cols = where(is.double), .fns = ~ mean(.x, na.rm = T)), # average
            across(.cols = where(is.double), .fns = ~ if_else(.x, condition = is.nan(.x), true = NA_real_, false = .x)), # convert NaN to NA
            # take mean height over whole measurement period for each individual, to average over time and look at mean behavior
            .groups = "drop") %>% 
  glimpse()
toc()

## 3) Aggregate to plot values ----
tic("Plot aggregation")
df_towerplot <- df_individual %>% 
  dplyr::select(-individualID) %>% 
  group_by(siteID, plotID) %>%
  mutate(
    height_max = quantile(height, probs = 0.9, na.rm = T), # take maximum height of each plot
    height_max = if_else(condition = is.infinite(height_max), true = NA_real_, false = height_max), # convert -Inf to NA
    meanHeight_max = quantile(meanHeight, probs = 0.9, na.rm = T), # take maximum height of each plot (shrubs)
    meanHeight_max = if_else(condition = is.infinite(meanHeight_max), true = NA_real_, false = meanHeight_max) # convert -Inf to NA
    ) %>%
  summarise(
    across(.cols = c(where(is.double), -height_max, -meanHeight_max), .fns = ~ mean(.x, na.rm = T)), # average
    across(.cols = c(where(is.double), -height_max, -meanHeight_max), .fns = ~ if_else(.x, condition = is.nan(.x), true = NA_real_, false = .x)), # convert NaN to NA
    height_max = unique(height_max),
    meanHeight_max = unique(meanHeight_max),
    .groups = "drop"
    ) %>% 
  glimpse()
toc()

## 4) Aggregate to site values ----
tic("Site aggregation")
df_site <- df_towerplot %>% 
  dplyr::select(-plotID) %>% 
  group_by(siteID) %>%
  mutate(
    height_max = quantile(height_max, probs = 0.9, na.rm = T), # take maximum height of each plot
    height_max = if_else(condition = is.infinite(height_max), true = NA_real_, false = height_max), # convert -Inf to NA
    meanHeight_max = quantile(meanHeight_max, probs = 0.9, na.rm = T), # take maximum height of each plot (shrubs)
    meanHeight_max = if_else(condition = is.infinite(meanHeight_max), true = NA_real_, false = meanHeight_max) # convert -Inf to NA
  ) %>%
  summarise(
    across(.cols = c(where(is.double), -height_max, -meanHeight_max), .fns = ~ mean(.x, na.rm = T)), # average
    across(.cols = c(where(is.double), -height_max, -meanHeight_max), .fns = ~ if_else(.x, condition = is.nan(.x), true = NA_real_, false = .x)), # convert NaN to NA
    height_max = unique(height_max),
    meanHeight_max = unique(meanHeight_max),
    .groups = "drop"
    ) %>%
  glimpse()
toc()


## Exclude variables with NAs ----
keep_vars <- df_site %>%
  summarise(across(.cols = where(is.double), .fns = ~sum(is.na(.x)))) %>% 
  pivot_longer(everything(), names_to = "varname", values_to = "datavalue") %>% 
  arrange(datavalue) %>% 
  dplyr::filter(datavalue < 3)

df_out <- df_site %>% 
  select(siteID, all_of(keep_vars$varname)) %>%
  dplyr::rename(SITE_ID = siteID) %>% 
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  # NEON structure
  write_csv(df_out, glue::glue("data/input/NEON/NEON_struct-plant/NEONstructure_extracted_{vers_out}.csv"))
}