#### CALCULATE ECOSYSTEM FUNCTIONAL PROPERTIES

### Authors: Ulisse Gomarasca
### Changelog ------------------------------------------------------------------
# v03, 10.05.2023:  Added WUE metrics; corrected NA output for CUE when error:
#                   previously it could delete sites in the overall output.
#      03.08.2023:  Added WUEt calculation.
# v04, 06.10.2023:  Added option for LUE calculation.
# v05, 27.11.2023:  Corrected growing season filter.
# v06, 28.12.2023:  Moved filtering inside functions (sequential filters were outputting
#                   slightly different filtered datasets than filtering everything at once)



### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {
  vers_out <- "v06"
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_calculate_EFPs_{vers_out}.txt")
}



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plots
library(purrr)      # map functions
library(readr)      # read table format files
library(REddyProc)  # eddy covariance functions
library(rlang)      # quoting inside functions
library(tictoc)     # measure time
library(tidyr)      # clean and reshape tidy data

## Functions
source("scripts/functions/calc_CUEeco.R")
source("scripts/functions/calc_GPPsat_NEPmax.R")
source("scripts/functions/calc_Gsmax.R")
source("scripts/functions/calc_LUE.R")
source("scripts/functions/calc_WUE.R")
source("scripts/functions/calc_WUEt.R")
source("scripts/functions/filter_growing_season.R")



### Data -----------------------------------------------------------------------
load(file = "data/input/NEON/fluxes/NCAR-NEON_fluxes_expanded.RData") # load NCAR-NEON fluxes ('dat')

sites <- dat %>% pull(SITE_ID) %>% unique()



### Define 5-days moving window for GPPsat calculation -------------------------
dat <- dat %>%
  group_by(SITE_ID) %>% 
  mutate(FiveDaySeq = rep(c(1:ceiling(n()/5)), each = 48 * 5, length.out = n())) %>% 
  ungroup() %>% glimpse()



### Exclude cropland sites -----------------------------------------------------
igbp_filt <- read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F) %>% 
  dplyr::filter(IGBP == "CRO")

n0 <- dat %>% pull(SITE_ID) %>% unique() %>% length() # number of sites BEFORE filter

txt <- "==> Excluding cropland sites."; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
dat <- dat %>% 
  dplyr::filter(!(SITE_ID %in% igbp_filt$SITE_ID))

## Evaluation
n1 <- dat %>% pull(SITE_ID) %>% unique() %>% length() # number of sites AFTER filter
txt <- glue::glue("{n0 - n1} cropland site(s) excluded from the analysis.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

# "STER" site is excluded



### Filtering ------------------------------------------------------------------
## Filter thresholds ----
QCfilt <- c(0, 1) # quality filter
GSfilt <- 0.2     # growing season
Pfilt  <- 0.1     # precipitation filter
Pfilt_time <- 24  # period to exclude after precipitation events (hours)
SWfilt <- 50      # radiation filter (daytime)
USfilt <- 0.2     # u* filter
GPPsatfilt <- 60  # filter for GPPsat outliers
# Rfilt  <- 30      # filter for RECO_NT outliers

## Initialize evaluation
sites <- dat %>% janitor::remove_empty(which = "rows") %>% pull(SITE_ID) %>% unique()
n0 <- length(sites) # number of sites BEFORE filter


# ## Filter data for good quality and growing season ----
# txt <- "==> Filtering data for quality and growing season."
# print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
# 
# for (gg in 1:n0) {
#   if (gg == 1) {dat_filt0 <- tibble()} # initialize
#   dat_filt0 <- bind_rows(
#     dat_filt0,
#     dat %>% 
#       dplyr::filter(SITE_ID == sites[gg]) %>% 
#       as.data.frame() %>%
#       bigleaf::filter.data(quality.control = TRUE, filter.growseas = TRUE, filter.precip = FALSE,
#                          GPP = "GPP", doy = "DOY", year = "YEAR", tGPP = GSfilt,
#                          precip = "P", tprecip = 0.1, precip.hours = 24, records.per.hour = 2,
#                          vars.qc = c("TA","H","LE", "NEE", "RH"), quality.ext = "_QC", good.quality = c(0, 1)
#                          ) %>%
#       as_tibble()
#   )
# }
# dat_filt0 <- dat_filt0 %>%
#   drop_na(SITE_ID) %>%
#   glimpse()
# gc() # clean memory
# 
# dat_filt <- dat %>%
#   # dplyr::filter(SITE_ID == "BLAN") %>% # test on one site
#   group_by(SITE_ID, YEAR, DOY) %>%
#   mutate(GPP_daily = mean(GPP, na.rm = T), # daily GPP average for GS filter
#          GPP_daily = if_else(is.nan(GPP_daily), NA_real_, GPP_daily) # clean NA
#          ) %>% 
#   ungroup() %>%
#   dplyr::filter(
#     ## Quality flags filter
#     (TA_QC %in% QCfilt | is.na(TA_QC)) &
#       (H_QC %in% QCfilt | is.na(H_QC)) &
#       (LE_QC %in% QCfilt | is.na(LE_QC)) &
#       (NEE_QC %in% QCfilt | is.na(NEE_QC)) &
#       (RH_QC %in% QCfilt | is.na(RH_QC)) #&
#   ) %>%
#   group_by(SITE_ID) %>% 
#   nest(.key = "GSdata") %>%
#   mutate(GPPd = map(.x = GSdata, .f = filter_growing_season
#                     )
#          ) %>% 
#   ungroup() %>%
#   unnest(cols = c(GSdata, GPPd)) %>%
#   ## Growing season filter
#   dplyr::filter(GPPd) %>%
#   select(-GPP_daily, -GPPd) %>%
#   glimpse()
# # TO DO: add evaluation of excluded data
# 
# 
# ## Evaluation
# n1 <- dat_filt %>% drop_na(SITE_ID) %>% pull(SITE_ID) %>% unique() %>% length() # number of sites AFTER filter
# txt <- glue::glue("{n0 - n1} site(s) excluded from the analysis based on general filtering criteria.")
# print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
# 
# 
# 
### Calculate Ecosystem Functional Properties (EFPs) ---------------------------
# Calculate EFPs mapping on each site (and year if specified)
## Define computation grouping strategy ----
while (T) {
  group_year <- F #readline(prompt = "Calculate EFPs for site-years (T) or whole sites (F)? T/F:") # ask if sites or site-years
  if (group_year %in% c("y", "yes", "Y", "YES", "T", "TRUE")) {
    grouping_var <- rlang::sym("YEAR")
    txt <- "==> Computing EFPs for each site-year."
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    break
  } else if (group_year %in% c("n", "no", "N", "NO", "F", "FALSE")) {
    grouping_var <- NULL
    txt <- "==> Computing EFPs for each site."
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    break
  } else {
    print("Invalid input. Specify 'yes' or 'no'")
    next
  }
}; rm(group_year) # clean environment


## Calculations ----
## Initialize evaluation
n0 <- dat %>% janitor::remove_empty(which = "rows") %>% pull(SITE_ID) %>% unique() %>% length() # number of sites BEFORE filter

## Compute
tic()
dat_out <- dat %>%
  # dplyr::filter(SITE_ID == "BLAN") %>% # for testing on one site with data
  # dplyr::filter(SITE_ID == "BONA") %>% # for testing not working site
  group_by(SITE_ID, !!grouping_var) %>% # grouping variables for mapping
  nest(data4EFPs = -c(SITE_ID, !!grouping_var)) %>%
  tidyr::drop_na() %>% # remove empty sites-years
  mutate(
    ## CUEeco
    CUEeco = map(.x = data4EFPs, .f = calc_CUEeco,
                 site = SITE_ID, year = !!grouping_var, qile = c(0.1, 0.5, 0.9),
                 QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt, USfilt = USfilt
    ),
    ## GPPsat & NEP95
    LRCmetrics = map(.x = data4EFPs, .f = calc_GPPsat_NEPmax,
                     site = SITE_ID, year = !!grouping_var,
                     QCfilt = QCfilt, GSfilt = GSfilt, SWfilt = SWfilt, USfilt = USfilt, GPPsatfilt = GPPsatfilt
                     ),
    ## Gsmax
    Gsmax = map_dbl(.x = data4EFPs, .f = calc_Gsmax,
                    site = SITE_ID, year = !!grouping_var,
                    QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt * 2, USfilt = USfilt
    ),
    # ## LUE
    # LUE = map_dbl(.x = data4EFPs, .f = calc_LUE,
    #               site = SITE_ID, year = !!grouping_var, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt, USfilt = USfilt
    # ),
    ## WUE
    WUEmetrics = map(.x = data4EFPs, .f = calc_WUE,
                     site = SITE_ID, year = !!grouping_var,
                     QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt * 2, USfilt = USfilt
    ),
    ## WUEt
    WUEt = map_dbl(.x = data4EFPs, .f = calc_WUEt,
                   site = SITE_ID, year = !!grouping_var,
                   QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt * 2, USfilt = USfilt
    )
  ) %>%
  ungroup() %>%
  unnest(cols = c(WUEmetrics, LRCmetrics, CUEeco)) %>% # extract variables; to keep empty outputs: 'keep_empty = T'
  glimpse()
toc()

## Evaluation
n1 <- dat_out %>% pull(SITE_ID) %>% unique() %>% length() # number of sites AFTER filter
txt <- glue::glue("{n0 - n1} site(s) excluded from the analysis based on specific filtering criteria and further computation of EFPs.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}


## Evaluate sites with long time-series
n_longsites <- dat_out %>%
  unnest(cols = data4EFPs) %>%
  dplyr::select(SITE_ID, YEAR) %>% 
  unique() %>% 
  drop_na() %>% 
  group_by(SITE_ID) %>% 
  group_size()
n_longsites <- n_longsites[n_longsites >= 4] %>% length()
txt <- glue::glue("EFPs calculated for {n_longsites} sites with at least 4 years of data.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}


## Remove input data
dat_out <- dat_out %>%
  dplyr::select(-data4EFPs) %>%
  glimpse()



### Plot -----------------------------------------------------------------------
p_efps <- dat_out %>% 
  pivot_longer(cols = c(!SITE_ID & !contains("pval")),
               names_to = "VARIABLENAME", values_to = "DATAVALUE") %>% 
  # filter(VARIABLENAME == "GPPsat") %>% 
  ggplot() +
  geom_point(aes(SITE_ID, DATAVALUE), alpha = 0.8, color = "#808080", size = 3) +
  # geom_boxplot(aes(VARIABLENAME, DATAVALUE), color = "#808080") +
  # geom_violin(aes(x = VARIABLENAME, y = DATAVALUE), color = "#808080",
  #             draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
  facet_wrap(. ~ VARIABLENAME, scales = "free_y") +
  labs(caption = paste("n =", nrow(dat_out))) +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 270), # rotate x axis text
        panel.grid.major = element_line(),
        plot.caption = element_text(size = 24),  # caption text
        plot.margin = margin(10, 10, 10, 10, unit = "mm"), # margins around plot
        strip.text = element_text(size = 20),    # subplots title text
        strip.background = element_blank()       # subplots title with no border
  )
print(p_efps)

## Save
if (savedata) {
  ggsave(filename = glue::glue("EFPs.jpg"),
         plot = p_efps, device = "jpeg", path = "results/scatterplots",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
}



### Save -----------------------------------------------------------------------
if (savedata) {
  if (is_empty(grouping_var)) {groupin <- ""} else {groupin <- "_ByYears"}
  write_csv(x = dat_out, file = glue::glue("data/inter/efps_neon{groupin}_{vers_out}.csv"))
}