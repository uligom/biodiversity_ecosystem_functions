#### EXPLORE RAOQ
# RaoQ estimates from Javier

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

if (savedata) {
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_raoq_extraction.txt")
  txt <- "Initializing analysis..."; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
}


### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plot
library(lubridate)  # dates
library(readr)      # read tables
library(scales)     # scale functions for visualization
library(stringr)    # string manipulation
library(terra)      # raster data
library(tidyr)

## Functions
source("scripts/functions/plot_timeseries.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
## RaoQ from single scenes: Sentinel 2 band files
bands_raoq_files <- list.files("//minerva/BGI/work_2/ugomar/Sentinel2_2A_RaoQ/S2_RaoQ_L2A/", pattern = ("S2_2A_bands_")) # available files from Javier

## Sites
sites <- stringr::str_extract(bands_raoq_files, "(?<=S2_2A_bands_)[:graph:]+(?=.csv)") %>% na.omit() # extract site names


## Extract RaoQ for single bands for all sites
dat <- tibble() # initialize
for (ii in 1:length(sites)) {
  ## RaoQ from bands for all scenes ----
  dat_site <- read_delim(glue::glue("//minerva/BGI/work_2/ugomar/Sentinel2_2A_RaoQ/S2_RaoQ_L2A/{bands_raoq_files[ii]}"), delim = ";", show_col_types = F) %>% 
    select(-1) %>%
    drop_na(Year)
  
  if (nrow(dat_site) == 0) { # if input data is empty
    warning(glue::glue("No RaoQ metric could be extracted for site '{sites[ii]}': no input data."))
    next
  }
  
  dat_site <- dat_site %>% 
    rename(YEAR = Year, DOY = DoY) %>% 
    mutate(YEAR = as.integer(YEAR),
           DATE = as.Date(DOY - 1, origin = paste0(YEAR, "-01-01"))
           ) %>% # subtract 1 from DOY because R uses a 0 base index, otherwise DOY = 1 will be Jan. 2.
    relocate(DATE, .before = YEAR) %>%
    glimpse()
  
  ## Plots for single sites ----
  ## Point specs
  point_border_color <- "white"
  point_fill_color <- "#EE4500"
  point_shape <- 21
  small_point_size <- 3
  # main_point_size <- 6
  
  p_timeseries <- dat_site %>% 
    mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2)) %>% 
    drop_na(RaoQ_S2_median) %>% 
    ggplot(aes(x = DATE, y = RaoQ_S2_median)) +
    # geom_hline(data = dat_old %>% filter(SITE_ID == sites[ii]), aes(yintercept = RaoQ_S2ndvimax_median, color = "Composite RaoQ"),
    #            linewidth = 0.75, show.legend = T) +
    guides(color = guide_legend(title = "Reference value")) +
    geom_line(color = "gray50", linewidth = 0.75, na.rm = T) +
    geom_point(aes(fill = f_valid_samples), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
    scale_fill_viridis_c(option = "viridis", direction = -1,
                         breaks = c(0.3, 0.5, 0.7, 0.9), limits = c(0.3, 0.9), oob = squish, # set range and squish out-of-bounds values to range
                         na.value = "#E2A2BE") +
    guides(fill = guide_legend(title = "Fraction of valid pixels")) +
    labs(title = glue::glue("Site {sites[ii]}")) + xlab("") +
    theme_bw() +
    # theme(legend.position = "none") +
    NULL
  
  
  ## Save plots ----
  if (savedata) {
    ggsave(filename = glue::glue("results/timeseries/RaoQ_S2/RaoQ_S2_{sites[ii]}.jpg"),
           plot = p_timeseries, device = "jpeg",
           width = 508, height = 285.75, units = "mm", dpi = 300)
  }
  
  
  ## Merge data ----
  dat <- bind_rows(
    dat,
    bind_cols(
      SITE_ID = sites[ii],
      dat_site
    )
  )
} # end ii for loop

rm(dat_site) # clean memory



### Plot -----------------------------------------------------------------------
## Data for plotting ----
dat_plot <- dat %>% mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2))


## Time series of RaoQ metrics ----
dat_plot %>% plot_timeseries(x = "DATE", y = "RaoQ_S2_median", color = "f_valid_samples", facet = "SITE_ID", savepath = "results/timeseries/")

dat_plot %>% plot_timeseries(x = "DATE", y = "RaoQ_NDVI_S2_median", color = "f_valid_samples", facet = "SITE_ID", savepath = "results/timeseries/")

dat_plot %>% plot_timeseries(x = "DATE", y = "RaoQ_NIRv_S2_median", color = "f_valid_samples", facet = "SITE_ID", savepath = "results/timeseries/")



### Plot VIs -------------------------------------------------------------------
## Plot NDVI
p_ndvi <- dat %>%
  mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2)) %>% 
  plot_timeseries("DATE", "NDVI_S2_median", color = "f_valid_samples", facet = "SITE_ID") +
  ylim(c(0.5, 1.0)) +
  scale_color_viridis_c(option = "viridis", direction = -1, alpha = 0.5,
                       breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                       na.value = na_color) +
  guides(color = guide_legend(title = "Fraction of valid pixels")) +
  NULL
p_ndvi

## Plot NIRv
p_nirv <- dat %>%
  mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2)) %>% 
  plot_timeseries("DATE", "NIRv_S2_median", color = "f_valid_samples", facet = "SITE_ID") +
  # ylim(c(0.5, 1.0)) +
  scale_color_viridis_c(option = "viridis", direction = -1, alpha = 0.5,
                        breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                        na.value = na_color) +
  guides(color = guide_legend(title = "Fraction of valid pixels", override.aes = list(size = point_size_big))) +
  NULL
p_nirv



### FILTER data ----------------------------------------------------------------
# NDVIquants <- quantile(dat$NDVI_S2_median, probs = c(seq(0.5:1, by = 0.05)), na.rm = T)
q_thresh <- 0.89
f_thresh <- 0.5

## Number of sites before filtering
n_sites <- dat %>% drop_na(RaoQ_S2_median, RaoQ_NDVI_S2_median, RaoQ_NIRv_S2_median) %>%
  select(SITE_ID) %>% dplyr::filter(str_detect(SITE_ID, "[:upper:]{4}")) %>% unique() %>% nrow()

txt <- glue::glue("{n_sites} NEON sites with valid RaoQ estimates BEFORE filtering.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

## Filtering
txt <- glue::glue("==> Filtering out scenes below {q_thresh*100}th quantile NDVI threshold, and with less than {f_thresh*100}% valid pixels.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

dat_filt <- dat %>% 
  group_by(SITE_ID) %>% 
  mutate(NDVIthresh = quantile(NDVI_S2_median, probs = q_thresh, na.rm = T), .after = NDVI_S2_median) %>% 
  ungroup() %>% 
  dplyr::filter(f_valid_samples > f_thresh) %>% # filter on % of valid pixels
  dplyr::filter(NDVI_S2_median >= NDVIthresh) #%>% # filter on NDVI threshold
  # glimpse()


## Number of sites after filtering
n_sites <- dat_filt %>% drop_na(RaoQ_S2_median, RaoQ_NDVI_S2_median, RaoQ_NIRv_S2_median) %>%
  select(SITE_ID) %>% dplyr::filter(str_detect(SITE_ID, "[:upper:]{4}")) %>% unique() %>% nrow()

txt <- glue::glue("{n_sites} NEON sites with valid RaoQ estimates AFTER filtering.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}



### CALCULATE aggregated site RaoQ and NDVImax ---------------------------------
dat_agg <- dat_filt %>% 
  dplyr::select(-DATE, -DOY, -YEAR, -NDVIthresh) %>% 
  group_by(SITE_ID) %>% 
  mutate(NDVImax = quantile(NDVI_S2_median, 0.95, na.rm = T), # calculate NDVImax
         NIRvmax = quantile(NIRv_S2_median, 0.95, na.rm = T) # calculate NIRvmax
         ) %>%
  summarise(across(.cols = c(where(is.double), -NDVImax, -NIRvmax), ~ mean(.x, na.rm = T)),
            NDVImax = unique(NDVImax),
            NIRvmax = unique(NIRvmax)
            ) %>% # average every variable
  ungroup() %>% 
  relocate(NDVImax, .after = NDVI_S2_median) %>% 
  relocate(NIRvmax, .after = NIRv_S2_median) %>% 
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  # Band data
  write_csv(dat, "data/input/Sentinel2/RaoQ/S2_L2A_bands_allsites.csv")
  
  # New aggregated RaoQ estimates
  write_csv(dat_agg, "data/input/Sentinel2/RaoQ/raoQ_S2_L2A_filtered_averaged.csv")
  
  # NDVI timeseries
  ggplot2::ggsave(filename = glue::glue("NDVI_FromBandsMedians.jpg"), plot = p_ndvi, device = "jpeg",
                  path = "results/timeseries/", width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
  # NIRv timeseries
  ggplot2::ggsave(filename = glue::glue("NIRv_FromBandsMedians.jpg"), plot = p_nirv, device = "jpeg",
                  path = "results/timeseries/", width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
  
}