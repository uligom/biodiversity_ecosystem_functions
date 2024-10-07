### RESPONSE TO REVIEWER 2:
# Are deciduous broadleaf forests or evergreen needleleaf forests more productive / higher LAI / lower albedo?


### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
# Data settings
savedata <- F #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



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
# source("scripts/functions/plot_timeseries.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
## Vegetation Indices as proxies of productivity ----
bands_raoq_files <- list.files("//minerva/BGI/work_2/ugomar/Sentinel2_2A_RaoQ/S2_RaoQ_L2A/", pattern = ("S2_2A_bands_")) %>% # available files from Javier
  stringr::str_subset(pattern = "[:upper:]{4}") # only NEON sites

## Sites
sites <- stringr::str_extract(bands_raoq_files, "(?<=S2_2A_bands_)[:graph:]+(?=.csv)") %>% na.omit() # extract site names

## Extract RaoQ for single bands for all sites
dat_ndvi <- tibble() # initialize
for (ii in 1:length(sites)) {
  ## RaoQ/VIs from bands for all scenes
  dat_site <- read_delim(glue::glue("//minerva/BGI/work_2/ugomar/Sentinel2_2A_RaoQ/S2_RaoQ_L2A/{bands_raoq_files[ii]}"), delim = ";", show_col_types = F) %>% 
    dplyr::select(-1) %>%
    tidyr::drop_na(Year)
  
  if (nrow(dat_site) == 0) { # if input data is empty
    warning(glue::glue("No RaoQ metric could be extracted for site '{sites[ii]}': no input data."))
    next
  }
  
  dat_site <- dat_site %>% 
    rename(YEAR = Year, DOY = DoY) %>% 
    mutate(YEAR = as.integer(YEAR),
           DATE = as.Date(DOY - 1, origin = paste0(YEAR, "-01-01"))
    ) %>% # subtract 1 from DOY because R uses a 0 base index, otherwise DOY = 1 will be Jan. 2.
    relocate(DATE, .before = YEAR)
  
  ## Merge data
  dat_ndvi <- bind_rows(
    dat_ndvi,
    bind_cols(
      SITE_ID = sites[ii],
      dat_site
    )
  )
} # end ii for loop
rm(dat_site); gc() # clean memory

## Extract NDVI max and sum
dat_ndvi <- dat_ndvi %>%
  group_by(SITE_ID, YEAR) %>%
  summarise(
    NDVImax = quantile(NDVI_S2_median, 0.95, na.rm = T),
    NDVIsum = sum(NDVI_S2_median, na.rm = T)
  ) %>% 
  ungroup()


## LAImax ----
## List files in folder
folder_path <- "data/input/NEON/LAI/RM07/" # folder path
gbov_lai_files <- list.files(folder_path, pattern = ".csv") # list of files

## Read single csv file and merge into single dataframe
dat_lai <- tibble() # initialize
for (ii in 1:length(gbov_lai_files)) {
  ## Read single csv file
  dat_ii <- read_delim(glue::glue("{folder_path}{gbov_lai_files[ii]}"), delim = ";", show_col_types = F,
                       col_types = "ccccccdcddTdcddddddddddddddddddddddddii",
                       na = "-999")

  ## Merge data
  dat_lai <- bind_rows(dat_lai, dat_ii)
} # end ii loop
rm(dat_ii); gc() # clean memory

## Extract SITE_ID
dat_lai <- dat_lai %>% 
  mutate(SITE_ID = str_extract(PLOT_ID, "[:upper:]{4}")) %>% 
  relocate(SITE_ID, .before = Site)

## Extract LAImax
dat_lai <- dat_lai %>%
  mutate(YEAR = lubridate::year(TIME_IS)) %>% 
  group_by(SITE_ID, YEAR) %>%
  summarise(
    LAImax = quantile(LAI_Miller_up, 0.95, na.rm = T)
  ) %>%
  ungroup()



## NEPmax and GPPsat ----
dat_efp <- read_csv("data/inter/efps_neon_v06.csv", show_col_types = F) %>% 
  select(SITE_ID, GPPsat, NEP95) %>% 
  rename(NEPmax = NEP95)



### Merge data -----------------------------------------------------------------
dat <- full_join(
  dat_ndvi,
  dat_lai,
  by = c("SITE_ID", "YEAR")
) %>% 
  full_join(dat_efp, by = "SITE_ID") %>%
  left_join(read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F), by = "SITE_ID") %>%
  relocate(IGBP, .after = SITE_ID)



### Plot -----------------------------------------------------------------------
## NDVImax vs NDVIsum distribution ----
dat_plot <- dat %>% 
  dplyr::filter(IGBP %in% c("DBF", "ENF")) %>% # select DBF and ENF
  pivot_longer(cols = !c(SITE_ID, IGBP, YEAR), names_to = "Variable", values_to = "Value")

## Test statistical difference between groups
# NDVImax
DBF_NDVImax <- dat_plot %>% dplyr::filter(IGBP == "DBF" & Variable == "NDVImax") %>% pull(Value)
if (shapiro.test(DBF_NDVImax)[["p.value"]] < 0.05) {message("DBF_NDVImax is NOT normally distributed!")} else {print("DBF_NDVImax is normally distributed.")}
ENF_NDVImax <- dat_plot %>% dplyr::filter(IGBP == "ENF" & Variable == "NDVImax") %>% pull(Value)
if (shapiro.test(ENF_NDVImax)[["p.value"]] < 0.05) {message("ENF_NDVImax is NOT normally distributed!")} else {print("ENF_NDVImax is normally distributed.")}
if (t.test(x = DBF_NDVImax, y = ENF_NDVImax)[["p.value"]] < 0.05) {print("==> means are NOT equal! DBF_NDVImax and ENF_NDVImax are significantly different.")} else {print("==> means are equal. DBF_NDVImax and ENF_NDVImax are statistically indistinguishable.")}

# NDVIsum
DBF_NDVIsum <- dat_plot %>% dplyr::filter(IGBP == "DBF" & Variable == "NDVIsum") %>% pull(Value)
if (shapiro.test(DBF_NDVIsum)[["p.value"]] < 0.05) {message("DBF_NDVIsum is NOT normally distributed!")} else {print("DBF_NDVIsum is normally distributed.")}
ENF_NDVIsum <- dat_plot %>% dplyr::filter(IGBP == "ENF" & Variable == "NDVIsum") %>% pull(Value)
if (shapiro.test(ENF_NDVIsum)[["p.value"]] < 0.05) {message("ENF_NDVIsum is NOT normally distributed!")} else {print("ENF_NDVIsum is normally distributed.")}
if (t.test(x = DBF_NDVIsum, y = ENF_NDVIsum)[["p.value"]] < 0.05) {print("==> means are NOT equal! DBF_NDVIsum and ENF_NDVIsum are significantly different.")} else {print("==> means are equal. DBF_NDVIsum and ENF_NDVIsum are statistically indistinguishable.")}

# LAImax
DBF_LAImax <- dat_plot %>% dplyr::filter(IGBP == "DBF" & Variable == "LAImax") %>% pull(Value)
if (shapiro.test(DBF_LAImax)[["p.value"]] < 0.05) {message("DBF_LAImax is NOT normally distributed!")} else {print("DBF_LAImax is normally distributed.")}
ENF_LAImax <- dat_plot %>% dplyr::filter(IGBP == "ENF" & Variable == "LAImax") %>% pull(Value)
if (shapiro.test(ENF_LAImax)[["p.value"]] < 0.05) {message("ENF_LAImax is NOT normally distributed!")} else {print("ENF_LAImax is normally distributed.")}
if (t.test(x = DBF_LAImax, y = ENF_LAImax)[["p.value"]] < 0.05) {print("==> means are NOT equal! DBF_LAImax and ENF_LAImax are significantly different.")} else {print("==> means are equal. DBF_LAImax and ENF_LAImax are statistically indistinguishable.")}

# GPPsat
DBF_GPPsat <- dat_plot %>% dplyr::filter(IGBP == "DBF" & Variable == "GPPsat") %>% pull(Value)
if (shapiro.test(DBF_GPPsat)[["p.value"]] < 0.05) {message("DBF_GPPsat is NOT normally distributed!")} else {print("DBF_GPPsat is normally distributed.")}
ENF_GPPsat <- dat_plot %>% dplyr::filter(IGBP == "ENF" & Variable == "GPPsat") %>% pull(Value)
if (shapiro.test(ENF_GPPsat)[["p.value"]] < 0.05) {message("ENF_GPPsat is NOT normally distributed!")} else {print("ENF_GPPsat is normally distributed.")}
if (t.test(x = DBF_GPPsat, y = ENF_GPPsat)[["p.value"]] < 0.05) {print("==> means are NOT equal! DBF_GPPsat and ENF_GPPsat are significantly different.")} else {print("==> means are equal. DBF_GPPsat and ENF_GPPsat are statistically indistinguishable.")}

# NEPmax
DBF_NEPmax <- dat_plot %>% dplyr::filter(IGBP == "DBF" & Variable == "NEPmax") %>% pull(Value)
if (shapiro.test(DBF_NEPmax)[["p.value"]] < 0.05) {message("DBF_NEPmax is NOT normally distributed!")} else {print("DBF_NEPmax is normally distributed.")}
ENF_NEPmax <- dat_plot %>% dplyr::filter(IGBP == "ENF" & Variable == "NEPmax") %>% pull(Value)
if (shapiro.test(ENF_NEPmax)[["p.value"]] < 0.05) {message("ENF_NEPmax is NOT normally distributed!")} else {print("ENF_NEPmax is normally distributed.")}
if (t.test(x = DBF_NEPmax, y = ENF_NEPmax)[["p.value"]] < 0.05) {print("==> means are NOT equal! DBF_NEPmax and ENF_NEPmax are significantly different.")} else {print("==> means are equal. DBF_NEPmax and ENF_NEPmax are statistically indistinguishable.")}


dat_plot <- dat_plot %>%
  mutate(
    Variable = factor(
      Variable,
      levels = unique(dat_plot$Variable),
      labels = c(
        expression("* NDVI"[max]),
        expression("NDVI"[sum]),
        expression("* LAI"[max]),
        expression("* GPP"[sat]),
        expression("* NEP"[max])
        )
    )
  )

## Plot
p_rev2 <- dat_plot %>%
  ggplot() +
  geom_boxplot(
    aes(y = Value, color = IGBP),
    linewidth = line_width_thin, na.rm = T
    ) +
  facet_wrap(. ~ Variable, labeller = label_parsed, scales = "free_y") +
  scale_color_manual(values = CatCol_igbp) +
  theme_bw() +
  theme_facets +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank()
  )
p_rev2


rm(dat_plot); gc() # clean memory


### Save -----------------------------------------------------------------------
if (savedata) {
  # NDVI timeseries
  ggplot2::ggsave(filename = glue::glue("productivity_DBF+ENF_comparison.jpg"), plot = p_rev2, device = "jpeg",
                  path = "results/SITES/", width = 508, height = 285.75, units = "mm", dpi = 150) # 1920 x  1080 px resolution (16:9)
  
}



### End ------------------------------------------------------------------------