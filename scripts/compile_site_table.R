#### TABLE OF SITES

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
vers_in <- "v11.06.06"; vers_out <- vers_in
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # plots
library(janitor)      # clean variable names
library(lubridate)    # deal with dates
library(readr)        # tidy read/save
library(tidyr)        # tidy data

## Other Utilities
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
## Metadata
meta <- read_csv("data/input/NEON/NEON-SiteMap-Table.csv", show_col_types = F) %>% glimpse() # Site metadata

meta2 <- read_csv("data/input/NEON/NEON_Field_Site_Metadata_20220412.csv", show_col_types = F) %>% glimpse() # Other site metadata

igbp <- read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F) %>% glimpse() # IGBP classes

sites <- read_csv(glue::glue("results/site_list_{vers_in}.csv"), show_col_types = F) %>% pull(SITE_ID);  sites # Site list



### Build Table ----------------------------------------------------------------
dat_out <- meta %>% dplyr::rename(SITE_ID = siteCode) %>% dplyr::select(SITE_ID, siteName, latitude, longitude) %>% 
  left_join(igbp, by = "SITE_ID") %>% relocate(IGBP, .after = "siteName") %>% 
  left_join(meta2 %>%
              dplyr::rename(SITE_ID = field_site_id, elevation = field_mean_elevation_m,
                     MAT = field_mean_annual_temperature_C, MAP = field_mean_annual_precipitation_mm,
                     URL = field_site_url
                     ) %>%
              dplyr::select(SITE_ID, elevation, MAT, MAP, URL),
            by = "SITE_ID") %>% 
  dplyr::filter(SITE_ID %in% sites) %>% 
  arrange(SITE_ID) %>% 
  clean_names(case = "title", abbreviations = c("ID", "IGBP", "MAT", "MAP")) %>% 
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  ## Table S1: Table of sites.
  write_csv(dat_out, "results/tableS1_table_of_sites.csv")
}