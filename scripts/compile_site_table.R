#### TABLE OF SITES

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
vers_in <- "v11.06.06"
savedata <- T



### Utilities ------------------------------------------------------------------
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(janitor)      # clean variable names
library(readr)        # tidy read/save



## Packages
### Data -----------------------------------------------------------------------
meta <- read_csv("data/input/NEON/NEON-SiteMap-Table.csv", show_col_types = F) %>% glimpse()

meta2 <- read_csv("data/input/NEON/NEON_Field_Site_Metadata_20220412.csv", show_col_types = F) %>% glimpse()

igbp <- read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F) %>% glimpse()

sites <- read_csv(glue::glue("results/site_list_{vers_in}.csv"), show_col_types = F) %>% pull(SITE_ID);  sites



### Build Table ----------------------------------------------------------------
dat <- meta %>% rename(SITE_ID = siteCode) %>% dplyr::select(SITE_ID, siteName, latitude, longitude) %>% 
  left_join(igbp, by = "SITE_ID") %>% relocate(IGBP, .after = "siteName") %>% 
  left_join(meta2 %>%
              rename(SITE_ID = field_site_id, elevation = field_mean_elevation_m,
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
  write_csv(dat, "results/tableS1_table_of_sites.csv")
}