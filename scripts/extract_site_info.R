#### EXTRACT SITE BASIC INFORMATION FROM METADATA
# Site ID (SITE_ID), vegetation class (VEG_CLASS), coordinates (latitude, longitude)

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
# Data settings
savedata = T # T when output needs to be saved



### Utilities ------------------------------------------------------------------
library(dplyr)
options(dplyr.summarise.inform = F) # suppress summary info
library(readr)
library(stringr)



### Data -----------------------------------------------------------------------
# Load metadata
meta_neon <- read_csv("data/input/NEON/NEON_Field_Site_Metadata_20220412.csv", show_col_types = F)
meta_amf <- read_tsv("data/input/NEON/AmeriFlux_NEON_sites_characteristics.tsv", show_col_types = F)




### Extract metadata information -----------------------------------------------
## NEON
meta_neon <- meta_neon %>%
  select(field_site_id, field_dominant_nlcd_classes, field_latitude, field_longitude) %>%
  unique() %>%
  dplyr::rename(SITE_ID = field_site_id,
                VEG_CLASS = field_dominant_nlcd_classes,
                latitude = field_latitude, longitude = field_longitude) %>% 
  arrange(SITE_ID, VEG_CLASS)

## NEON site information (from Ameriflux)
igbp_neon <- meta_amf %>% 
  rename(IGBP = `Vegetation Abbreviation (IGBP)`) %>% 
  mutate(SITE_ID = str_extract(Name, "(?<=[:punct:])[:upper:]{4}")) %>% 
  select(SITE_ID, IGBP) %>%
  dplyr::mutate( # manually extrapolate IGBP for missing information
    IGBP = if_else(SITE_ID == "GUAN", true = "EBF", false = IGBP), # https://www.neonscience.org/field-sites/guan
    IGBP = if_else(SITE_ID == "LAJA", true = "CRO", false = IGBP) # https://www.neonscience.org/field-sites/laja
  ) %>% 
  drop_na(SITE_ID) %>% 
  arrange(SITE_ID) %>% 
  glimpse()


## NEON/Ameriflux site code translations
codes_amf_neon <- meta_amf %>% 
  rename(SITEID_amf = `Site ID`) %>% 
  mutate(SITEID_neon = str_extract(Name, "(?<=[:punct:])[:upper:]{4}")) %>% 
  select(SITEID_amf, SITEID_neon) %>%
  drop_na(SITEID_neon) %>% 
  glimpse()



## ICOS
file_list <- list.files("data/input/ICOS/unzipped_all/", pattern = "SITEINFO")
site_list <- str_extract(file_list, "[:upper:]{2}-[:alnum:]{3}")

meta_icos <- tibble()
for (i in 1:length(file_list)) {
  meta_icos <- bind_rows(meta_icos,
                         read_csv(glue::glue("data/input/ICOS/unzipped_all/{file_list[i]}"), show_col_types = F)
  )
}
# Tidy
meta_icos <- meta_icos %>% 
  pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) %>% 
  glimpse()



### Extract vegetation type ----------------------------------------------------
igbp_icos <- meta_icos %>%
  drop_na(IGBP) %>% 
  select(SITE_ID, IGBP) %>% 
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  # NEON site information (vegetation class and coordinates)
  write_csv(meta_neon, "data/input/NEON/neon_sites.csv")
  write_csv(igbp_neon, "data/input/NEON/neon_igbp.csv")
  write_csv(codes_amf_neon, "data/input/NEON/neon-amf_code_translations.csv")
  
  # ICOS site information
  write_csv(meta_icos, "data/input/ICOS/icos_metadata.csv")
  write_csv(igbp_icos, "data/input/ICOS/icos_igbp.csv")
}