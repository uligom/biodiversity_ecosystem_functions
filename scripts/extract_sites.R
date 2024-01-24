#### SELECT SITES

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
folder_path <- getwd()  # main directory
vers_in  <- "v12" # version identifier to load input files
vers_out <- "v01" # version identifier to save output files (cf. version history)
# savedata = T      # saving option true/false



### Utilities ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(sf)



### Data -----------------------------------------------------------------------
# ## Test sites
# df <- read_csv(glue::glue("{folder_path}/data/multivar_mean_{vers_in}.csv"), show_col_types = F)

## Site list (from calculation of biodiversity indices)
site_csv <- read_csv("data/input/site_list_neon_icos.csv")

## site coordinates
site_coords <- bind_rows(
  read_xlsx("data/input/ICOS/species_at_subplots/grasslands.xlsx", sheet = 4) %>% # ICOS grasslands
    dplyr::rename(SITE_ID = Site, longitude = LOCATION_LONG, latitude = LOCATION_LAT) %>% 
    select(SITE_ID, longitude, latitude),
  read_xlsx("data/input/ICOS/species_at_subplots/GRP_TREE - 20220804.xlsx", sheet = 3) %>% # ICOS forests
    dplyr::rename(SITE_ID = Site, longitude = LOCATION_LONG, latitude = LOCATION_LAT) %>% 
    select(SITE_ID, longitude, latitude),
  read_xlsx("data/input/ICOS/species_at_subplots/mires.xlsx", sheet = 4) %>% # ICOS mires
    dplyr::rename(SITE_ID = Site, longitude = LOCATION_LONG, latitude = LOCATION_LAT) %>% 
    select(SITE_ID, longitude, latitude),
  read_csv("data/input/NEON/NEON-SiteMap-Table.csv") %>% # NEON sites
    dplyr::rename(SITE_ID = siteCode) %>%
    select(SITE_ID, longitude, latitude)
)



### Attach coordinates ---------------------------------------------------------
site_all <- site_csv %>% 
  left_join(site_coords, by = "SITE_ID") %>% 
  unique()


# ### Select sites ---------------------------------------------------------------
# site_long <- df %>%
#   dplyr::select(SITE_ID, latitude, longitude)
#   
# 
# site_filt <- df %>% 
#   dplyr::select(SITE_ID, IGBP, GPPsat, RECOmax, EF, Gsmax, wLL, wLMA, wSSD, wNmass,
#                 Hc, LAImax, LAI_Hc, PNUE, WUEt, n_spp, latitude, longitude) %>% # variables from PCA analyses
#   drop_na() %>% # remove sites with no data for variables used
#   dplyr::filter(IGBP %in% c("DBF", "DEF", "EBF", "ENF", "MF")) #%>% # only forests (for now)
#   # dplyr::filter(n_spp > 3 & n_spp < 35) # remove extremes in number of species
# 
# site_short <- site_filt %>% 
#   dplyr::filter(SITE_ID %in% c("AU-Wom", # EBF, Australia
#                                "BE-Vie", # MF,  Central Europe
#                                "CA-Gro", # MF,  Canada
#                                "CA-Obs", # ENF, Canada
#                                "CH-Dav", # ENF, Central Europe, mountains
#                                "DE-Hai", # DBF, Central Europe
#                                "FI-Hyy", # ENF, Northern Europe
#                                "FR-Pue", # EBF, Mediterranean
#                                "IL-Yat", # ENF, Mediterranean
#                                "US-Ha1"  # DBF, USA
#                                )
#                 ) %>% 
#   dplyr::select(SITE_ID, latitude, longitude)
# 
# 
#
### Convert to shapefile -------------------------------------------------------
site_shp <- st_as_sf(site_all, coords = c("longitude", "latitude"),
                           crs = "+proj=longlat +datum=WGS84+no_defs +towgs84=0,0,0")

# site_short_shp <- st_as_sf(site_short, coords = c("longitude", "latitude"),
#                            crs = "+proj=longlat +datum=WGS84+no_defs +towgs84=0,0,0")

# world <- map_data('world')
# ggplot() +
#   geom_map(
#     data = world, map = world,
#     aes(long, lat, map_id = region),
#     color = "white", fill = "lightgray", size = 0.1
#   ) +
#   # geom_point(
#   #   data = site_short,
#   #   aes(longitude, latitude),
#   #   alpha = 0.7
#   # ) +
#   geom_polygon(
#     data = site_short_shp,
#     aes(geometry),
#     alpha = 0.7
#   ) +
#   coord_equal() +
#   NULL


### Save -----------------------------------------------------------------------
# All sites
write_csv(site_all, "data/input/site_list_all.csv")
sf::st_write(site_shp, "data/input/site_list_all.shp", append = F)

# Test sites
# write_csv(site_long, glue::glue("{folder_path}/site_list_long.csv"))
# write_csv(site_short, glue::glue("{folder_path}/site_list_short.csv"))
# sf::st_write(site_short, glue::glue("{folder_path}/site_list_short.shp"))
# sf::st_write(site_short, glue::glue("{folder_path}/site_list_short.geojson"))