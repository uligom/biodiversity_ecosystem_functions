#### PLOT NEON SITES X IGBP

### Authors: Ulisse Gomarasca
### Version History
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
vers_in <- "v11.06.06"
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- vers_in



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plots
library(ggrepel)      # non-overlapping text labels
library(lubridate)    # deal with dates
library(patchwork)    # combine and arrange plots
library(RColorBrewer) # plot color functionalities
library(readr)        # read table format files
library(rlang)        # quotes
library(tidyr)        # clean and reshape tidy data

## Other
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
## Data availability
load("C:/Users/ugomar/Desktop/B_EF/data/input/NEON/fluxes/NCAR-NEON_fluxes_expanded.RData") # Fluxes
dat_s2 <- read_csv("data/input/Sentinel2/RaoQ/S2_L2A_bands_allsites.csv", show_col_types = F) # Rao Q & vegetation indeces
dat_spp <- read_csv("data/input/NEON/NEON_presence-cover-plant/species_data_availability.csv", show_col_types = F) # Species data at the sites
dat_str <- read_csv("data/input/NEON/NEON_struct-plant/structure_data_availability.csv", show_col_types = F) # Species data at the sites

## Metadata
sites <- read_csv(glue::glue("results/site_list_{vers_in}.csv"), show_col_types = F) # Site list
igbp <- read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F) # IGBP class
coords <- read_csv("data/input/NEON/site_coords.csv", show_col_types = F) # site coordinates



### Process data ---------------------------------------------------------------
flux_period <- dat %>%
  group_by(SITE_ID) %>%
  summarise(
    Flux_start = min(DATETIME, na.rm = T) %>% as_date(),
    Flux_end = max(DATETIME, na.rm = T) %>% as_date()
  ) %>% 
  ungroup()

s2_period <- dat_s2 %>% 
  drop_na(RaoQ_NIRv_S2_median) %>% 
  group_by(SITE_ID) %>%
  summarise(
    S2_start = min(DATE, na.rm = T),
    S2_end = max(DATE, na.rm = T)
  ) %>% 
  ungroup()



### Merge data -----------------------------------------------------------------
dat_all <- sites %>% 
  left_join(igbp, by = "SITE_ID") %>%
  left_join(coords, by = "SITE_ID") %>%
  left_join(flux_period, by = "SITE_ID")  %>%
  left_join(s2_period, by = "SITE_ID")  %>%
  left_join(dat_spp, by = "SITE_ID")  %>%
  left_join(dat_str, by = "SITE_ID") %>% 
  glimpse()


## Clean memory ----
rm(sites, igbp, coords, dat, flux_period, dat_s2, s2_period, dat_spp, dat_str)
gc()



### Plot Map of sites ----------------------------------------------------------
colorvar <- NULL #rlang::quo(Rao_Q_NIRv) # define variable to plot as color (leave NULL for no color)

## Sites x IGBP
p_map <- dat_all %>% 
  # drop_na(!!colorvar) %>% 
  ggplot2::ggplot(aes(longitude, latitude)) + #, color = !!colorvar
  ggplot2::borders("world", fill = "#B3B3B3", alpha = 0.5, xlim = c(-160, -65), ylim = c(16, 55)) + # world borders # fill = "#5A7F95"
  ggplot2::geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = point_size_medium) + # points
  # ggplot2::scale_color_manual(values = CatCol_igbp) + # color palette
  # ggrepel::geom_label_repel(aes(label = paste0(SITE_ID, " (", IGBP, ")")), max.overlaps = 100) +
  ggrepel::geom_text_repel(aes(label = SITE_ID), force = 2, max.overlaps = 100) +
  # guides(color = guide_legend(title = "Rao's Q (NDVImax)")) + # legend titles
  scale_fill_manual(values = CatCol_igbp, guide = guide_legend(title = "IGBP class:", title.position = "top")) +
  theme_classic() +
  theme_combine +
  # theme_transparent +
  theme(axis.text = element_blank(), # remove graph lines
        panel.border = element_blank()) +
  theme(
    axis.title = element_blank(),
    # legend.key.height = unit(15, "mm"),
    # legend.key.width = unit(20, "mm"),
    # legend.direction = "horizontal",
    # legend.position = "bottom"
  ) +
  NULL
p_map



### Plot data availability period ----------------------------------------------
## Data for plotting ----
dat_plot <- dat_all %>% 
  pivot_longer(cols = contains("Start"), names_to = "Dataset", values_to = "start_date") %>% 
  mutate(Dataset = stringr::str_extract(Dataset, "[:alnum:]+(?=_start)")) %>% 
  select(-contains("end")) %>% 
  left_join(
    dat_all %>% 
      pivot_longer(cols = contains("End"), names_to = "Dataset", values_to = "end_date") %>% 
      mutate(Dataset = stringr::str_extract(Dataset, "[:alnum:]+(?=_end)")) %>% 
      select(-contains("start")),
    by = c("SITE_ID", "IGBP", "latitude", "longitude", "Dataset")
  ) %>% 
  mutate(Dataset = case_when( # rename
    Dataset == "Flux" ~ "NEON Flux/Meteo data",
    Dataset == "S2" ~ "Sentinel 2 scenes",
    Dataset == "Spp" ~ "NEON Species data",
    Dataset == "Structure" ~ "NEON Structure data"
    )
  ) %>% 
  arrange(SITE_ID)

## Plot ----
line_thickness <- 2.6

p_avail <- ggplot() +
  geom_linerange(data = dplyr::filter(dat_plot, Dataset == "NEON Flux/Meteo data"),
                 aes(xmin = start_date, xmax = end_date, y = SITE_ID, group = Dataset, color = Dataset),
                 linewidth = line_thickness, na.rm = T) +
  geom_linerange(data = dplyr::filter(dat_plot, Dataset == "Sentinel 2 scenes"),
                 aes(xmin = start_date, xmax = end_date, y = SITE_ID, group = Dataset, color = Dataset),
                 position = position_nudge(y = 0.2),
                 linewidth = line_thickness, na.rm = T) +
  geom_linerange(data = dplyr::filter(dat_plot, Dataset == "NEON Species data"),
                 aes(xmin = start_date, xmax = end_date, y = SITE_ID, group = Dataset, color = Dataset),
                 position = position_nudge(y = 0.4),
                 linewidth = line_thickness, na.rm = T) +
  geom_linerange(data = dplyr::filter(dat_plot, Dataset == "NEON Structure data"),
                 aes(xmin = start_date, xmax = end_date, y = SITE_ID, group = Dataset, color = Dataset),
                 position = position_nudge(y = 0.6),
                 linewidth = line_thickness, na.rm = T) +
  scale_color_manual(values = c(
    `NEON Flux/Meteo data` = "#2DB1B2", `Sentinel 2 scenes` = "#B22E2D", `NEON Species data` = "#dfa8a7", `NEON Structure data` = "#8FAD02"),
    guide = guide_legend(title = "Dataset:", nrow = 2)#title.position = "top")
  ) +
  scale_y_discrete(expand = expansion(add = c(0.5, 1))) + # add space on top and bottom of grid
  theme_bw() +
  theme_combine +
  theme(
    axis.title = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom"
    )
p_avail



# ### Combine plots --------------------------------------------------------------
# figureS1 <- (
#   (
#     ((p_avail & theme(legend.direction = "horizontal")) / guide_area()) +
#       plot_layout(heights = c(0.95, 0.05), guides = "collect")
#   ) |
#     (p_map / guide_area() / plot_spacer()) +
#       plot_layout(heights = c(0.4, 0.4, 0.2), guides = "collect")
#   ) +
#   plot_layout(widths = c(0.6, 0.4)) +
#   plot_annotation(tag_levels = "a")
# figureS1
# 
# 

### Save -----------------------------------------------------------------------
if (savedata) {
  if (!is.null(colorvar)) {colorvar <- paste0("_", ensym(colorvar))} else {colorvar = ""}
  ## Figure S1: map (jpg)
  ggplot2::ggsave(filename = glue::glue("results/SITES/figureS1_site_map{colorvar}_{vers_out}.jpg"),
                  plot = p_map, device = "jpeg",
                  width = 508, height = 285.75, units = "mm", dpi = 150)
  # # transparent png
  # ggplot2::ggsave(filename = glue::glue("results/SITES/site_map_{ensym(colorvar)}_{vers_out}.png"), plot = p_map, device = "png",
  #                 bg = "transparent", width = 508, height = 285.75, units = "mm", dpi = 300)
  
  ## Figure S2: data availability
  ggsave(filename = glue::glue("results/SITES/figureS2_data_availability_{vers_out}.jpg"),
         plot = p_avail, device = "jpeg",
         width = 360, height = 480, units = "mm", dpi = 150) # 3:4 ratio
  
  # ## Figure S1
  # ggsave(filename = glue::glue("results/SITES/figureS1_data_availability_{vers_out}.jpg"),
  #        plot = figureS1, device = "jpeg",
  #        width = 600, height = 450, units = "mm", dpi = 300) # 4:3 ratio
}