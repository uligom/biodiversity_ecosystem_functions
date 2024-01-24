#### EXTRACT SOIL WATER CONTENT FOR NEON SITES

### Authors: Ulisse Gomarasca
### Version History
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
savedata <- F #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # plots
library(readr)        # read table format files
library(stringr)      # string manipulation
library(tidyr)        # clean and reshape tidy data

## Other



### Data -----------------------------------------------------------------------
dat <- tibble() # initialize
file_list <- list.files(path = "data/input/NEON/NEON_soil", pattern = "BASE-BADM")
site_list <- stringr::str_extract(file_list, "[:upper:]{2}-[:alnum:]{3}")

for (i in 1:length(file_list)) {
  file_name <- file_list[i] %>% stringr::str_replace("-BADM", "_HH") %>% stringr::str_replace("zip", "csv")
  
  dat <- bind_rows(dat,
                   read_csv(unzip(zipfile = glue::glue("data/input/NEON/NEON_soil/{file_list[i]}"),
                                  files = file_name,
                                  exdir = "/data/input/NEON/NEON_soil"),
                            skip = 2, show_col_types = F, na = "-9999"
                            ) %>% 
                     mutate(SITE_ID = site_list[i], .before = everything())
  )
}
# NB: Details: The three components of the qualifier are indices that indicate an
# observation's spatial position. In other words, the indices describe the position
# of a sensor relative to other sensors that measure the same variable within a site,
# i.e., Horizontal position (_<H>), Vertical position (_<V>), Replicate (_<R>).
# The letters H, V, and R are to be replaced with integer values to represent,
# e.g., SWC_1_1_1, SWC_1_1_2, SWC_1_2_1

## Clean memory
gc()



### Process --------------------------------------------------------------------
## Add NEON site ID translations
neon_ids <- read_csv("data/input/NEON/NEON-AMERIFLUX_siteID_translations.csv", show_col_types = F)
dat <- neon_ids %>%
  left_join(dat, by = "SITE_ID") %>% 
  glimpse()

# dat <- dat %>% 
#   pivot_longer(cols = contains("SWC"), names_to = "SWC_var", values_to = "SWC") %>% 
#   drop_na(SWC) %>% 
#   # arrange(SWC_var) %>% 
#   group_by(NEON_ID, SWC_var) %>% 
#   summarise(SWC = mean(SWC), .groups = "drop") %>% 
#   glimpse() %>%
#   ggplot(aes(x = SWC, y = SWC_var, color = NEON_ID)) +
#   geom_point() +
#   scale_y_discrete(limits=rev) +
#   theme_bw() +
#   # theme(axis.text.x = element_text(angle = 270)) +
#   NULL

## Positions to same variable (longer format)
dat <- dat %>%
  dplyr::select(SITE_ID, NEON_ID, contains("SWC")) %>%
  pivot_longer(cols = contains("SWC"), names_to = "SWC_var", values_to = "SWC") %>%
  drop_na(SWC) %>%
  glimpse()

## Extract positional information of sensors
dat <- dat %>% 
  dplyr::mutate(SWC_horizontal_pos = stringr::str_extract(SWC_var, pattern = "(?<=SWC_)[:digit:]"),
                SWC_vertical_pos = stringr::str_extract(SWC_var, pattern = "(?<=SWC_[:digit:]_)[:digit:]"),
                SWC_repl = stringr::str_extract(SWC_var, pattern = "(?<=SWC_[:digit:]_[:digit:]_)[:digit:]")
                ) %>% 
  glimpse()

## Extract mean Soil Water Content for positions
dat_pos <- dat %>%
  group_by(NEON_ID, SWC_horizontal_pos, SWC_vertical_pos, SWC_repl) %>% 
  summarise(SWC = mean(SWC, na.rm = T), .groups = "drop") %>% 
  glimpse()

phor <- dat_pos %>%
  ggplot(aes(x = SWC, y = SWC_horizontal_pos)) +
  geom_boxplot() +
  geom_point(color = "gray50") +
  facet_wrap(. ~ NEON_ID) +
  scale_y_discrete(limits = rev) +
  labs(title = "Mean soil water content at horizontal position of sensor.") +
  theme_bw() +
  NULL

pver <- dat_pos %>%
  ggplot(aes(x = SWC, y = SWC_vertical_pos)) +
  geom_boxplot() +
  geom_point(color = "gray50") +
  facet_wrap(. ~ NEON_ID) +
  scale_y_discrete(limits = rev) +
  labs(title = "Mean soil water content at vertical position of sensor.") +
  theme_bw() +
  NULL


## Extract mean Soil Water Content at sites ----
# # Mean of means
# dat_out1 <- dat_pos %>% 
#   group_by(NEON_ID) %>% 
#   summarise(SWC = mean(SWC, na.rm = T), .groups = "drop") %>% 
#   glimpse()
#
# Everything means
dat_out2 <- dat %>%
  group_by(NEON_ID) %>%
  summarise(SWC = mean(SWC, na.rm = T), .groups = "drop") %>%
  glimpse()

# dat_out <- dat_out1 %>% 
#   left_join(dat_out2, by = "NEON_ID", suffix = c("1", "2")) %>% 
#   glimpse()
#
# dat_out %>% 
#   ggplot(aes(x = SWC1, y = SWC2)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   ggrepel::geom_label_repel(aes(label = NEON_ID)) +
#   theme_bw()



### Save -----------------------------------------------------------------------
if (savedata) {
  ## Images
  ggplot2::ggsave(filename = glue::glue("results/scatterplots/soil_water_content_horizontal_distribution_NEON.jpg"),
                  plot = phor, device = "jpeg",
                  width = 508, height = 285.75, units = "mm", dpi = 300)
  ggplot2::ggsave(filename = glue::glue("results/scatterplots/soil_water_content_vertical_distribution_NEON.jpg"),
                  plot = pver, device = "jpeg",
                  width = 508, height = 285.75, units = "mm", dpi = 300)
  
  ## Data
  write_csv(dat_out2, "data/input/NEON/NEON_soil/mean_swc_sites.csv")
}