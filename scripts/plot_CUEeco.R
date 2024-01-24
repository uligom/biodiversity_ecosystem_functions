#### PLOT CUE relationships

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Script settings ------------------------------------------------------------
# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- "v11.03"



### Utilities ------------------------------------------------------------------
## Packages
library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
library(ggrepel)      # repelled labels
library(patchwork)    # combine and arrange plots
library(readr)        # tidy read/save
library(scales)       # package for rescaling
library(tidyr)        # reorganize tibbles

## Functions
source("scripts/functions/plot_best.R")

## Other
# source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
dat <- read_csv("data/inter/data_efps_clim_struct_biodiv_v02.csv", show_col_types = F) %>% 
  left_join(read_csv("data/input/NEON/neon_sites.csv", show_col_types = F), by = "SITE_ID") %>% 
  dplyr::relocate(VEG_CLASS, latitude, longitude, .after = IGBP) %>% 
  glimpse()



### Plot -----------------------------------------------------------------------
## Plot specs ----
point_border_color <- "white"
point_fill_color <- "#EE4500"
point_shape <- 21
small_point_size <- 3
main_point_size <- 6
text_title <- 24
text_normal <- 20


## Diversity-CUE scatter ----
r2 <- round(summary(lm(CUE_eco_90 ~ precip, dat, na.action = na.omit))$r.squared * 100, digits = 1)
dat %>% plot_best(x = bray_curtis, y = CUE_eco_90, r2 = r2)

## Incl. aridity gradient
p_cue <- dat %>% 
  ggplot(aes(x = precip, y = CUE_eco_90)) +
  geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T) +
  geom_point(aes(fill = SWC), color = point_border_color, shape = point_shape, size = main_point_size, na.rm = T) +
  scale_fill_viridis_c() +
  labs(caption = glue::glue("R^2 = {r2}% (lm)")) +
  theme_bw() +
  theme(
    axis.text = element_text(size = text_normal),
    axis.ticks = element_blank(),
    axis.title = element_text(size = text_title),
    text = element_text(size = text_normal),
    title = element_text(size = text_title)
  ) +
  NULL
p_cue



### Save -----------------------------------------------------------------------
if (savedata) {
  ggsave(filename = glue::glue("results/scatterplots/GPPsat-satbiodiv_{save_string}_{vers_out}.jpg"),
         plot = p_cue, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300)
}