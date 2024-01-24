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
library(RColorBrewer) # plot color functionalities
library(readr)        # read table format files
library(rlang)        # quotes
library(tidyr)        # clean and reshape tidy data

## Other
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("results/site_list_{vers_in}.csv"), show_col_types = F) %>% 
  left_join(read_csv("data/input/NEON/neon_igbp.csv"), by = "SITE_ID") %>% 
  left_join(read_csv("data/input/NEON/site_coords.csv"), by = "SITE_ID") %>% 
  glimpse()



### Plot -----------------------------------------------------------------------
colorvar <- NULL #rlang::quo(Rao_Q_NIRv) # define variable to plot as color (leave NULL for no color)

## Sites x IGBP
p_map <- dat %>% 
  drop_na(!!colorvar) %>% 
  ggplot2::ggplot(aes(longitude, latitude, color = !!colorvar)) +
  ggplot2::borders("world", fill = "#5A7F95", alpha = 0.5, xlim = c(-160, -80), ylim = c(20, 70)) +  # world borders
  ggplot2::geom_point(size = 5, alpha = 0.7) + # points
  # ggplot2::scale_color_manual(values = CatCol_igbp) + # color palette
  ggrepel::geom_label_repel(aes(label = paste0(SITE_ID, " (", IGBP, ")")), max.overlaps = 100) + 
  # ylab("") + xlab("") + # title
  # guides(color = guide_legend(title = "Rao's Q (NDVImax)")) + # legend titles
  theme_classic() +
  theme_print +
  # theme_transparent +
  theme(axis.text = element_blank(), # remove graph lines
        panel.border = element_blank()) +
  theme(
    axis.title = element_blank(),
    legend.key.height = unit(15, "mm"),
    legend.key.width = unit(20, "mm")
  ) +
  NULL
p_map



### Save -----------------------------------------------------------------------
if (savedata) {
  if (!is.null(colorvar)) {colorvar <- paste0("_", ensym(colorvar))} else {colorvar = ""}
  # jpg
  ggplot2::ggsave(filename = glue::glue("results/SITES/site_map{colorvar}_{vers_out}.jpg"),
                  plot = p_map, device = "jpeg",
                  width = 508, height = 285.75, units = "mm", dpi = 300)
  # # transparent png
  # ggplot2::ggsave(filename = glue::glue("results/SITES/site_map_{ensym(colorvar)}_{vers_out}.png"), plot = p_map, device = "png",
  #                 bg = "transparent", width = 508, height = 285.75, units = "mm", dpi = 300)
}