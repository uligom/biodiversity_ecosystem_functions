#### PLOT SIMULATION RESULTS FROM JAVIER PACHECO-LABRADOR

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
# Data settings
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
# library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
# library(ggrepel)      # repelled labels
library(patchwork)    # combine and arrange plots
library(readr)        # tidy read/save
# library(scales)       # package for rescaling
library(stringr)      # string manipulation
library(tidyr)        # reorganize tibbles

## Functions
# source("scripts/functions/min_max_norm.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
dat_svi <- read_delim(glue::glue("data/input/javiers_simulations/Simulations_fLAI_run00_com_SVIs.csv"), delim = ";", show_col_types = F) %>%
  glimpse()

dat_raoq <- read_delim(glue::glue("data/input/javiers_simulations/Simulations_fLAI_run00_com_RaoQ.csv"), delim = ";", show_col_types = F) %>%
  glimpse()



### Process data ---------------------------------------------------------------
dat_svi <- dat_svi %>% 
  pivot_longer(cols = contains("comm_"), names_to = "Community", values_to = "Values") %>% 
  mutate(Community = str_replace(Community, pattern = "_%d...", replacement = "")) %>% 
  pivot_wider(names_from = Index, values_from = "Values") %>% 
  glimpse()

dat_raoq <- dat_raoq %>% 
  pivot_longer(cols = contains("comm_"), names_to = "Community", values_to = "Values") %>% 
  mutate(Community = str_replace(Community, pattern = "_%d...", replacement = "")) %>% 
  pivot_wider(names_from = Index, values_from = "Values") %>% 
  glimpse()


## Merge datasets ----
dat <- dat_svi %>% 
  left_join(dat_raoq, by = c("LAI", "Community")) %>% 
  glimpse()



### Plot -----------------------------------------------------------------------
theme_custom <- theme_bw() +
  theme_combine +
  theme(
    legend.position = "none"
  )

mypalette <- colorRampPalette(Five_colorblind)(length(unique(dat$Community))) # generate color palette
set.seed(002); mypalette <- sample(mypalette)

p_ndvi <- dat %>% 
  ggplot(aes(x = LAI, y = NDVI, color = Community)) +
  geom_point(alpha = alpha_opaque, size = point_size_small) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium) +
  # ylim(c(0, 1)) +
  ylab("NDVI [-]") +
  scale_color_manual(values = mypalette) +
  # scale_color_viridis_d(option = "D") +
  theme_custom +
  theme(axis.title.x = element_blank()) +
  NULL

p_nirv <- dat %>% 
  ggplot(aes(x = LAI, y = NIRv, color = Community), alpha = 0.8) +
  geom_point(alpha = alpha_opaque, size = point_size_small) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium) +
  # ylim(c(0, 1)) +
  ylab("NIRv [-]") +
  scale_color_manual(values = mypalette) +
  # scale_color_viridis_d(option = "D") +
  theme_custom +
  theme(axis.title.x = element_blank()) +
  NULL


p_raoq_ndvi <- dat %>% 
  ggplot(aes(x = LAI, y = RaoQ_NDVI, color = Community)) +
  geom_point(alpha = alpha_opaque, size = point_size_small) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium) +
  # ylim(c(0, 0.16)) +
  xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  ylab(expression(RaoQ[NDVI])) +
  scale_color_manual(values = mypalette) +
  # scale_color_viridis_d(option = "D") +
  theme_custom +
  NULL

p_raoq_nirv <- dat %>% 
  ggplot(aes(x = LAI, y = RaoQ_NIRv, color = Community)) +
  geom_point(alpha = alpha_opaque, size = point_size_small) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium) +
  # ylim(c(0, 0.16)) +
  xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  ylab(expression(RaoQ[NIRv])) +
  scale_color_manual(values = mypalette) +
  # scale_color_viridis_d(option = "D") +
  theme_custom +
  NULL



### Combine plots --------------------------------------------------------------
p0 <- ((p_ndvi | p_nirv) / (p_raoq_ndvi | p_raoq_nirv)) + plot_annotation(tag_levels = "a")
p0



### Save -----------------------------------------------------------------------
if (savedata) {
  ## Final plot
  ggsave(filename = glue::glue("results/scatterplots/figure3_RaoQ_simulations.jpg"),
         plot = p0, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 16:9 ratio
  
}