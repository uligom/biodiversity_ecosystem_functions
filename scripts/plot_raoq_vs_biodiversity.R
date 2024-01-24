#### PLOT RaoQ vs SITE BIODIVERSITY

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Measure script run time
library(tictoc)
tic("Script run time")



### Options --------------------------------------------------------------------
# Data settings
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- "v06"


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
# source("scripts/functions/min_max_norm.R")
source("scripts/functions/plot_scatterplot_models.R")
source("scripts/functions/plot_best.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("data/inter/data_efps_clim_struct_biodiv_{vers_out}.csv"), show_col_types = F) %>% 
  glimpse()



# ### Facet plot -----------------------------------------------------------------
# dat_plot <- dat %>%
#   pivot_longer(cols = c(contains("Rao_Q"), sat_phen_std), names_to = "sat_biodiv_names", values_to = "sat_biodiv_values") %>%
#   pivot_longer(cols = c(n_spp, shannon, pielou_evenness, simpson_diversity, bray_curtis), names_to = "ground_biodiv_names", values_to = "ground_biodiv_values") %>%
#   mutate(sat_biodiv_names = factor(sat_biodiv_names, levels = c("Rao_Q_S2", "Rao_Q_NDVI", "Rao_Q_NIRv", "sat_phen_std")),
#          ground_biodiv_names = factor(ground_biodiv_names, levels = c("n_spp", "shannon", "pielou_evenness", "simpson_diversity", "bray_curtis"))
#   ) %>%
#   glimpse()
# 
# p_fac <- dat_plot %>%
#   ggplot(aes(ground_biodiv_values, sat_biodiv_values)) +
#   geom_point(na.rm = T) +
#   geom_smooth(method = 'glm', formula = 'y ~ x', na.rm = T) +
#   facet_wrap(sat_biodiv_names ~ ground_biodiv_names, scales = "free") +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.title = element_text(size = 20),
#     # panel.border = element_rect(fill = NA),
#     # panel.spacing = unit(0.5, "cm"),
#     plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
#     # plot.caption = element_text(size = 28),
#     # plot.title = element_text(size = 40, face = "bold"),
#     strip.text = element_text(size = 18), # facet title strips
#     strip.background = element_blank()
#   ) +
#   NULL
# 
# rm(dat_plot) # clean memory
# 

## Best relationships ----
r2 <- round(summary(lm(Rao_Q_S2 ~ n_spp, dat %>% drop_na(Rao_Q_S2, n_spp)))$r.squared * 100, digits = 1)
dat %>% plot_best(x = n_spp, y = Rao_Q_S2, r2 = r2)

r2 <- round(summary(lm(Rao_Q_S2 ~ bray_curtis, dat %>% drop_na(Rao_Q_S2, bray_curtis)))$r.squared * 100, digits = 1)
dat %>% plot_best(x = bray_curtis, y = Rao_Q_S2, r2 = r2)

r2 <- round(summary(lm(Rao_Q_NIRv ~ n_spp, dat %>% drop_na(Rao_Q_NIRv, n_spp)))$r.squared * 100, digits = 1)
dat %>% plot_best(x = n_spp, y = Rao_Q_NIRv, r2 = r2)

r2 <- round(summary(lm(Rao_Q_NIRv ~ bray_curtis, dat %>% drop_na(Rao_Q_NIRv, bray_curtis)))$r.squared * 100, digits = 1)
dat %>% plot_best(x = bray_curtis, y = Rao_Q_NIRv, r2 = r2)



# ### Phenodiv vs Bray-Curtis ----------------------------------------------------
# r2 <- round(summary(lm(sat_phen_std ~ bray_curtis, dat))$r.squared * 100, digits = 1)
# dat %>% drop_na(bray_curtis, sat_phen_std) %>% nrow()
# cor.test(x = dat$bray_curtis, y = dat$sat_phen_std, use = "complete.obs", method = "spearman")
# dat %>% plot_best(x = bray_curtis, y = sat_phen_std, r2 = r2)
# 
# 
#
### Save -----------------------------------------------------------------------
if (savedata) {
  ## All biodiversity scatterplots
  # ggsave(filename = glue::glue("results/scatterplots/all_biodiv_{vers_out}.jpg"),
  #        plot = p_fac, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
}



### End ------------------------------------------------------------------------