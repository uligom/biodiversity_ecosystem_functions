#### PLOT SIMULATION RESULTS FROM JAVIER PACHECO-LABRADOR

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
# Data settings
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- "v03"



### Utilities ------------------------------------------------------------------
## Packages
# library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
# library(ggrepel)      # repelled labels
library(patchwork)    # combine and arrange plots
library(purrr)        # map function family
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
# ## Single simulation (included in dat_all)
# dat_svi <- read_delim(glue::glue("data/input/javiers_simulations/Simulations_fLAI_run00_com_SVIs.csv"), delim = ";", show_col_types = F) %>%
#   glimpse()
# dat_rao <- read_delim(glue::glue("data/input/javiers_simulations/Simulations_fLAI_run00_com_RaoQ.csv"), delim = ";", show_col_types = F) %>%
#   glimpse()

## Files names
svi_list <- list.files(path = "data/input/javiers_simulations/", pattern = "_com_SVIs")
rao_list <- list.files(path = "data/input/javiers_simulations/", pattern = "_com_RaoQ")
spp_list <- list.files(path = "data/input/javiers_simulations/", pattern = "CommxSpAbundances")

list_ids <- str_extract(svi_list, pattern = "run[:digit:]{2}") # str_extract(svi_list, pattern = "(?<=run)[:digit:]{2}") %>% as.integer()

## List of simulations
dat_all_sim <- map(.f = read_delim, .x = paste0("data/input/javiers_simulations/", svi_list), delim = ";", show_col_types = F)
dat_all_rao <- map(.f = read_delim, .x = paste0("data/input/javiers_simulations/", rao_list), delim = ";", show_col_types = F)

## Species abundances tables
dat_all_spp <- map(.f = read_delim, .x = paste0("data/input/javiers_simulations/", spp_list), delim = ";", show_col_types = F)


### Process data ---------------------------------------------------------------
## Flatten lists, pivot, and merge ----
## Species x Community tables
dat_all_biodiv <- dat_all_spp %>% 
  bind_rows(.id = "Simulation") %>% 
  rename(Community = Communities) %>% 
  pivot_longer(cols = contains("Species_"), names_to = "Species", values_to = "Abundance") %>% 
  ## Calculate Diversity indices of simulated communities
  group_by(Simulation, Community) %>%
  mutate(
    Richness = length(Species[Abundance != 0 & !is.na(Abundance)]),
    Shannon = -sum(Abundance * log(Abundance), na.rm = T), # Shannon (1948) (incl. n_spp, cover) cf. Spellerberg & Fedor, 2003 for controversy on index name/reference
    Simpson = sum(Abundance^2, na.rm = T) # Simpson's index (1949)
    ) %>%
  ungroup() %>% 
  dplyr::select(-Species, -Abundance) %>% 
  unique()

## Simulated vegetation indices and RaoQs
dat_all <- bind_rows(
  bind_rows(dat_all_sim, .id = "Simulation"),
  bind_rows(dat_all_rao, .id = "Simulation")
  ) %>%
  rename_with(.fn = unique, x = dat_all_biodiv$Community, .cols = contains("comm_")) %>% # correct Communities' names, likely wrong due to wrong export/import, i.e. column values included
  pivot_longer(cols = contains("Com_"), names_to = "Community", values_to = "Values") %>% 
  drop_na(Values) %>% 
  pivot_wider(names_from = Index, values_from = "Values") %>% 
  left_join(dat_all_biodiv, by = c("Simulation", "Community")) %>%
  mutate(Simulation = as.integer(Simulation) - 1) %>% # start with run00
  glimpse()


## Calculate derivative ----
dat_all <- dat_all %>%
  group_by(Simulation, Community) %>%
  mutate(
    f1_LAI = c(diff(LAI), NA_real_),
    f1_NDVI = c(diff(NDVI), NA_real_) / f1_LAI,
    f1_NIRv = c(diff(NIRv), NA_real_) / f1_LAI,
    f1_RaoQ_NDVI = c(diff(RaoQ_NDVI), NA_real_) / f1_LAI,
    f1_RaoQ_NIRv = c(diff(RaoQ_NIRv), NA_real_) / f1_LAI
  ) %>%
  ungroup() %>%
  glimpse()



## Aggregate all simulations ----
dat_all_agg <- dat_all %>%
  # dplyr::filter(Community == "comm15") %>% # for testing
  group_by(LAI, Community) %>%
  summarise(across(.cols = where(is.numeric), .fns = ~ sd(.x, na.rm = T), .names = "{.col}_sd"),
            across(.cols = where(is.numeric), .fns = ~ mean(.x, na.rm = T))
            ) %>%
  ungroup()



### Plot all simulations for supplementary figure ------------------------------
## Data for plotting ----
dat_plot <- dat_all %>%
  dplyr::filter(Simulation == 0) # only first simulation


## Specs ----
theme_custom <- theme_bw() +
  theme_combine +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
    )

# mypalette <- palette14_lumin
mypalette <- Thirteen_safe #colorRampPalette(Eight_categorical2)(length(unique(dat_plot$Community))) # generate color palette
# set.seed(005); mypalette <- sample(mypalette) # randomize order


## Subplots ----
## VIs
p_ndvi <- dat_plot %>% 
  ggplot(aes(x = LAI, y = NDVI, color = Community, group = Community)) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
  # geom_point(alpha = alpha_opaque, size = point_size_small, na.rm = T) +
  ylab("NDVI [-]") +
  # scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
  scale_y_continuous(labels = ~ format(.x, nsmall = 2)) + # labels with 2 small digits
  scale_color_manual(values = mypalette) + #scale_fill_manual(values = mypalette) +
  # scale_color_viridis_d(option = "D") +
  theme_custom + 
  guides(color = guide_legend(nrow = 2, byrow = T, override.aes = list(linewidth = line_width_extracthick))) +
  NULL

p_nirv <- dat_plot %>% 
  ggplot(aes(x = LAI, y = NIRv, color = Community, group = Community)) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
  # geom_point(alpha = alpha_opaque, size = point_size_small, na.rm = T) +
  ylab("NIRv [-]") +
  # scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
  scale_y_continuous(labels = ~ format(.x, nsmall = 2)) + # labels with 2 small digits
  scale_color_manual(values = mypalette) + #scale_fill_manual(values = mypalette) +
  # scale_color_viridis_d(option = "D") +
  theme_custom + 
  guides(color = guide_legend(nrow = 2, byrow = T, override.aes = list(linewidth = line_width_extracthick))) +
  NULL

## RaoQs
p_raoq_ndvi <- dat_plot %>% 
  ggplot(aes(x = LAI, y = RaoQ_NDVI, color = Community, group = Community)) + #linetype = Community, 
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
  # geom_point(alpha = alpha_opaque, size = point_size_small, na.rm = T) +
  xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  ylab(expression(paste(RaoQ[NDVI], " [-]"))) +
  # scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
  scale_y_continuous(labels = ~ format(.x, nsmall = 2)) + # labels with 2 small digits
  scale_color_manual(values = mypalette) + #scale_fill_manual(values = mypalette) +
  # scale_color_viridis_d(option = "F") +
  # scale_linetype_manual(values = rep(c("solid", "longdash"), length.out = length(unique(dat_plot$Community)))) +
  theme_custom + 
  guides(color = guide_legend(nrow = 2, byrow = T, override.aes = list(linewidth = line_width_extracthick))) +
  NULL

p_raoq_nirv <- dat_plot %>% 
  ggplot(aes(x = LAI, y = RaoQ_NIRv, color = Community, group = Community)) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
  # geom_point(alpha = alpha_opaque, size = point_size_small) +
  xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  ylab(expression(paste(RaoQ[NIRv], " [-]"))) +
  # scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
  scale_y_continuous(labels = ~ format(.x, nsmall = 2)) + # labels with 2 small digits
  scale_color_manual(values = mypalette) + #scale_fill_manual(values = mypalette) +
  # scale_color_viridis_d(option = "B") +
  theme_custom + 
  guides(color = guide_legend(nrow = 2, byrow = T, override.aes = list(linewidth = line_width_extracthick))) +
  NULL

## Deltas/derivatives of RaoQs
p_delta_raoq_ndvi <- dat_plot %>% 
  ggplot(aes(x = LAI + f1_LAI / 2, y = f1_RaoQ_NDVI, color = Community)) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
  # geom_point(alpha = alpha_opaque, size = point_size_small) +
  xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  ylab(expression(paste(delta~RaoQ[NDVI], " [-]"))) +
  # scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
  scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
  theme_custom +
  NULL

p_delta_raoq_nirv <- dat_plot %>% 
  ggplot(aes(x = LAI + f1_LAI / 2, y = f1_RaoQ_NIRv, color = Community)) +
  geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
  # geom_point(alpha = alpha_opaque, size = point_size_small) +
  xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  ylab(expression(paste(delta~RaoQ[NIRv], " [-]"))) +
  # scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
  scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
  theme_custom +
  NULL


## Combine subplots ----
p0 <- (
  (
    (
      (
        (p_ndvi | p_nirv) &
          theme(axis.title.x = element_blank(), axis.text.x = element_blank())
      ) /
        (p_raoq_ndvi | p_raoq_nirv)
    ) &
      scale_x_continuous(limits = c(0, 15.5), expand = c(0, 0)) # remove padding before and after limits
  ) /
    guide_area()
) +
  plot_layout(heights = c(0.45, 0.45, 0.1), guides = "collect") +
  plot_annotation(tag_levels = "a")
p0



# ### Plot all simulations for supplementary figure ------------------------------
# ## Data for plotting
# dat_plot <- dat_all_agg
# 
# 
# ## Specs
# theme_custom <- theme_bw() +
#   theme_combine +
#   theme(
#     legend.position = "none"
#   )
# 
# mypalette <- colorRampPalette(CatCol)(length(unique(dat_plot$Community))) # generate color palette
# set.seed(002); mypalette <- sample(mypalette)
# # mypalette <- colorRampPalette(Six_colorblind)(length(unique(dat_plot$Community))) # generate color palette
# # set.seed(001); mypalette <- sample(mypalette)
# 
# 
# ## Subplots
# ## VIs
# p_ndvi <- dat_plot %>% 
#   ggplot(aes(x = LAI, y = NDVI, color = Community, fill = Community)) +
#   geom_ribbon(aes(ymin = NDVI - NDVI_sd, ymax = NDVI + NDVI_sd),
#               alpha = alpha_very_transparent, linewidth = NA
#   ) +
#   geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
#   # geom_point(alpha = alpha_opaque, size = point_size_small, na.rm = T) +
#   ylab("NDVI [-]") +
#   scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
#   # scale_color_viridis_d(option = "D") +
#   theme_custom +
#   NULL
# 
# p_nirv <- dat_plot %>% 
#   ggplot(aes(x = LAI, y = NIRv, color = Community, fill = Community)) +
#   geom_ribbon(aes(ymin = NIRv - NIRv_sd, ymax = NIRv + NIRv_sd),
#               alpha = alpha_very_transparent, linewidth = NA
#   ) +
#   geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
#   # geom_point(alpha = alpha_opaque, size = point_size_small, na.rm = T) +
#   ylab("NIRv [-]") +
#   scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
#   # scale_color_viridis_d(option = "D") +
#   theme_custom +
#   NULL
# 
# ## RaoQs
# p_raoq_ndvi <- dat_plot %>% 
#   ggplot(aes(x = LAI, y = RaoQ_NDVI, color = Community, fill = Community)) +
#   geom_ribbon(aes(ymin = RaoQ_NDVI - RaoQ_NDVI_sd, ymax = RaoQ_NDVI + RaoQ_NDVI_sd),
#               alpha = alpha_very_transparent, linewidth = NA
#   ) +
#   geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
#   # geom_point(alpha = alpha_opaque, size = point_size_small, na.rm = T) +
#   xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
#   ylab(expression(paste(RaoQ[NDVI], " [-]"))) +
#   scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
#   # scale_color_viridis_d(option = "H") +
#   theme_custom +
#   NULL
# 
# p_raoq_nirv <- dat_plot %>% 
#   ggplot(aes(x = LAI, y = RaoQ_NIRv, color = Community, fill = Community)) +
#   geom_ribbon(aes(ymin = RaoQ_NIRv - RaoQ_NIRv_sd, ymax = RaoQ_NIRv + RaoQ_NIRv_sd),
#               alpha = alpha_very_transparent, linewidth = NA
#   ) +
#   geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
#   # geom_point(alpha = alpha_opaque, size = point_size_small) +
#   xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
#   ylab(expression(paste(RaoQ[NIRv], " [-]"))) +
#   scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
#   # scale_color_viridis_d(option = "D") +
#   theme_custom +
#   NULL
# 
# ## Deltas/derivatives of RaoQs
# p_delta_raoq_ndvi <- dat_plot %>% 
#   ggplot(aes(x = LAI + f1_LAI / 2, y = f1_RaoQ_NDVI, color = Community, fill = Community)) +
#   geom_ribbon(aes(ymin = f1_RaoQ_NDVI - f1_RaoQ_NDVI_sd, ymax = f1_RaoQ_NDVI + f1_RaoQ_NDVI_sd),
#               alpha = alpha_very_transparent, linewidth = NA
#   ) +
#   geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
#   # geom_point(alpha = alpha_opaque, size = point_size_small) +
#   xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
#   ylab(expression(paste(delta~RaoQ[NDVI], " [-]"))) +
#   scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
#   theme_custom +
#   NULL
# 
# p_delta_raoq_nirv <- dat_plot %>% 
#   ggplot(aes(x = LAI + f1_LAI / 2, y = f1_RaoQ_NIRv, color = Community, fill = Community)) +
#   geom_ribbon(aes(ymin = f1_RaoQ_NIRv - f1_RaoQ_NIRv_sd, ymax = f1_RaoQ_NIRv + f1_RaoQ_NIRv_sd),
#               alpha = alpha_very_transparent, linewidth = NA
#   ) +
#   geom_line(alpha = alpha_opaque, linewidth = line_width_medium, na.rm = T) +
#   # geom_point(alpha = alpha_opaque, size = point_size_small) +
#   xlab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
#   ylab(expression(paste(delta~RaoQ[NIRv], " [-]"))) +
#   scale_x_continuous(expand = c(0, 0)) + # remove padding before and after limits
#   scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette) +
#   theme_custom +
#   NULL
# 
# 
# ## Combine subplots
# p_all <- (
#   ((p_ndvi |
#       (p_nirv & theme(axis.text.y = element_blank()))
#   ) &
#     theme(axis.title.x = element_blank(), axis.text.x = element_blank()) &
#     ylim(c(0.1, 1))
#   ) /
#     ((p_raoq_ndvi |
#         (p_raoq_nirv & theme(axis.text.y = element_blank()))
#     ) &
#       ylim(c(0, 0.16)) &
#       theme(axis.title.x = element_blank(), axis.text.x = element_blank())
#     ) /
#     ((p_delta_raoq_ndvi |
#         (p_delta_raoq_nirv & theme(axis.text.y = element_blank()))) &
#        xlim(c(0.25, 15.5)) & ylim(-0.25, 0.2))
# ) +
#   plot_annotation(tag_levels = "a")
# p_all



### Save -----------------------------------------------------------------------
if (savedata) {
  ## Figure 4
  ggsave(filename = glue::glue("results/scatterplots/figure4_RaoQ_simulations_{vers_out}.jpg"),
         plot = p0, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 16:9 ratio
  
  # ## Supplementary figure
  # ggsave(filename = glue::glue("results/scatterplots/figureSX_RaoQ_simulations_ALL_{vers_out}.jpg"),
  #        plot = p_all, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300) # 16:9 ratio
  
}