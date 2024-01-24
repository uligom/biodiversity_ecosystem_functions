#### COMPARE SATELLITE + GROUND BIODIVERSITY AND PHENO-DIVERSITY

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
igbp_subset <- "all" # should sites be subsetted based on vegetation type? ("all", or "forest")

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
library(ggpubr)       # publication ready plots
library(ggrepel)      # repelling labels
library(lubridate)    # dates
library(readr)        # tidy read/save
library(RColorBrewer) # color palettes
library(stringr)      # work with strings
library(tidyr)

## Functions
source("scripts/functions/extract_biodiv_metrics.R")
source("scripts/functions/plot_scatterplot_models.R")
source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
## metadata
igbp_neon <- read_csv("data/input/NEON/neon_igbp.csv", show_col_types = F)
igbp_icos <- read_csv("data/input/ICOS/icos_igbp.csv", show_col_types = F)

## Ground biodiversity metrics and raoq
dat <- extract_biodiv_metrics(biodiv_indices = T, phenology = F, raoq = T) %>% # phenology will be compiled later in this script
  glimpse()


## Pheno-diversity
file_string <- "1C_300m_Avail0.7-10_noNegNDVI_OutlierRemX3_DatElongTx2_v03" # input key
load(file = glue::glue("data/inter/phenodiv_{file_string}.RData")) #read_csv(file = glue::glue("data/inter/phenodiv_{file_string}.csv"))

# ground phenology intensity
phe_ground <- read_csv(glue::glue("data/inter/phen_ground_neon.csv"), show_col_types = F)



### Combine metrics ----------------------------------------------------------
dat <- dat %>%
  left_join(phenodiv, by = "SITE_ID") %>%
  ## Add IGBP or vegetation classes
  left_join(igbp_icos, by = "SITE_ID") %>%
  left_join(igbp_neon, by = "SITE_ID", suffix = c("", ".y")) %>% 
  mutate(IGBP = if_else(is.na(IGBP), IGBP.y, IGBP),
         IGBP.y = NULL) %>% 
  relocate(IGBP, .after = SITE_ID) %>% 
  glimpse()



### Scatterplot ----------------------------------------------------------------
for (pp in 4:ncol(dat)) {
  var_y <- names(dat)[pp] %>% rlang::sym()
  if (typeof(dat %>% pull(!!var_y)) != "list") {
    p_scat <- dat %>% 
      # filter(str_detect(SITE_ID, "[:upper:]{4}")) %>% # NEON sites
      # filter(str_detect(SITE_ID, "[:upper:]{2}-")) %>% # ICOS sites
      plot_scatterplot_models(x = n_spp, y = !!var_y, color = "IGBP", label = "SITE_ID")
    print(p_scat)
    
    if (savedata == T) {
      ggsave(filename = glue::glue("nSpp_vs_{as.character(var_y)}.jpg"),
             plot = p_scat, device = "jpeg", path = "results/scatterplots",
             width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
    }
  } else if (typeof(dat %>% pull(!!var_y)) == "list") {next}
} # end scatterplots for loop


### Plot correlation matrix ----------------------------------------------------
text_color <- "gray25"
p_corr <- dat %>%
  # filter(str_detect(SITE_ID, "[:upper:]{4}")) %>% # NEON sites
  # filter(str_detect(SITE_ID, "[:upper:]{2}-")) %>% # ICOS sites
  dplyr::select(-SITE_ID, -IGBP, -phe_var) %>%
  cor(use = "complete.obs", method = "spearman") %>%
  ggcorrplot::ggcorrplot(method = "square", ggtheme = "theme_classic", show.diag = F,
                         type = "upper", colors = brewer.pal(3, "RdBu"),
                         lab = T, lab_col = text_color, lab_size = 5.5
                         ) +
  theme(axis.text.x  = element_text(color = text_color, size = 24, angle = 45),
        axis.text.y  = element_text(color = text_color, size = 24),
        axis.ticks   = element_blank(),
        legend.box.spacing = margin(0, 0, 0, 1.5, unit = "cm"), # box distance from plot panel
        legend.key.height = unit(4.22, "cm"), # height of legend bar
        legend.key.width = unit(3, "cm"),  # width of legend bar
        legend.text  = element_text(color = text_color, size = 24),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid   = element_line(color = "gray75")
  ) +
  theme( # transparent background
    panel.background  = element_rect(fill = 'transparent'), # transparent panel bg
    plot.background   = element_rect(fill = 'transparent', color = NA), # transparent plot bg
    # panel.grid.major = element_blank(), # remove major gridlines
    panel.grid.minor  = element_blank(), # remove minor gridlines
    legend.background = element_rect(fill = 'transparent'), # transparent legend bg
    legend.box.background = element_rect(colour = 'transparent', fill = 'transparent') # transparent legend panel and legend box
  )
print(p_corr)

if (savedata) {
  ggsave(glue::glue("/spearman_{file_string}.jpg"),
         plot = p_corr, device = "jpeg", path = "results/metrics_corr",
         width = 400, height = 300, units = "mm", dpi = 300)
}



### Process phenodiv and ground phenology --------------------------------------
## Process ground phenology data ----
phe_ground <- phe_ground %>% # select variables
  dplyr::select(SITE_ID, editedDate, phenophaseName, phenophaseStatus, intensity_std) %>%
  drop_na(phenophaseStatus) %>% # remove empty entries
  unique() %>% # remove duplicates
  arrange(SITE_ID) %>% 
  glimpse()
gc() # clean memory

phe_ground <- phe_ground %>% # summarise phenophase categories
  mutate(pheno_cat = case_when(
    is.na(intensity_std)                     ~ "None",
    phenophaseName == "Leaves"               ~ "Canopy_fullness",
    phenophaseName == "Needles"              ~ "Canopy_fullness",
    phenophaseName == "Breaking leaf buds"   ~ "Greening",
    phenophaseName == "Breaking needle buds" ~ "Greening",
    phenophaseName == "Emerging needles"     ~ "Greening",
    phenophaseName == "Increasing leaf size" ~ "Greening",
    phenophaseName == "Initial growth"       ~ "Greening",
    phenophaseName == "Young leaves"         ~ "Greening",
    phenophaseName == "Young needles"        ~ "Greening",
    phenophaseName == "Open flowers"         ~ "Flowering",
    phenophaseName == "Open pollen cones"    ~ "Flowering",
    phenophaseName == "Fruits"               ~ "Fruiting",
    phenophaseName == "Ripe fruits"          ~ "Fruiting",
    phenophaseName == "Colored leaves"       ~ "Senescence",
    phenophaseName == "Colored needles"      ~ "Senescence",
    phenophaseName == "Falling leaves"       ~ "Senescence",
    phenophaseName == "Falling needles"      ~ "Senescence"
  ),
  .after = phenophaseName
  ) %>%
  mutate(intensity_std = if_else(pheno_cat == "None", 0, intensity_std), # convert presence = F measurements to 0
         DOY = lubridate::yday(editedDate) # add day of year
  )

## Add year column
phe_ground <- phe_ground %>%
  mutate(YEAR = year(editedDate)) %>% # add year column
  glimpse()


## Filter phenological categories ----
phe_ground <- phe_ground %>%
  dplyr::filter(pheno_cat != "Fruiting" & pheno_cat != "None") %>% # remove uninteresting categories
  glimpse()


## Filter outliers ----
# Keep 95% interval (2.5%-97.5%)
phe_ground <- phe_ground %>%
  group_by(SITE_ID) %>% 
  mutate(q2_5 = quantile(intensity_std, 0.025, na.rm = T),
         q97_5 = quantile(intensity_std, 0.975, na.rm = T),
         .after = intensity_std) %>% 
  ungroup() %>% 
  dplyr::filter((intensity_std > q2_5 & intensity_std < q97_5) | phenophaseStatus == F) %>% 
  glimpse()
gc()


## Process satellite phenology data ----
phenodiv <- phenodiv %>% unnest(cols = phe_var) %>% # unnest
  mutate(sat_phen_std = sqrt(phe_var)) # calculate standard deviation

# phenodiv %>%
#   ggplot(aes(x = DOY, y = sat_phen_std, color = SITE_ID)) +
#   geom_line() +
#   # geom_label_repel(aes(label = SITE_ID), nudge_x = 1, na.rm = T) +
#   theme_classic() +
#   # theme(legend.position = "none") +
#   NULL


## Normalize sat and ground ----
phenodiv <- phenodiv %>% 
  mutate(sat_phen_std_norm = sat_phen_std / phe_std_max) # normalize between 0 and 1 fr each site

phe_ground <- phe_ground %>%
  group_by(SITE_ID) %>% 
  mutate(ground_std_max = max(intensity_std, na.rm = T), # maximum of stadard deviation for normalization
         ground_std_max = if_else(is.finite(ground_std_max), ground_std_max, NA_real_), # set infinite to NA
         ground_phen_std_norm = intensity_std / ground_std_max, # normalize between 0 and 1 fr each site
         .after = intensity_std) %>% 
  ungroup() %>% 
  glimpse()


## Merge satellite and ground ----
dat_phen <- phenodiv %>% 
  left_join(phe_ground, by = c("SITE_ID", "DOY")) %>% 
  glimpse()


## Extract vectors ----
SITES <- dat_phen %>%
  drop_na(sat_phen_std, intensity_std) %>% arrange(SITE_ID) %>%
  pull(SITE_ID) %>% unique()

YEARS <- dat_phen %>%
  drop_na(YEAR) %>% arrange(YEAR) %>%
  pull(YEAR) %>% unique()

CATS <- dat_phen %>%
  drop_na(pheno_cat) %>% 
  pull(pheno_cat) %>% unique()


## Aggregate to 10 days windows ----
phen_agg <- dat_phen %>%
  mutate(DOY = round(DOY, -1), .after = DOY) %>% # aggregate to 10 days means
  group_by(SITE_ID, DOY, pheno_cat) %>%
  summarise(sat_phen_std = mean(sat_phen_std, na.rm = T),
            sat_phen_std_norm = mean(sat_phen_std_norm, na.rm = T),
            ground_phen_std = mean(intensity_std, na.rm = T),
            ground_phen_std_norm = mean(ground_phen_std_norm, na.rm = T),
            .groups = "drop") %>%
  mutate(ground_phen_std = if_else(is.nan(ground_phen_std), NA_real_, ground_phen_std),
         ground_phen_std_norm = if_else(is.nan(ground_phen_std_norm), NA_real_, ground_phen_std_norm)
         ) %>% 
  glimpse()



### Plot Phenology -------------------------------------------------------------
## Colors ----
myCol <- c(Flowering = "#B22D6E", # light purple: flowering
           Fruiting = "#F15620", # dark orange: fruiting
           Greening = "#1EB8B3", # cyan: greening
           Leaves_needles = "#73C03C", # light green: leaves
           Senescence = "#FDB71E", # gold: senescence
           None = "#808080" # gray: empty data
)


## Function for plotting ----
plot_phen <- function(data = dat_plot, x = DOY, y_ground = intensity_std, y_sat = sat_phen_std,
                      color = pheno_cat, palette = myCol, title = "",
                      save_name = glue::glue("/phen_groundStd_sat_{file_string}_{idx[1]}-{idx[length(idx)]}.jpg")) {
  ## Quote
  require(rlang)
  
  x <- rlang::enquo(x)
  y_ground <- rlang::enquo(y_ground)
  y_sat <- rlang::enquo(y_sat)
  color <- rlang::enquo(color)
  
  ## Plot
  p_phen <- data %>% 
    ggplot() +
    geom_point(aes(x = !!x, y = !!y_sat, fill = ""), alpha = 1, size = 1.5) + # plot satellite data
    scale_fill_manual(labels = "Pheno-diversity", values = "black") +
    geom_point(aes(x = !!x, y = !!y_ground, color = !!color), # plot ground data
               alpha = 0.5, size = 1.5, na.rm = T, show.legend = T) +
    scale_color_manual(values = palette) +
    scale_y_continuous(
      name = "Standard deviation of Pheno-diversity",
      sec.axis = sec_axis(~ ., name = "Standard deviation of Pheno-phase Intensity")
    ) +
    facet_wrap(. ~ SITE_ID) +
    labs(title = title) +
    theme_classic() +
    theme(text = element_text(size = 16),
          title = element_text(size = 16),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16),
          panel.border = element_rect(fill = NA),
          strip.text = element_text(size = 16),
          strip.background = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 0.67, size = 6), title = "Pheno-phase (ground)"),
           fill = guide_legend(override.aes = list(alpha = 1, size = 6), title = "")
    ) +
    NULL
  print(p_phen)
  
  if (savedata == T) {
    ggsave(filename = save_name,
           plot = p_phen, device = "jpeg", path = "results/phenodiv",
           width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
  }
}
# debugonce(plot_phen)


## All data ----
## Prepare data for plotting
dat_plot <- phen_agg %>%
  # dplyr::filter(SITE_ID %in% SITES[idx]) %>% # select sites
  dplyr::select(SITE_ID, DOY, sat_phen_std_norm, ground_phen_std_norm, pheno_cat) %>% 
  unique() %>% 
  glimpse()

plot_phen(data = dat_plot, x = DOY, y_ground = ground_phen_std_norm, y_sat = sat_phen_std_norm, title = "",
          save_name = glue::glue("/phen_groundStd_sat_{file_string}_AllSites.jpg"))


## Single categories ----
dat_plot <- phen_agg %>%
  dplyr::select(SITE_ID, DOY, sat_phen_std_norm, ground_phen_std_norm, pheno_cat) %>% 
  unique() %>% 
  glimpse()

for (pp in 1:length(CATS)) {
  dat_plot %>%
    mutate(ground_phen_std_norm = if_else(pheno_cat == CATS[pp], ground_phen_std_norm, NA_real_),
           pheno_cat = if_else(pheno_cat == CATS[pp], pheno_cat, NA_character_)) %>% 
    plot_phen(data = ., x = DOY, y_ground = ground_phen_std_norm, y_sat = sat_phen_std_norm, title = "",
              save_name = glue::glue("/phen_groundStd_{CATS[pp]}_sat_{file_string}_AllSites.jpg"))
}


## Single years ----
## Aggregate for single years
dat_plot <- dat_phen %>%
  mutate(DOY = round(DOY, -1), .after = DOY) %>% # aggregate to 10 days means
  group_by(SITE_ID, YEAR, DOY, pheno_cat) %>%
  summarise(sat_phen_std = mean(sat_phen_std, na.rm = T),
            sat_phen_std_norm = mean(sat_phen_std_norm, na.rm = T),
            ground_phen_std = mean(intensity_std, na.rm = T),
            ground_phen_std_norm = mean(ground_phen_std_norm, na.rm = T),
            .groups = "drop") %>%
  dplyr::select(SITE_ID, DOY, YEAR, sat_phen_std_norm, ground_phen_std_norm, pheno_cat) %>% 
  unique() %>% 
  glimpse()

for (pp in 1:length(YEARS)){
  dat_plot %>%
    mutate(ground_phen_std_norm = if_else(YEAR == YEARS[pp], ground_phen_std_norm, NA_real_),
           pheno_cat = if_else(YEAR == YEARS[pp], pheno_cat, NA_character_)) %>% 
    plot_phen(data = ., x = DOY, y_ground = ground_phen_std_norm, y_sat = sat_phen_std_norm, title = YEARS[pp],
              save_name = glue::glue("/phen_groundStd_{YEARS[pp]}_sat_{file_string}_AllSites.jpg"))
}



### Peak/mean of phenological phases -------------------------------------------
## Peak of ground phenology ----
dat_plot <- phen_agg %>% 
  drop_na(pheno_cat) %>% 
  group_by(SITE_ID, pheno_cat) %>%
  mutate(ground_std_max = max(ground_phen_std, na.rm = T), # maximum of standard deviation for normalization
         ground_std_max = if_else(is.finite(ground_std_max), ground_std_max, NA_real_) # set infinite to NA
  ) %>%
  ungroup() %>%
  dplyr::filter(ground_phen_std == ground_std_max) %>% 
  glimpse()

t_cor <- cor(dat_plot %>% dplyr::select(ground_phen_std, sat_phen_std), method = "spearman")

# dat_lm <- dat_plot %>% 
#   dplyr::filter(pheno_cat == "Greening") %>% 
#   dplyr::select(sat_phen_std, ground_phen_std) %>% 
#   as.data.frame()
# lm1 <- lm(formula = sat_phen_std ~ ground_phen_std, data = dat_lm)
# summary(lm1)


## Plot
dat_plot %>% 
  ggplot(aes(x = ground_phen_std, y = sat_phen_std)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(. ~ pheno_cat) +
  labs(title = "Peak of ground phenology std", caption = paste0("Spearman correlation = ", round(t_cor[2], 2))) +
  theme_classic() +
  theme(text = element_text(size = 16),
        title = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(fill = NA),
        strip.text = element_text(size = 16),
        strip.background = element_blank()
  ) +
  NULL


## Peak of ndvi phenology ----
dat_plot <- phen_agg %>% 
  drop_na(pheno_cat) %>% 
  group_by(SITE_ID, pheno_cat) %>%
  mutate(sat_std_max = max(sat_phen_std, na.rm = T), # maximum of standard deviation for normalization
         sat_std_max = if_else(is.finite(sat_std_max), sat_std_max, NA_real_) # set infinite to NA
  ) %>%
  ungroup() %>%
  dplyr::filter(sat_phen_std == sat_std_max) %>% 
  glimpse()

t_cor <- cor(dat_plot %>% dplyr::select(ground_phen_std, sat_phen_std), method = "spearman")

## Plot
dat_plot %>% 
  ggplot(aes(x = ground_phen_std, y = sat_phen_std)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(. ~ pheno_cat) +
  labs(title = "Peak of ndvi phenology std", caption = paste0("Spearman correlation = ", round(t_cor[2], 2))) +
  theme_classic() +
  theme(text = element_text(size = 16),
        title = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(fill = NA),
        strip.text = element_text(size = 16),
        strip.background = element_blank()
  ) +
  NULL


## Mean of ground phenology ----
dat_plot <- phen_agg %>% 
  drop_na(pheno_cat) %>% 
  group_by(SITE_ID, pheno_cat) %>%
  mutate(ground_std_mean = mean(ground_phen_std, na.rm = T) # for normalization
  ) %>%
  ungroup() %>%
  dplyr::filter(ground_phen_std > ground_std_mean - ground_std_mean/100 & ground_phen_std < ground_std_mean + ground_std_mean/100) %>% 
  glimpse()

t_cor <- cor(dat_plot %>% dplyr::select(ground_phen_std, sat_phen_std), method = "spearman")


## Plot
dat_plot %>% 
  ggplot(aes(x = ground_phen_std, y = sat_phen_std)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(. ~ pheno_cat) +
  labs(title = "Mean of ground phenology std", caption = paste0("Spearman correlation = ", round(t_cor[2], 2))) +
  theme_classic() +
  theme(text = element_text(size = 16),
        title = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(fill = NA),
        strip.text = element_text(size = 16),
        strip.background = element_blank()
  ) +
  NULL



## Mean of ndvi phenology ----
dat_plot <- phen_agg %>% 
  drop_na(pheno_cat) %>% 
  group_by(SITE_ID, pheno_cat) %>%
  mutate(sat_std_mean = mean(sat_phen_std, na.rm = T) # for normalization
  ) %>%
  ungroup() %>%
  dplyr::filter(sat_phen_std > sat_std_mean - sat_std_mean/100 & sat_phen_std < sat_std_mean + sat_std_mean/100) %>% 
  glimpse()

t_cor <- cor(dat_plot %>% dplyr::select(ground_phen_std, sat_phen_std), method = "spearman")

## Plot
dat_plot %>% 
  ggplot(aes(x = ground_phen_std, y = sat_phen_std)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(. ~ pheno_cat) +
  labs(title = "Mean of ndvi phenology std", caption = paste0("Spearman correlation = ", round(t_cor[2], 2))) +
  theme_classic() +
  theme(text = element_text(size = 16),
        title = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.border = element_rect(fill = NA),
        strip.text = element_text(size = 16),
        strip.background = element_blank()
  ) +
  NULL



## Aggregate to whole site metrics ----
phen_site <- phen_agg %>%
  dplyr::select(-sat_phen_std_norm, -ground_phen_std_norm) %>% 
  group_by(SITE_ID) %>%
  summarise(sat_phen_std = mean(sat_phen_std, na.rm = T),
            ground_phen_std = mean(ground_phen_std, na.rm = T),
            .groups = "drop") %>%
  mutate(sat_phen_std = if_else(is.nan(sat_phen_std), NA_real_, sat_phen_std),
         ground_phen_std = if_else(is.nan(ground_phen_std), NA_real_, ground_phen_std)) %>% 
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(phen_site, "data/inter/phen_ground_sat.csv")
}