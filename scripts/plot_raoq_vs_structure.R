#### BUILD TABLE OF RAOQ x STRUCTURE

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
dat_in <- "v06.06"
vers_out <- paste0(dat_in, "_rev2")



### Utilities ------------------------------------------------------------------
## Packages
# library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
library(ggrepel)      # repelled labels
library(patchwork)    # combine and arrange plots
library(readr)        # tidy read/save
# library(scales)       # package for rescaling
library(stringr)
library(tidyr)        # reorganize tibbles

## Functions
source("scripts/functions/min_max_norm.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("data/inter/data_efps_clim_struct_biodiv_{dat_in}.csv"), show_col_types = F) %>%
  glimpse()



### Process data ---------------------------------------------------------------
dat_sub <- dat %>% 
  select(SITE_ID, IGBP,
         Rao_Q_NIRv, Rao_Q_NDVI, NDVI_max,
         stem_diameter, height_max, max_crown_diameter, max_base_crown_diameter, LAI_max, height,
         NDVI_max, NIRv_max
         ) %>% 
  mutate(across(where(is.double), min_max_norm)) %>% # normalize
  glimpse()

dat_long <- dat_sub %>% 
  pivot_longer(cols = contains("Rao_Q"), names_to = "Rao_Q_names", values_to = "Rao_Q_values") %>%
  pivot_longer(cols = c(!contains("Rao_Q"), -SITE_ID, -IGBP), names_to = "struct_names", values_to = "struct_values") %>%
  glimpse()



### Plot -----------------------------------------------------------------------
# ## RaoQ vs all structure ----
# p_allstruct <- dat_plot %>% 
#   dplyr::filter(struct_values != 0) %>% 
#   ggplot(aes(x = struct_values, y = Rao_Q_values)) +
#   geom_smooth(aes(group = Rao_Q_names), method = "lm", formula = "y ~ x", na.rm = T) +
#   geom_point(aes(color = IGBP, shape = struct_names), size = point_size_small, na.rm = T) +
#   geom_hline(yintercept = 0.5, linetype = "dashed") +
#   geom_vline(xintercept = 0.5, linetype = "dashed") +
#   facet_wrap(. ~ Rao_Q_names, labeller = label_parsed, nrow = 2, strip.position = "left") +
#   scale_color_manual(values = CatCol_igbp) +
#   xlab("Structure (all metrics, standardized)") + ylab("Rao Q (standardized)") +
#   guides(color = guide_legend(title = "IGBP class"),
#          shape = guide_legend(title = "Structural variable")
#          ) +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = text_size_small),
#     axis.title = element_text(size = text_size_big),
#     legend.text = element_text(size = text_size_medium),
#     legend.title = element_text(size = text_size_big),
#     # panel.border = element_rect(fill = NA),
#     # panel.spacing = unit(0.5, "cm"),
#     # plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
#     # plot.caption = element_text(size = 28),
#     # plot.title = element_text(size = 40, face = "bold"),
#     strip.text = element_text(size = text_size_big), # facet title strips
#     strip.background = element_rect(fill = "white")
#   ) +
#   NULL
# p_allstruct
# 
#
# ## RaoQ vs structure panels ----
# p_struct_facets <- dat_long %>% 
#   ggplot(aes(x = struct_values, y = Rao_Q_values, fill = IGBP)) +
#   geom_point(color = point_border_color, shape = point_shape, size = point_size_small, na.rm = T) +
#   geom_hline(yintercept = 0.5, linetype = "dashed") +
#   geom_vline(xintercept = 0.5, linetype = "dashed") +
#   facet_wrap(Rao_Q_names ~ struct_names, nrow = 2) +
#   scale_fill_manual(values = CatCol_igbp) +
#   xlab("Structure (standardized)") + ylab("Rao Q (standardized)") +
#   theme_bw() +
#     theme(
#       axis.text = element_text(size = text_size_medium),
#       axis.title = element_text(size = text_size_big),
#       legend.text = element_text(size = text_size_medium),
#       legend.title = element_text(size = text_size_big),
#       # panel.border = element_rect(fill = NA),
#       # panel.spacing = unit(0.5, "cm"),
#       # plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
#       # plot.caption = element_text(size = 28),
#       # plot.title = element_text(size = 40, face = "bold"),
#       strip.text = element_text(size = text_size_big * 0.6), # facet title strips
#       strip.background = element_rect(fill = "white")
#     ) +
#   NULL
# p_struct_facets
# 
# 
#
### RaoQ vs structure boxplots by class (2) ------------------------------------
## Plot function and specs ----
small_digits <- 2 # for number display

plot_struct_boxplot_bins <- function(
    data = dat_plot, x, xlab = "", group
) {
  require(rlang)
  require(tidyr)
  
  x <- rlang::ensym(x)
  group <- rlang::ensym(group)
  
  n <- data %>% drop_na(!!x, contains("Rao_Q")) %>% nrow()
  
  p_out <- data %>%
    pivot_longer(cols = contains("Rao_Q"), names_to = "Rao_Q_names", values_to = "Rao_Q_values") %>%
    # pivot_longer(cols = c(max_crown_diameter, LAI_max), names_to = "struct_names", values_to = "struct_values") %>%
    mutate( # add labels
      Rao_Q_names = factor(Rao_Q_names,
                           levels = c("Rao_Q_NDVI", "Rao_Q_NIRv"),
                           labels = c("RaoQ[NDVI]", "RaoQ[NIRv]") #y_labels_r2
      )
    ) %>%
    ggplot(aes(x = !!x, y = Rao_Q_values)) +
    geom_boxplot(aes(group = !!group),
                 varwidth = F, alpha = 1, color = "black", na.rm = T,
                 outlier.size = point_size_medium, outlier.shape = 3, outlier.stroke = line_width_thick) +
    # scale_fill_brewer(palette = "Set2") +
    geom_point(aes(fill = IGBP), alpha = 0.75, color = point_border_color, shape = point_shape, size = point_size_medium_small, na.rm = T) +
    scale_fill_manual(values = CatCol_igbp) +
    # ggrepel::geom_label_repel(aes(label = SITE_ID), alpha = 0.5, na.rm = T) + # site id labels
    # # geom_hline(yintercept = 0.5, linetype = "dashed") +
    # # geom_vline(xintercept = 0.5, linetype = "dashed") +
    facet_wrap(. ~ Rao_Q_names, labeller = label_parsed, nrow = 2, strip.position = "left", scales = "free_y") +
    guides(fill = guide_legend(title = "IGBP class", override.aes = list(size = point_size_big)),
           #group = guide_legend(title = "Structure group")
    ) +
    xlab(xlab) + ylab("Rao Q") +
    labs(caption = glue::glue("n = {n}")) +
    theme_bw() +
    theme_combine +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 75, vjust = 0.5)
      ) +
    NULL
  return(p_out)
}


## Max crown diameter ----
p_crown_classes <- dat %>%
  select(SITE_ID, IGBP,
         Rao_Q_NIRv, Rao_Q_NDVI,
         max_crown_diameter # ==> significant structural predictors of RaoQ in linear models
  ) %>%
  drop_na() %>%
  # mutate(across(where(is.double), min_max_norm)) %>% # normalize
  arrange(max_crown_diameter) %>%
  mutate( # discretize structure into equal-sized classes
    max_crown_diameter_class = cut_number(max_crown_diameter, 4),
    # max_crown_diameter_class = as.numeric(max_crown_diameter_class) / 5
    max_crown_diameter_class = factor(
      max_crown_diameter_class,
      levels = unique(max_crown_diameter_class),
      labels = paste0(
        str_extract(unique(max_crown_diameter_class), pattern = "[:digit:]+.?[:digit:]*(?=,)") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits),
        " - ",
        str_extract(unique(max_crown_diameter_class), pattern = "(?<=,)[:digit:]+.?[:digit:]*(?=]|\\))") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits)
        )
      )
    ) %>%
  glimpse() %>% 
  plot_struct_boxplot_bins(
    x = max_crown_diameter_class, xlab = expression(paste("Crown Diameter (max) [", m, "]")), group = max_crown_diameter_class
  )

## LAI ----
p_lai_classes <- dat %>%
  select(SITE_ID, IGBP,
         Rao_Q_NIRv, Rao_Q_NDVI,
         LAI_max # ==> significant structural predictors of RaoQ in linear models
  ) %>%
  drop_na() %>%
  arrange(LAI_max) %>%
  mutate( # discretize structure into equal-sized classes
    LAI_max_class = cut_number(LAI_max, 4),
    LAI_max_class = factor(
      LAI_max_class,
      levels = unique(LAI_max_class),
      labels = paste0(
        str_extract(unique(LAI_max_class), pattern = "[:digit:]+.?[:digit:]*(?=,)") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits),
        " - ",
        str_extract(unique(LAI_max_class), pattern = "(?<=,)[:digit:]+.?[:digit:]*(?=]|\\))") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits)
        )
      )
    ) %>%
  glimpse() %>% 
  plot_struct_boxplot_bins(
    x = LAI_max_class, xlab = expression(paste("Leaf Area Index (max) [",m^{2}," ", m^{-2},"]")), group = LAI_max_class
  )

## Height max ----
p_height_classes <- dat %>%
  select(SITE_ID, IGBP,
         Rao_Q_NIRv, Rao_Q_NDVI,
         height_max # ==> significant structural predictors of RaoQ in linear models
  ) %>%
  drop_na() %>%
  arrange(height_max) %>%
  mutate( # discretize structure into equal-sized classes
    height_max_class = cut_number(height_max, 4),
    height_max_class = factor(
      height_max_class,
      levels = unique(height_max_class),
      labels = paste0(
        str_extract(unique(height_max_class), pattern = "[:digit:]+.?[:digit:]*(?=,)") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits),
        " - ",
        str_extract(unique(height_max_class), pattern = "(?<=,)[:digit:]+.?[:digit:]*(?=]|\\))") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits)
      )
    )
  ) %>%
  glimpse() %>% 
  plot_struct_boxplot_bins(
    x = height_max_class, xlab = expression(paste("Canopy Height (max) [", m, "]")), group = height_max_class
  )


## Stem diameter ----
p_stem_diameter_classes <- dat %>%
  select(SITE_ID, IGBP,
         Rao_Q_NIRv, Rao_Q_NDVI,
         stem_diameter # ==> significant structural predictors of RaoQ in linear models
  ) %>%
  drop_na() %>%
  arrange(stem_diameter) %>%
  mutate( # discretize structure into equal-sized classes
    stem_diameter_class = cut_number(stem_diameter, 4),
    stem_diameter_class = factor(
      stem_diameter_class,
      levels = unique(stem_diameter_class),
      labels = paste0(
        str_extract(unique(stem_diameter_class), pattern = "[:digit:]+.?[:digit:]*(?=,)") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits),
        " - ",
        str_extract(unique(stem_diameter_class), pattern = "(?<=,)[:digit:]+.?[:digit:]*(?=]|\\))") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits)
      )
    )
  ) %>%
  glimpse() %>% 
  plot_struct_boxplot_bins(
    x = stem_diameter_class, xlab = expression(paste("Stem Diameter [", m, "]")), group = stem_diameter_class
  )


## NDVI max ----
cor.test(dat$Rao_Q_NDVI, dat$NDVI_max, method = "spearman")

p_ndvimax_classes <- dat %>%
  select(SITE_ID, IGBP,
         Rao_Q_NIRv, Rao_Q_NDVI,
         NDVI_max # ==> significant structural predictors of RaoQ in linear models
  ) %>%
  drop_na() %>%
  # mutate(across(where(is.double), min_max_norm)) %>% # normalize
  arrange(NDVI_max) %>%
  mutate( # discretize structure into equal-sized classes
    NDVI_max_class = cut_number(NDVI_max, 4),
    NDVI_max_class = factor(
      NDVI_max_class,
      levels = unique(NDVI_max_class),
      labels = paste0(
        str_extract(unique(NDVI_max_class), pattern = "[:digit:]+.?[:digit:]*(?=,)") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits),
        " - ",
        str_extract(unique(NDVI_max_class), pattern = "(?<=,)[:digit:]+.?[:digit:]*(?=]|\\))") %>% 
          as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits)
      )
    )
  ) %>%
  glimpse() %>% 
  plot_struct_boxplot_bins(
    x = NDVI_max_class, xlab = expression(paste("NDVI (max) [-]")), group = NDVI_max_class
  )



## NIRv max ----
cor.test(dat$Rao_Q_NIRv, dat$NDVI_max, method = "spearman")

# p_nirv_classes <- dat %>%
#   select(SITE_ID, IGBP,
#          Rao_Q_NIRv, Rao_Q_NDVI,
#          NIRv_max # ==> significant structural predictors of RaoQ in linear models
#   ) %>%
#   drop_na() %>%
#   # mutate(across(where(is.double), min_max_norm)) %>% # normalize
#   arrange(NIRv_max) %>%
#   mutate( # discretize structure into equal-sized classes
#     NIRv_max_class = cut_number(NIRv_max, 4),
#     NIRv_max_class = factor(
#       NIRv_max_class,
#       levels = unique(NIRv_max_class),
#       labels = paste0(
#         str_extract(unique(NIRv_max_class), pattern = "[:digit:]+.?[:digit:]*(?=,)") %>% 
#           as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits),
#         " - ",
#         str_extract(unique(NIRv_max_class), pattern = "(?<=,)[:digit:]+.?[:digit:]*(?=]|\\))") %>% 
#           as.double() %>% round(digits = small_digits) %>% format(nsmall = small_digits)
#       )
#     )
#   ) %>%
#   glimpse() %>% 
#   plot_struct_boxplot_bins(
#     x = NIRv_max_class, xlab = expression(paste("NIRv (max) [-]")), group = NIRv_max_class
#   )
# 

## Combine subplots ----
figure3 <- (p_lai_classes + theme(legend.position = "none") |
              p_crown_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank())
              | p_height_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank())
              # | p_stem_diameter_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank())
) + plot_layout(guides = 'collect') + plot_annotation(tag_levels = "a")
figure3

figure3plus <- (p_lai_classes + theme(legend.position = "none") |
    p_crown_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank(), legend.position = "none")
  | p_height_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank(), legend.position = "none")
  | p_ndvimax_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank(), legend.direction = "horizontal")
  # | p_nirv_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank(), legend.direction = "horizontal")
  + guides(fill = guide_legend(title = "IGBP class:", nrow = 1))
  # | p_stem_diameter_classes + theme(axis.title.y = element_blank(), strip.text.y = element_blank())
) / (
  guide_area()
) +
  plot_layout(guides = 'collect', heights = c(0.9, 0.1)) + plot_annotation(tag_levels = "a")



### Save -----------------------------------------------------------------------
if (savedata) {
  # # All structure plot
  # ggsave(filename = glue::glue("results/scatterplots/RaoQ_all_structure_fit_{vers_out}.jpg"),
  #        plot = p_allstruct, device = "jpeg",
  #        width = 300, height = 400, units = "mm", dpi = 300) # 3:4 ratio
  # # Facets plot
  # ggsave(filename = glue::glue("results/scatterplots/RaoQ_saturation_{vers_out}.jpg"),
  #        plot = p_struct_facets, device = "jpeg",
  #        width = 508, height = 24.19048 * 9, units = "mm", dpi = 300) # 21:9 ratio
  # # Structure classes plot
  # ggsave(filename = glue::glue("results/scatterplots/RaoQ_saturation_{vers_out}.jpg"),
  #        plot = p_struct_facets, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300) # 16:9 ratio
  
  ## Final plot
  ggsave(filename = glue::glue("results/scatterplots/RaoQ_structure_boxplot_categories_{vers_out}.jpg"),
         plot = figure3, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 16:9 ratio
  
  ggsave(filename = glue::glue("results/scatterplots/figure3_RaoQ_structure_boxplot_categories_{vers_out}.jpg"),
         plot = figure3plus, device = "jpeg",
         width = 600, height = 272.73, units = "mm", dpi = 300) # 11:5 ratio
  ggsave(filename = glue::glue("results/scatterplots/figure3_RaoQ_structure_boxplot_categories_{vers_out}.pdf"),
         plot = figure3plus, device = "pdf",
         width = 600, height = 272.73, units = "mm", dpi = 300) # 11:5 ratio
  
}