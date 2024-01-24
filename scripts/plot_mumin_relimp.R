#### PLOT COEFFICINETS OF MULTIMODEL INFERENCE

### Authors: Ulisse Gomarasca
### Version History
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

scale_vers <- "all" # what biodiversity subset? ("no"/"ground"/"satellite"/"all"/"main")

dat_in <- "v06.06"
vers <- paste0("v11.", stringr::str_extract(dat_in, "[:digit:]+[:punct:]?[:digit:]+"))



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plots
library(patchwork)    # combine plots
library(RColorBrewer) # plot color functionalities
library(readr)        # read table format files
library(scales)       # modify scales of axes
library(stringr)      # string manipulation
library(tidyr)        # clean and reshape tidy data

## Other
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### More settings --------------------------------------------------------------
## Select RaoQ input
for (bbb in 1:3) {
  if (bbb == 1) {raoq_in <- "bands"} else if (bbb == 2) {raoq_in <- "ndvi"} else if (bbb == 3) {raoq_in <- "nirv"}
  # "bands" = raoQ from all bands
  # "ndvi" = raoQ from NDVI
  # "nirv" = raoQ from NIRv
  
  vers_in <- glue::glue("{scale_vers}_{raoq_in}_{vers}")
  vers_out <- paste0(vers_in)
  
  
  
  ### Data ---------------------------------------------------------------------
  dat <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_in}.csv"), show_col_types = F)
  
  # if (scale_vers == "main") {
  #   dat_crossval <- read_csv(glue::glue("results/multimodel_inference/prediction_cross_validation_{vers_in}.csv"), show_col_types = F)
  # }
  
  
  
  ### Vector names -------------------------------------------------------------
  load(file = glue::glue("data/inter/efps_names_{dat_in}.RData"));           efps_names <- efps_names[efps_names %in% dat$prediction]
  load(file = glue::glue("data/inter/clim_names_{dat_in}.RData"));           clim_names <- clim_names[clim_names %in% dat$variable]
  load(file = glue::glue("data/inter/struct_names_{dat_in}.RData"));         struct_names <- struct_names[struct_names %in% dat$variable]
  load(file = glue::glue("data/inter/ground_biodiv_names_{dat_in}.RData"));  ground_biodiv_names <- ground_biodiv_names[ground_biodiv_names %in% dat$variable]
  load(file = glue::glue("data/inter/sat_biodiv_names_{dat_in}.RData"));     sat_biodiv_names <- sat_biodiv_names[sat_biodiv_names %in% dat$variable]
  load(file = glue::glue("data/inter/biodiv_names_{dat_in}.RData"));         biodiv_names <- biodiv_names[biodiv_names %in% dat$variable]
  
  
  
  
  ## Omit NAs ----
  dat <- dat %>% drop_na()
  
  
  ## Number of observations ----
  # Order of n_obs same as EFPs
  n_obs <- dat %>%
    drop_na() %>%
    select(prediction, n) %>% 
    unique() %>% 
    pull(n)
  
  
  
  ### Plot ---------------------------------------------------------------------
  ## Labels and options ----
  ## Add color labels
  Three_colorblind <- setNames(Three_colorblind2, c("Biodiversity", "Climate", "Structure"))
  
  ## Add predictor labels
  dat <- dat %>% 
    mutate(var_type = case_when(
      variable %in% clim_names ~ "Climate",
      variable %in% struct_names ~ "Structure",
      # variable %in% ground_biodiversity ~ "Ground Biodiversity",
      # variable %in% satellite_diversity ~ "Satellite Biodiversity"
      T ~ "Biodiversity"
      )
    )
  
  ## Add levels for y axis order
  # variable as factor with levels to define order on y axis
  var_names <- dat %>% dplyr::arrange(desc(var_type), desc(variable)) %>% pull(variable) %>% unique()
  y_names <- dat %>% arrange(prediction) %>% pull(prediction) %>% unique()
  if(length(y_names) == 5) {
    y_labels <- c(expression("CUE"[eco]), expression("GPP"[sat]), expression("Gs"[max]), expression("NEP"[max]), expression("WUE"))#, " [(n == ", n_obs, ")]")
  } else if (length(y_names) == 6) {
    y_labels <- c(expression("CUE"[eco]), expression("GPP"[sat]), expression("Gs"[max]), expression("NEP"[max]), expression("WUE"), expression("WUE"[t]))
  }
  
  nice_names <- var_names %>% str_replace_all("_", " ")
  
  dat <- dat %>% 
    mutate(
      variable = factor(
        variable,
        levels = var_names,
        labels = nice_names
        ),
      prediction = factor(
        prediction,
        levels = y_names,
        labels = y_labels
        ),
      var_type = factor(
        var_type,
        levels = c("Biodiversity", "Climate", "Structure")
        )
      ) %>%
    arrange(prediction, var_type, variable) %>% 
    glimpse()
  
  
  ## Plot model coefficients ----
  p_effects <- list() # initialize list of plots
  for (pp in 1:length(unique(dat$prediction))) {
    dat_pp <- dat %>% dplyr::filter(prediction == unique(dat$prediction)[pp])
    p_effects[[pp]] <- dat_pp %>% 
      ggplot(aes(x = estimate, y = variable, alpha = rel_importance, color = var_type)) +
      geom_vline(xintercept = 0, color = text_color_background) + # 0 line
      geom_pointrange(aes(xmin = estimate - std_error, xmax = estimate + std_error, alpha = rel_importance),
                      size = point_size_medium, shape = 19, linewidth = line_width_thick, fatten = 1, na.rm = T) +
      # geom_errorbarh(aes(xmin = estimate - std_error, xmax = estimate + std_error, alpha = rel_importance),
      #                height = 0, linewidth = line_width_thick, na.rm = T) +
      # geom_point(size = point_size_big, shape = 19, na.rm = T, show.legend = T) +
      scale_alpha_continuous(breaks = c(0.25, 0.50, 0.75), labels = c("25 %", "50 %", "75 %"),
                             limits = c(0.24, 0.76), range = c(0.33, 1), oob = squish) + #, labels = c("less important", "more important")
      scale_color_manual(values = Three_colorblind, na.translate = F) + # custom colors
      xlim(c(-0.25, 0.25)) +
      labs(title = y_labels[pp],
           caption = paste0("n = ", n_obs[pp])) +
      guides(alpha = guide_legend(title = "Relative importance", override.aes = list(size = 1.5)),
             color = guide_legend(title = "Predictor type", override.aes = list(size = 1.5))) + # legend titles
      theme_bw() +
      theme_combine +
      theme(axis.title = element_blank()) +
      NULL
    if (pp != length(unique(dat$prediction))) {
      p_effects[[pp]] <- p_effects[[pp]] + theme(legend.position = "none")
    }
  }
  
  
  ## Combine ----
  p_effects <- wrap_plots(p_effects, nrow = 2) +
    plot_layout(guides = 'collect') # + plot_annotation(tag_levels = "a")
  
  
  
  ### Save ---------------------------------------------------------------------
  if (savedata) {
    scal <- 1
    width <- 508
    height <- 285.75 #width * 6.66 / 31.02
    # height = 508
    # width = height * 2 * 6.66 / 31.02
    
    # multimodel effects plot
    ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/mumin_coefficients_{vers_out}.jpg"), plot = p_effects, device = "jpeg",
                    width = width, height = height, units = "mm", dpi = 300 * scal)
    
    # # transparent png
    # ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/mumin_coefficients_{vers_out}.png"), plot = p_effects, device = "png",
    #                 bg = "transparent", width = width, height = height, units = "mm", dpi = 300 * scal)
    
    # if (scale_vers == "main") {
    #   # cross validation effects plot
    #   ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/crossval_coefficients_{vers_out}.jpg"), plot = p_crossval, device = "jpeg",
    #                   width = width, height = height, units = "mm", dpi = 300 * scal)
    # }
  }
}



### End ------------------------------------------------------------------------