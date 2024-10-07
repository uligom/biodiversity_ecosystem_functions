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
  # vers_in <- "main_nirv_raoq+nirvmax+ndvimax_v11.06.06"
  vers_out <- paste0(vers_in, "_rev2") # updated to second ERE review
  
  
  
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
  ## color labels
  accessible_palette <- setNames(Three_colorblind2, c("Biodiversity", "Climate", "Structure"))
  
  # plot specifications
  line_width <- line_width_thick
  point_size <- point_size_medium
  
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
        ),
      rel_cat = case_when( # add relative importance categories
        rel_importance >= 0.0 & rel_importance < 0.1 ~ 0.00,
        rel_importance >= 0.1 & rel_importance < 0.2 ~ 0.25,
        rel_importance >= 0.2 & rel_importance < 0.3 ~ 0.50,
        rel_importance >= 0.3 & rel_importance < 0.4 ~ 0.75,
        rel_importance >= 0.4 & rel_importance < 1.0 ~ 1.00
      ) %>% factor(
        levels = c(0.00, 0.25, 0.50, 0.75, 1.00),
        labels = c("  0% -  10%", "10% -  20%", "20% -  30%", "30% -  40%", "40% - 100%")
        )
      ) %>%
    arrange(prediction, var_type, variable) %>% 
    glimpse()
  
  # x limits
  x_lim <- max(abs(dat %>% mutate(val_std = if_else(estimate > 0, true = estimate + std_error, false = estimate - std_error)) %>% pull(val_std)), na.rm = T) * 1.1 # identify max coefficient across analyses and round, with a buffer
  
  
  
  ## Plot model coefficients ----
  p_effects <- list() # initialize list of plots
  for (pp in 1:length(unique(dat$prediction))) {
    # data
    dat_pp <- dat %>% dplyr::filter(prediction == unique(dat$prediction)[pp])
    
    # caption (pp)
    if (scale_vers == "main") {
      labelcap <- bquote(R^2 ~ "=" ~ .(sprintf("%.1f", signif(unique(dat_pp$R2), 3) * 100)) ~ "%" ~
                           "  RMSE =" ~ .(sprintf("%.1f", signif(unique(dat_pp$RMSE), 2))) ~
                           "  n =" ~ .(unique(dat_pp$n))
      )
    } else {
      labelcap <- bquote(R^2 ~ "=" ~ .(sprintf("%.1f", signif(unique(dat_pp$R2), 3) * 100)) ~ "%" ~
                           "  n =" ~ .(unique(dat_pp$n))
      )
    }
    
    # # previous to ERE review 2:
    # p_effects[[pp]] <- dat_pp %>% 
    #   ggplot(aes(x = estimate, y = variable, alpha = rel_importance, color = var_type)) +
    #   geom_vline(xintercept = 0, color = text_color_background) + # 0 line
    #   geom_pointrange(aes(xmin = estimate - std_error, xmax = estimate + std_error, alpha = rel_importance),
    #                   size = point_size_medium, shape = 19, linewidth = line_width_thick, fatten = 1, na.rm = T) +
    #   # geom_errorbarh(aes(xmin = estimate - std_error, xmax = estimate + std_error, alpha = rel_importance),
    #   #                height = 0, linewidth = line_width_thick, na.rm = T) +
    #   # geom_point(size = point_size_big, shape = 19, na.rm = T, show.legend = T) +
    #   scale_alpha_continuous(breaks = c(0.25, 0.50, 0.75), labels = c("25 %", "50 %", "75 %"),
    #                          limits = c(0.24, 0.76), range = c(0.33, 1), oob = squish) + #, labels = c("less important", "more important")
    #   scale_color_manual(values = Three_colorblind, na.translate = F) + # custom colors
    #   xlim(c(-0.25, 0.25)) +
    #   labs(title = y_labels[pp],
    #        caption = paste0("n = ", n_obs[pp])) +
    #   guides(alpha = guide_legend(title = "Relative importance", override.aes = list(size = 1.5)),
    #          color = guide_legend(title = "Predictor type", override.aes = list(size = 1.5))) + # legend titles
    #   theme_bw() +
    #   theme_combine +
    #   theme(axis.title = element_blank()) +
    #   NULL
    
    # updated plot
    p_effects[[pp]] <- dat_pp %>% 
      ggplot(aes(x = estimate, y = variable)) +
      geom_vline(xintercept = 0, color = text_color_background) + # 0 line
      geom_errorbarh( # draw errorbars without transparency
        aes(color = var_type, xmin = estimate - std_error, xmax = estimate + std_error),
        alpha = 1, height = 0, linewidth = line_width, na.rm = T, show.legend = T
      ) +
      geom_point( # add white fill, full-color stroke points to cover errorbars in the central region
        aes(color = var_type),
        alpha = 1, fill = "white",
        stroke = line_width, size = point_size, shape = 21, na.rm = T, show.legend = T
      ) +
      geom_point( # draw round filled shape with transparency
        aes(alpha = rel_cat, color = var_type),
        size = point_size, shape = 19, na.rm = T, show.legend = T
      ) +
      geom_text( # add significance asterisks
        aes(color = var_type, label = ifelse(p_val < 0.05, "*", NA)),
        nudge_x = x_lim * 0.1, nudge_y = 0.1,
        size = point_size * 1.5, na.rm = T, show.legend = F
      ) +
      scale_alpha_discrete(
        range = c(0, 1), # display transparency range
        # limits = c(0.04, 0.56), oob = squish # replaces out of bounds values with the nearest limit
        drop = F
      ) +
      scale_discrete_manual( # custom colors
        aesthetics = "color",
        values = accessible_palette,
        na.translate = F
      ) +
      scale_x_continuous(
        breaks = waiver(),
        limits = c(-x_lim, x_lim), # define same x axis limits for all subplots
        n.breaks = 5 # "algorithm may choose a slightly different number to ensure nice break labels" <-- do what you want then #!*@$!!
      ) +
      labs(
        title = y_labels[pp],
        caption = labelcap
      ) +
      guides(
        alpha = guide_legend(title = "Relative importance", override.aes = list(size = point_size)),
        color = guide_legend(title = "Predictor type", override.aes = list(size = point_size))
      ) + # legend titles
      theme_bw() +
      theme_combine +
      theme(
        axis.title = element_blank(), # remove axes titles
        legend.background = element_rect(fill = "transparent"),
        plot.caption = element_text(color = text_color_background), # color of caption label
        plot.margin = unit(c(0, 10, 0, 0), "mm")
      ) +
      NULL
    
    if (pp != length(unique(dat$prediction))) {
      p_effects[[pp]] <- p_effects[[pp]] + theme(legend.position = "none")
    }
    if (pp <= length(unique(dat$prediction)) / 2) {
      p_effects[[pp]] <- p_effects[[pp]] + theme(plot.margin = unit(c(0, 10, 5, 0), "mm")) # add spacing between first and second row when combining subplots
    }
  }
  
  
  ## Combine ----
  p_effects <- wrap_plots(p_effects, nrow = 2) +
    plot_layout(guides = 'collect') #+ plot_annotation(tag_levels = "a")
  
  
  
  ### Save ---------------------------------------------------------------------
  if (savedata) {
    scal <- 1
    width <- 508
    height <- 285.75 #width * 6.66 / 31.02
    
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