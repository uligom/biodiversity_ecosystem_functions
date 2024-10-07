#### TEST RELATIONSHIPS BETWEEN EFPS AND BIODIVERSITY

### Authors: Ulisse Gomarasca
### Version History
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
raoq_in <- "nirv" # one of: "bands", "ndvi", or "nirv"
dat_in <- "v06.06"
vers_in <- paste0(raoq_in, "_v11.", stringr::str_extract(dat_in, "[:digit:]+[:punct:]?[:digit:]+"))
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {
  vers_out <- paste0(vers_in, "_rev2")
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_multivariate_analysis_{vers_out}.txt")
  txt <- "Initializing analysis..."; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
}



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plots
library(RColorBrewer) # plot color functionalities
library(readr)        # read table format files
library(tidyr)        # clean and reshape tidy data

## Other
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
dat_no <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_no_{vers_in}.csv"), show_col_types = F)
dat_ground <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_ground_{vers_in}.csv"), show_col_types = F)
dat_sat <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_satellite_{vers_in}.csv"), show_col_types = F)
dat_all <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_all_{vers_in}.csv"), show_col_types = F)
dat_main <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_main_{vers_in}.csv"), show_col_types = F)
dat_no_main <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_no_main_{vers_in}.csv"), show_col_types = F)


## Vector of names of biodiversity subsets
biodiv_scales <- c("No biodiversity", "Ground biodiversity", "RS biodiversity",
                   "All predictors", "A-posteriori predictors"#, "A-posteriori, no biodiversity"
                   )



### Process data ---------------------------------------------------------------
## Merge all datasets ----
dat <- rbind(
  dat_no %>% mutate(biodiv_subset = biodiv_scales[1]),
  dat_ground %>% mutate(biodiv_subset = biodiv_scales[2]),
  dat_sat %>% mutate(biodiv_subset = biodiv_scales[3]),
  dat_all %>% mutate(biodiv_subset = biodiv_scales[4]),
  dat_main %>% mutate(biodiv_subset = biodiv_scales[5]) %>% dplyr::select(-RMSE, -RMSE_loo)
  # ,dat_no_main %>% mutate(biodiv_subset = biodiv_scales[6])
  ) %>%
  glimpse()

## Calculate deltas ----
dat <- dat %>% 
  left_join(dat %>% dplyr::filter(biodiv_subset == "No biodiversity") %>% dplyr::select(R2, AICc, prediction) %>% unique(),
            by = "prediction", suffix = c("", "_none")) %>% 
  mutate(deltaR2 = R2 - R2_none,
         deltaAICc = AICc - AICc_none) %>% 
  glimpse()


## Arrange into longer format ----
dat_plot <- dat %>% 
  dplyr::select(R2, deltaR2, AICc, deltaAICc, prediction, biodiv_subset) %>% 
  unique() %>% 
  pivot_longer(cols = c(R2, deltaR2, AICc, deltaAICc), names_to = "item", values_to = "value") %>% 
  glimpse()



### Filter data ----------------------------------------------------------------
dat_plot <- dat_plot %>%
  # dplyr::filter(prediction != "WUEt") %>%
  # dplyr::filter(biodiv_subset != "none") %>%
  glimpse()



### Plot settings --------------------------------------------------------------
## Add levels for legend
dat_plot <- dat_plot %>% 
  mutate(biodiv_subset = factor(biodiv_subset, levels = biodiv_scales),
         item = factor(item, levels = c("R2", "deltaR2", "AICc", "deltaAICc"),
                       labels = c("R^{2}", "delta~R^{2}", "AICc", "delta~AICc"
                                  )
                       ),
         prediction = factor(prediction, levels = c("CUE_eco_90", "GPP_sat", "Gs_max", "NEP_95",
                                                    "WUE", "WUEt" #, "WUE_NEE", "uWUE", "IWUE"
                                                    )
                             )
         )


### Plot R2 and AIC with/without biodiversity ----------------------------------
if (length(biodiv_scales) == 5) {color_palette <- Five_colorblind
} else if (length(biodiv_scales) == 6) {color_palette <- Six_colorblind}
p0 <- dat_plot %>% 
  ggplot(aes(x = prediction, y = value, fill = biodiv_subset)) +
  geom_col(position = position_dodge2(padding = 0)) +
  facet_wrap(. ~ item, scales = "free_y",
             labeller = label_parsed) + # subplot for every predicted EFP
  scale_x_discrete(labels = c(expression(CUE[eco]), expression(GPP[sat]), expression(Gs[max]), expression(NEP[max]),
                              expression(WUE), expression(WUE[t])#, expression(WUE[NEE]), expression(uWUE), expression(IWUE)
                              )
                   ) +
  scale_fill_manual(values = color_palette, na.translate = F) + # custom colors
  xlab("") + ylab("") +
  guides(fill = guide_legend(title = "Biodiversity", keywidth = 3, keyheight = 1)) + # legend titles
  theme_classic() +
  theme_less_facets +
  theme_transparent +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_line(linewidth = 0.5)
        )
p0
  


### Save -----------------------------------------------------------------------
if (savedata) {
  scal <- 1
  # R2 with ground or satellite biodiversity
  ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/model_compare_biodiv_subsets_{vers_out}.jpg"),
                  plot = p0, device = "jpeg",
                  width = 508, height = 285.75, units = "mm", dpi = 300 * scal)
  
  # # Transparent png
  # ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/model_compare_biodiv_subsets_{vers_out}.png"),
  #                 plot = p0, device = "png", bg = "transparent",
  #                 width = 508, height = 285.75, units = "mm", dpi = 300 * scal)
}


### End ------------------------------------------------------------------------