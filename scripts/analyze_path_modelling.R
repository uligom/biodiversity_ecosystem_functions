##### SCRIPT FOR PATH MODELLING (STRUCTURE EQUATION MODELLING +)
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Saving settings
dat_in <- "v06.06"
vers_out <- dat_in # saving option

savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
library(broom)
library(mlr)
library(semPlot)
library(lavaan)
library(randomForest)
library(lavaanPlot)
library(tidyverse)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

## Functions/Other
source("scripts/functions/min_max_norm.R")
source("scripts/functions/runPathModel.R")
source("scripts/functions/SEM_models.R")
source("scripts/functions/save_png.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("data/inter/data_efps_clim_struct_biodiv_{dat_in}.csv"), show_col_types = F) %>% 
  glimpse()



### Process data ---------------------------------------------------------------
## Normalize ----
dat_norm <- dat %>% 
  dplyr::select(-SITE_ID) %>% 
  mutate(across(.cols = where(is.double), .fns = min_max_norm)) %>%
  glimpse()



### Define model ---------------------------------------------------------------
## Model:
# EFP <-- climate + structure + biodiversity
## Direct effects:
# structure <-- climate
# biodiversity <-- climate
# sat biodiversity <--> structure?
# sat biodiversity <--> ground biodiversity?

var_list <- list() # initialize
model_architecture <- list() # initialize

## GPPsat Model ----
var_list[[1]] <- c("GPP_sat", "precip", "temp", "NDVI_max", "height_max", "Rao_Q_NIRv") # variables for model

model_architecture[[1]] <- ' # measurement model
  GPP_sat ~ precip + temp + NDVI_max + height_max + Rao_Q_NIRv

  # 2. regressions (direct effect)
  Rao_Q_NIRv ~ precip + temp
  NDVI_max ~ precip + temp
  height_max ~ precip + temp

  # 3. (co)variances
  Rao_Q_NIRv ~~ NDVI_max + height_max
'

## Gsmax Model ----
var_list[[2]] <- c("Gs_max", "precip", "height_max", "NDVI_max", "Rao_Q_NIRv", "bray_curtis") # variables for model

model_architecture[[2]] <- ' # measurement model
  Gs_max ~ precip + height_max + NDVI_max + Rao_Q_NIRv + bray_curtis

  # 2. regressions (direct effect)
  height_max ~ precip
  NDVI_max ~ precip
  Rao_Q_NIRv ~ precip
  bray_curtis ~ precip

  # 3. (co)variances
  Rao_Q_NIRv ~~ height_max + NDVI_max
  bray_curtis ~~ height_max + NDVI_max
'

## CUEeco Model ----
var_list[[3]] <- c("CUE_eco_90", "SW_in", "VPD", "bray_curtis") # variables for model

model_architecture[[3]] <- ' # measurement model
  CUE_eco_90 ~ SW_in + VPD + bray_curtis

  # 2. regressions (direct effect)
  bray_curtis ~ SW_in + VPD

  # 3. (co)variances
'

## NEPmax Model ----
var_list[[4]] <- c("NEP_95", "precip", "temp", "NDVI_max", "Rao_Q_NIRv") # variables for model

model_architecture[[4]] <- ' # measurement model
  NEP_95 ~ precip + temp + NDVI_max + Rao_Q_NIRv

  # 2. regressions (direct effect)
  NDVI_max ~ precip + temp
  Rao_Q_NIRv ~ precip + temp

  # 3. (co)variances
  Rao_Q_NIRv ~~ NDVI_max
'

## WUE Model ----
var_list[[5]] <- c("WUE", "temp", "SW_in", "height_max", "NDVI_max", "Rao_Q_NIRv", "bray_curtis") # variables for model

model_architecture[[5]] <- ' # measurement model
  WUE ~ temp + SW_in + height_max + NDVI_max + Rao_Q_NIRv + bray_curtis

  # 2. regressions (direct effect)
  bray_curtis ~ temp + SW_in
  Rao_Q_NIRv ~ temp + SW_in
  height_max ~ temp + SW_in
  NDVI_max ~ temp + SW_in

  # 3. (co)variances
  bray_curtis ~~ height_max + NDVI_max
  Rao_Q_NIRv ~~ height_max + NDVI_max
'


## WUEt Model ----
# Test whether accounting for evaporation increases the importance of biodiversity/structural predictors
var_list[[6]] <- c("WUEt", "SW_in", "VPD", "height_max", "NDVI_max", "shannon", "Rao_Q_NIRv") # variables for model

model_architecture[[6]] <- ' # measurement model
  WUEt ~ height_max + NDVI_max + SW_in + VPD + shannon + Rao_Q_NIRv

  # 2. regressions (direct effect)
  shannon ~ SW_in + VPD
  height_max ~ SW_in + VPD
  NDVI_max ~ SW_in + VPD
  Rao_Q_NIRv ~ SW_in + VPD

  # 3. (co)variances
  shannon ~~ height_max + NDVI_max
  Rao_Q_NIRv ~~ height_max + NDVI_max
'


# ## All-in-one Model ----
# var_list[[7]] <- c("CUE_eco_90", "GPP_sat", "Gs_max", "NEP_95", "WUE", # EFPs
#                    "SW_in", "temp", "VPD", "precip", # climate
#                    "height_max", "NDVI_max", # structure
#                    "bray_curtis", # ground biodiversity
#                    "Rao_Q_NIRv" # satellite biodiversity
#                    ) # variables for model
# 
# model_architecture[[7]] <- ' # measurement model
#   ## EFP models
#   CUE_eco_90 ~ SW_in + VPD + bray_curtis
#   GPP_sat ~ precip + temp + NDVI_max + height_max + Rao_Q_NIRv
#   Gs_max ~ precip + height_max + Rao_Q_NIRv + bray_curtis
#   NEP_95 ~ precip + temp + NDVI_max + Rao_Q_NIRv
#   WUE ~ temp + SW_in + NDVI_max + height_max + Rao_Q_NIRv + bray_curtis
# 
#   # 2. regressions (direct effect)
#   ## structure <-- climate
#   height_max ~ SW_in + temp + VPD + precip
#   NDVI_max ~ SW_in + temp + VPD + precip
# 
#   ## biodiversity <-- climate
#   bray_curtis ~ SW_in + temp + VPD + precip
#   Rao_Q_NIRv ~ SW_in + temp + VPD + precip
# 
#   # 3. (co)variances
#   ## sat biodiversity <--> structure?
#   bray_curtis ~~ height_max + NDVI_max
#   Rao_Q_NIRv ~~ height_max + NDVI_max
# 
#   ## sat biodiversity <--> ground biodiversity?
#   Rao_Q_NIRv ~~ bray_curtis
# '
# 

### Run Path modelling ---------------------------------------------------------
SEMmodel_out <- list() # initialize list of outputs
pSEMplot <- list() # initialize list of plots
for (ss in 1:length(model_architecture)) { # loop over each EFP
  ## Subset variables ----
  # Choose variables to include based on main analysis of multimodel inference
  dat_sub <- dat_norm %>% dplyr::select(all_of(var_list[[ss]]))
  
  
  ## Remove NAs for input ----
  x_sem <- na.omit(dat_sub) # NA are not accepted by sem
  # x_sem_in <- na.omit(x_sem)
  
  
  ## Run SEM ----
  SEMmodel_out[[ss]] <- runPathModel(x_sem = na.omit(x_sem), model_struct = model_architecture[[ss]], savedata = savedata)
  
  ## Table of evaluation ----
  # table of model fit indexes, e.g.:
  #   AIC = summary_SEM[["fit"]][["aic"]], # Akaike Information Criterion
  #   Chi2 = summary_SEM[["fit"]][["chisq"]], # Chi-square "the “original” measure of fit. Most other absolute fit indices are simple transformations of the chi-squared" (https://www.statisticshowto.com/index-of-fit/)
  #   CFI = summary_SEM[["fit"]][["cfi"]], # Comparative Fit Index "compares the performance of your proposed model with the performance of a null or baseline model in which there was no correlation between observed variables" (https://www.statisticshowto.com/index-of-fit/)
  #   TLI = summary_SEM[["fit"]][["tli"]], # Tucker-Lewis Index "also called the non-normed fit index, penalizes for adding model parameters" (https://www.statisticshowto.com/index-of-fit/)
  #   NFI = (summary_SEM[["fit"]][["baseline.chisq"]] - summary_SEM[["fit"]][["chisq"]]) / summary_SEM[["fit"]][["baseline.chisq"]], # Normed Fit Index "Ranges from 0 to 1, where 1 is a perfect fit. The NFI is difference between the null (or baseline) model’s chi-square and the target model’s chi-square, divided by the null (or baseline) model’s chi-square." (https://www.statisticshowto.com/index-of-fit/)
  #   RMSEA = summary_SEM[["fit"]][["rmsea"]], # Root Mean Square Error of Approximation
  summary_SEM <- summary(SEMmodel_out[[ss]], fit.measures = T)
  
  if (ss == 1) { # initialize column of index names
    sem_eval <- tibble(`Fit Index` = names(summary_SEM[["fit"]])) %>% 
      add_row(`Fit Index` = "nfi")
  }
  y_name <- paste0(var_list[[ss]][1], " model") %>% rlang::sym() # model name
  
  sem_eval <- bind_cols(
    sem_eval,
    tibble(as.data.frame(summary_SEM$fit)) %>% 
      dplyr::rename({{y_name}} := `summary_SEM$fit`) %>% 
      dplyr::mutate({{y_name}} := as.double(!!y_name)) %>% 
      add_row({{y_name}} := (summary_SEM[["fit"]][["baseline.chisq"]] - summary_SEM[["fit"]][["chisq"]]) / summary_SEM[["fit"]][["baseline.chisq"]])
  )
  
  
  # ### Plot ---------------------------------------------------------------------
  # pSEMplot[[ss]] <- lavaanPlot(model = SEMmodel_out[[ss]] , coefs = T, covs = T, stand = T, stars = c("regress"))
  # print(pSEMplot[[ss]])
  # # fitmeasures(pSEMplot[[ss]])
} # end loop for SEM on EFPs


### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(sem_eval, "results/SEM/TableS4_SEM_indexes.csv")
}



### End ------------------------------------------------------------------------