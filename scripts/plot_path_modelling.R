##### PLOT OUTPUT OF PATH MODELLING (STRUCTURE EQUATION MODELLING +)
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Saving settings
dat_in <- "v06.06"
vers_out <- dat_in # saving option

savedata <- F #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
# library(broom)
# library(mlr)
# library(semPlot)
library(lavaan)
# library(randomForest)
library(lavaanPlot)
library(tidyverse)
# library(gridExtra)
# library(DiagrammeR)
# library(DiagrammeRsvg)
# library(rsvg)

# ## Functions/Other
# source("scripts/functions/min_max_norm.R")
# source("scripts/functions/runPathModel.R")
# source("scripts/functions/SEM_models.R")
# source("scripts/functions/save_png.R")



### Data -----------------------------------------------------------------------
model_files <- list.files(path = "results/SEM/", pattern = "_SEM_out")



### Labels ---------------------------------------------------------------------
Labels <- list(
  c("CUE_eco_90" = "CUEeco", "GPP_sat" = "GPPsat", "Gs_max" = "Gs max", "NEP_95" = "NEPmax", "WUE" = "WUE",
    "SW_in" = "SWin", "VPD" = "VPD", "temp" = "Temperature", "precip" = "Precipitation",
    "NDVI_max" = "NDVImax", "height_max" = "Maximum height",
    "bray_curtis" = "Bray-Curtis", "Rao_Q_NIRv" = "RaoQ (NIRv)"
  ),
  c("CUE_eco_90" = "CUEeco",
    "SW_in" = "SWin",
    "VPD" = "VPD",
    "bray_curtis" = "Bray-Curtis"
    ),
  c("GPP_sat" = "GPPsat",
    "precip" = "Precipitation",
    "temp" = "Temperature",
    "NDVI_max" = "NDVImax",
    "height_max" = "Height max",
    "Rao_Q_NIRv" = "RaoQ (NIRv)"
  ),
  c("Gs_max" = "Gs max",
    "precip" = "Precipitation",
    "height_max" = "Maximum height",
    "NDVI_max" = "NDVImax",
    "Rao_Q_NIRv" = "RaoQ (NIRv)",
    "bray_curtis" = "Bray-Curtis"
  ),
  c("NEP_95" = "NEPmax",
    "precip" = "Precipitation",
    "temp" = "Temperature",
    "NDVI_max" = "NDVImax",
    "Rao_Q_NIRv" = "RaoQ (NIRv)"
  ),
  c("WUE" = "WUE",
    "SW_in" = "SWin",
    "temp" = "Temperature",
    "NDVI_max" = "NDVImax",
    "height_max" = "Maximum height",
    "Rao_Q_NIRv" = "RaoQ (NIRv)",
    "bray_curtis" = "Bray-Curtis"
  ),
  c("WUEt" = "WUEt",
    "SW_in" = "SWin",
    "VPD" = "VPD",
    "NDVI_max" = "NDVImax",
    "height_max" = "Maximum height",
    "Rao_Q_NIRv" = "RaoQ (NIRv)",
    "shannon" = "Shannon index"
  )
)



### Plot -----------------------------------------------------------------------
# p <- list()
# c("#00A0A0", "#A00000")


for (ii in 1:length(model_files)) {
  load(file = glue::glue("results/SEM/{model_files[ii]}")) # 'semMod1'
  
  summary(semMod1)
  
  ## Plot ------
  print(lavaanPlot(name = "", #stringr::str_extract(model_files[ii], "[:graph:]+"),
             model = semMod1, labels = Labels[[ii]], coefs = T, covs = T,
             stars = c("regress", "covs")
             # , edge_options = list(e_opts, c_opts)
             )
  )
  
}