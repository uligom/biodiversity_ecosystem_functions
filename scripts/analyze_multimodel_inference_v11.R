#### TEST RELATIONSHIPS BETWEEN EFPS AND BIODIVERSITY

### Authors: Ulisse Gomarasca
### Version History ------------------------------------------------------------
# v01, 03.02.2023:  Trying multivariate analysis.
# v02, 15.02.2023:  Added PCA, multivariate analysis based on reduced set of variables from PCA
#                   Solved multimodel inference with single input variables per group (biodiversity ground, raoq...)
# v03, 23.02.2023:  Added mean climate variables
# v04, 21.03.2023:  Added mean structural properties at last measurement date
# v05, 22.03.2023:  Biodiversity indexes and structure calculated over all dates
# v06, 30.03.2023:  Only main BD predictors used; plot prediction (effect and R2);
#                   subdivided into conditions to select biodiversity predictors
# v07, 15.05.2023:  Added WUE EFPs and soil water content.
# v08, 27.06.2023:  Reworked pipeline; added cross-validation on a-posteriori analysis;
#                   saved input data as output.
# v09, 19.07.2023:  Corrected aggregation of NEON vegetation structure; filtered
#                   species at NEON tower plots
# v10, 01.08.2023:  Added pre-selection of predictors; corrected (again) aggregation
#                   of canopy height at NEON sites.
# v10.1, 21.08.23:  RaoQ based on all S2 bands at peak of NDVI instead of based on NDVI.
# v10.2, 21.08.23:  RaoQ based on all NIRv at peak of NDVI.
# v11, 18.09.23:    New RaoQ estimates based on single scenes instead of compounded images.
#                   Included LAI.
# v11.04-06, 18.09.23: Updated calculation of ground biodiversity indexes.
# v11.05.06, 27.11.23: Corrected flux growing season filter.



### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

## Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
dat_in <- "v06.06"
vers <- paste0("v11.", stringr::str_extract(dat_in, "[:digit:]+[:punct:]?[:digit:]+"))



### Utilities ------------------------------------------------------------------
## Packages
library(car)          # variable inflation factor (vif)
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plots
library(modEvA)       # pseudo-R2 for glm
library(MuMIn)        # dredge multimodel inference
library(RColorBrewer) # plot color functionalities
library(readr)        # read table format files
library(relaimpo)     # relative importance analysis
library(rlang)        # quoting
library(rstatix)      # levene's normality test
library(stringr)      # string manipulation
library(tictoc)       # measure time
library(tidyr)        # clean and reshape tidy data
library(tidyselect)   # tidyverse helpers

## Functions
source("scripts/functions/do_crossval_relaimpo.R")
source("scripts/functions/do_multimodel_relaimpo.R")
source("scripts/functions/min_max_norm.R")
source("scripts/functions/plot_scatterplot_models.R")

## Other
source("scripts/themes/MyThemes.R")



### More settings --------------------------------------------------------------
## Select RaoQ input
for (bbb in 1:3) {
  out_bb <- "main"
  # "no" = exclude all biodiversity variables
  # "sub" = include biodiversity but divide into ground and satellite
  # "all" = include all biodiversity predictors
  # "main" = a-posteriori analysis on significant predictors with cross validation
  # "no_main" = a-posteriori analysis on significant predictors without biodiversity
  
  if (bbb == 1) {raoq_in <- "bands"} else if (bbb == 2) {raoq_in <- "ndvi"} else if (bbb == 3) {raoq_in <- "nirv"}
  # "bands" = raoQ from all bands
  # "ndvi" = raoQ from NDVI
  # "nirv" = raoQ from NIRv
  
  vers_out <- glue::glue("{out_bb}_{raoq_in}_{vers}")
  vers_all <- glue::glue("all_{raoq_in}_{vers}") # last version for which analysis with all variables was performed (should be same as vers_out when fully updated)
  
  if (savedata) {
    eval_file <- glue::glue("results/analysis_evaluation/evaluation_multivariate_analysis_{vers_out}.txt")
    txt <- "Initializing analysis..."; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
  }
  
  
  
  ### Data -----------------------------------------------------------------------
  dat <- read_csv(glue::glue("data/inter/data_efps_clim_struct_biodiv_{dat_in}.csv"), show_col_types = F) %>% 
    glimpse()
  
  
  
  ### Process data -------------------------------------------------------------
  ## Pre-selection of EFPs and predictors ----
  if (raoq_in == "bands") {raoQ_name <- sym("Rao_Q_S2")
  } else if (raoq_in == "ndvi") {raoQ_name <- sym("Rao_Q_NDVI")
  } else if (raoq_in == "nirv") {raoQ_name <- sym("Rao_Q_NIRv")
  }
  
  dat <- dat %>% 
    dplyr::select(-IGBP) %>% # exclude pft class
    dplyr::select(-CUE_eco_10, -CUE_eco_50, -NEP_99, -IWUE, -WUE_NEE, -uWUE) %>% # exclude unnecessary metric
    dplyr::select(-contains("Rao_Q"), !!raoQ_name) %>% # satellite Rao's Q: only keep one
    dplyr::select(-sat_phen_std) %>% # satellite diversity: exclude phenological diversity
    dplyr::select(-pielou_evenness, # exclude because calculated from Shannon and species richness
                  -simpson_diversity # exclude because very odd distribution and probably wrong formula
    ) %>% # ground biodiversity: exclude similar indexes, exclude simpson because distribution of values is extremely skewed
    dplyr::select(-height, -LAI_max, -contains("ninety"), -max_base_crown_diameter) %>% # only keep a few structural variables
    dplyr::select(-NIRv_max) %>% # exclude because too correlated with RaoQ
    glimpse()
  
  
  # ## Save input ----
  # if (savedata) {
  #   if (out_bb != "sub") {
  #     # Data
  #     write_csv(dat, glue::glue("data/output/data_mumin_input_{vers_out}.csv"))
  #     # # Site list
  #     # write_csv(dat_sites, glue::glue("results/multimodel_inference/site_list_{vers_out}.csv"))
  #   } else if (out_bb == "sub") { # delay saving for sub option (ground/satellite)
  #     # Data
  #     dat_out <- dat
  #   }
  # }
  
  
  ## Normalize data between 0 and 1 ----
  dat_norm <- dat %>% 
    mutate(across(.cols = where(is.double), .fns = min_max_norm)) %>%
    glimpse()
  
  
  
  ### Vector names ---------------------------------------------------------------
  load(file = glue::glue("data/inter/efps_names_{dat_in}.RData"));           efps_names <- efps_names[efps_names %in% names(dat)]
  load(file = glue::glue("data/inter/clim_names_{dat_in}.RData"));           clim_names <- clim_names[clim_names %in% names(dat)]
  load(file = glue::glue("data/inter/struct_names_{dat_in}.RData"));         struct_names <- struct_names[struct_names %in% names(dat)]
  load(file = glue::glue("data/inter/ground_biodiv_names_{dat_in}.RData"));  ground_biodiv_names <- ground_biodiv_names[ground_biodiv_names %in% names(dat)]
  load(file = glue::glue("data/inter/sat_biodiv_names_{dat_in}.RData"));     sat_biodiv_names <- sat_biodiv_names[sat_biodiv_names %in% names(dat)]
  load(file = glue::glue("data/inter/biodiv_names_{dat_in}.RData"));         biodiv_names <- biodiv_names[biodiv_names %in% names(dat)]
  
  
  
  # ### Scatterplots ---------------------------------------------------------------
  # main_corrs <- corr_mat %>% # rearrange correlation matrix into tabular format
  #   as_tibble() %>%
  #   mutate(x = dat_norm %>% dplyr::select(-SITE_ID) %>% names(), .before = everything(),
  #          x_type = case_when(
  #            x %in% c("n_spp", "shannon", "pielou_evenness", "simpson_diversity", "bray_curtis") ~ "ground biodiversity",
  #            x == "sat_phen_std" ~ "satellite phenological biodiversity",
  #            x == "ground_phen_std" ~ "ground phenological biodiversity",
  #            str_detect(x, "RaoQ") ~ "satellite raoq",
  #            x %in% c("GPP_sat", "NEP_95", "NEP_99", "Gs_max", "CUE_eco_10", "CUE_eco_50", "CUE_eco_90") ~ "efp"
  #            )
  #          ) %>% 
  #   # exclude redundant information
  #   dplyr::filter(x_type != "efp") %>%
  #   dplyr::select(c(x, x_type, names(efps %>% select(-SITE_ID)))) %>% 
  #   # convert to long format
  #   pivot_longer(cols = !c(x, x_type), names_to = "y", values_to = "correlation") %>% 
  #   relocate(y, .before = everything()) %>% 
  #   # extract biggest correlations
  #   group_by(x_type) %>% 
  #   arrange(desc(abs(correlation)), .by_group = T) %>% 
  #   mutate(idx = row_number()) %>% 
  #   ungroup() %>% 
  #   dplyr::filter(idx <= 3) %>%
  #   select(-idx) %>% 
  #   arrange(x_type)
  # main_corrs
  # 
  # 
  ### Multivariate analysis (for single EFPs) ------------------------------------
  ## Select predictors
  # Extract variables for the model (select e.g. GPPSAT (for vif testing) and predictors)
  df_vifed <- dat_norm %>% 
    dplyr::select(!c(SITE_ID, !!!syms(efps_names)), GPP_sat) %>% # here it doesn't matter which variable is being predicted, we are just testing the predictors
    tidyr::drop_na() %>% 
    glimpse()
  
  
  ## Test collinearity (VIF) ----
  options(na.action = "na.fail") # global options for na.action (needed)
  
  txt <- "==> Computing Variance Inflation Factor to select explanatory variables for further analyses."
  print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
  
  for (vv in 3) {
    # if (vv == 1) {
    #   df_subvifed <- df_vifed %>% dplyr::select(all_of(sat_biodiv_names), GPP_sat) # raoq biodiversity proxies
    # } else
    # if (vv == 2) {
    #   df_subvifed <- df_vifed %>% dplyr::select(all_of(stringr::str_subset(struct_names, "CrownDiameter")), GPP_sat) # crown diameter measurements (structure variables)
    # } else {
      df_subvifed <- df_vifed %>% dplyr::select(!all_of(clim_names), GPP_sat) # final VIF on all (?) variables: align dataframes for VIF processing
    # }
    # } else if (vv == 3) {
    #   df_subvifed <- df_vifed %>% dplyr::select(all_of(ground_biodiv_names), GPP_sat) # ground biodiversity variables
    # } else if (vv == 4) {
    #   df_subvifed <- df_vifed %>% dplyr::select(all_of(clim_names), GPP_sat) # climate variables
    
    n_excl <- 1 # initialize
    var_excl_list <- c()
    while (n_excl > 0) {
      
      ## Generalized Linear Model with all predictors
      # (cf. normality assumption)
      formula1 <- as.formula(glue::glue("GPP_sat ~ .")) # here it doesn't matter which variable is being predicted, we are just testing the predictors
      fm1 <- glm(formula1, data = df_subvifed) # generalized linear model
      
      ## calculate variable inflation factor
      vif_excl <- bind_rows(vif(fm1)) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "inflation_factor") %>% 
        print(n = Inf)
      
      # check how many variables are have vif above threshold
      n_excl <- vif_excl %>% 
        dplyr::filter(inflation_factor > 10) %>%
        nrow()
      
      txt <- glue::glue("{n_excl} variables above inflation factor of 10.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
      
      if (n_excl > 0) {
        ## exclude variables with values over 10
        var_excl <- vif_excl %>%
          dplyr::filter(inflation_factor > 10) %>% 
          dplyr::filter(inflation_factor == max(inflation_factor)) %>% 
          pull(variable) %>% 
          rlang::sym()
        
        var_excl_list <- c(var_excl_list, as.character(var_excl))
        
        txt <- glue::glue("Removing variable with highest vif value ({var_excl}).")
        print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        
        df_vifed <- df_vifed %>% dplyr::select(-!!var_excl) # remove variable from list of explanatories
        df_subvifed <- df_subvifed %>% dplyr::select(-!!var_excl) # remove variable from subset
      }
    }
    
    txt <- glue::glue("Test of variance inflation factor excluded the following variables: {paste(var_excl_list, collapse = ', ')}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
  }
  predictors <- df_vifed %>% select(-GPP_sat) %>% names() %>% print()
  
  
  # ## Check assumptions for regression ----
  # txt <- "==> Testing assumption of normality on variables' residuals."
  # print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
  # 
  # ## Normal distribution of predictors:
  # # perform Shapiro-Wilk's test
  # shapiro_stats <- dat_norm %>%
  #   dplyr::select(all_of(predictors)) %>% 
  #   rstatix::shapiro_test(vars = names(dat_norm %>% dplyr::select(all_of(predictors) & where(is.numeric)))) %>%
  #   mutate(normality = if_else(p > 0.1, TRUE, FALSE))
  # shapiro_stats
  # 
  # txt <- glue::glue("{nrow(shapiro_stats) - shapiro_stats %>% pull(normality) %>% sum()} out of {nrow(shapiro_stats)} variables did NOT meet the assumption of normality.")
  # print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
  # 
  # # # Transform data?
  # # rcompanion::transformTukey()
  # 
  # # Residual plot
  # plot(fm1)
  # 
  # 
  
  ### Multimodel inference - one variable per group at a time --------------------
  if (out_bb == "no") { # A) NO BIODIVERSITY ----
    # Approximate running time: 30 seconds
    ## Select biodiversity predictors
    predictors_bb <- predictors[predictors %in% c(clim_names, struct_names)]
    
    ## Announce analysis
    txt <- glue::glue("==> Analysis on {out_bb} biodiversity predictors.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    ## Do analysis via function
    relimp_all <- do_multimodel_relaimpo(dat_norm, efps_names, predictors_bb, savedata, eval_file, vers_out)
    
    ## Save
    if (savedata) {
      # model output and variable importance
      write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
    }
    
    
    
  } else if (out_bb == "sub") { # B-C) SEPARATED GROUND/SATELLITE BIODIVERSITY ----
    # Approximate running time: 12 minutes
    for (bb in 1:2) {
      ## Select biodiversity predictors
      if (bb == 1) { # B) SATELLITE BIODIVERSITY
        out_bb <- "satellite"; vers_out <- glue::glue("{out_bb}_{raoq_in}_{vers}")
        predictors_bb <- predictors[!predictors %in% ground_biodiv_names & predictors != "ground_phen_std"]
      } else if (bb == 2) { # c) GROUND BIODIVERSITY
        out_bb <- "ground"; vers_out <- glue::glue("{out_bb}_{raoq_in}_{vers}")
        predictors_bb <- predictors[!predictors %in% sat_biodiv_names & predictors != "sat_phen_std"]
      }
      
      ## Announce analysis
      txt <- glue::glue("==> Analysis on {out_bb} biodiversity predictors.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
      
      ## Do analysis via function
      relimp_all <- do_multimodel_relaimpo(dat_norm, efps_names, predictors_bb, savedata, eval_file, vers_out)
      
      ## Save
      if (savedata) {
        # # Data
        # write_csv(dat_out, glue::glue("data/inter/data_efps_clim_struct_biodiv_{vers_out}.csv"))
        # # Site List 
        # write_csv(dat_sites, glue::glue("results/multimodel_inference/site_list_{vers_out}.csv"))
        # Model output and variable importance
        write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
      }
    } # end repeat for biodiversity ground vs biodiversity satellite
    
    
  } else if (out_bb == "all") { # D) ALL BIODIVERSITY ----
    # Approximate running time: > 1 hour
    ## Select biodiversity predictors
    predictors_bb <- predictors
    
    ## Announce analysis
    txt <- glue::glue("==> Analysis on {out_bb} biodiversity predictors.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    ## Do analysis via function
    relimp_all <- do_multimodel_relaimpo(dat_norm, efps_names, predictors_bb, savedata, eval_file, vers_out)
    
    ## Save
    if (savedata) {
      # model output and variable importance
      write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
    }
    
    
    
  } else if (out_bb == "main") { # E) MOST IMPORTANT PREDICTORS (a-posteriori) ----
    # Approximate running time: 1 s
    ## Load data
    dat_all <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_all}.csv"), show_col_types = F) %>% 
      # add variable categories
      mutate(var_type = case_when(
        variable %in% clim_names ~ "Climate",
        variable %in% struct_names ~ "Structure",
        variable %in% ground_biodiv_names ~ "Ground Biodiversity",
        variable %in% sat_biodiv_names ~ "Satellite Biodiversity"
        )
      ) %>%
      glimpse()
    
    ## Extract important predictors
    predictors_bb <- dat_all %>%
      dplyr::filter(variable %in% names(dat)) %>%
      group_by(prediction, var_type) %>% 
      mutate(quant_relaimpo = quantile(rel_importance, 0.60)) %>% 
      dplyr::filter(rel_importance >= quant_relaimpo) %>% # relative importance above 75th quantile threshold within group of similar variables (for each EFP)
      dplyr::filter(rel_importance >= 0.01) %>% # relative importance above absolute minimum value
      # keep variables with relative importance above quantile threshold within each group (biodiversity, climate, structure),
      # and above 1% absolute relative importance
      ungroup() %>% 
      dplyr::select(prediction, variable) %>% 
      glimpse()
    
    
    ## Announce analysis
    txt <- glue::glue("==> A-posteriori analysis on {out_bb} biodiversity predictors, including leave-one-out cross validation.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    
    ## Do analysis via function
    relimp_all <- do_multimodel_relaimpo(dat_norm, efps_names, predictors_bb, savedata, eval_file, vers_out)
    relimp_crossval <- do_crossval_relaimpo(dat_norm, efps_names, predictors_bb, savedata, eval_file, vers_out)
    
    ## Attach loo RMSE
    relimp_all <- relimp_all %>% 
      left_join(relimp_crossval %>% dplyr::select(prediction, RMSE) %>% unique(), by = "prediction", suffix = c("", "_loo")) %>% 
      relocate(RMSE_loo, .after = RMSE)
    
    ## Save
    if (savedata) {
      # model output and variable importance
      write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
      # # cross validation
      # write_csv(relimp_crossval, glue::glue("results/multimodel_inference/prediction_cross_validation_{vers_out}.csv"))
    }
    
    
  } else if (out_bb == "no_main") { # F) MOST IMPORTANT PREDICTORS WITHOUT BIODIVERSITY (a-posteriori) ----
    # Approximate running time: 1 s
    ## Load data
    dat_all <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_all}.csv"), show_col_types = F) %>%
      # add variable categories
      mutate(var_type = case_when(
        variable %in% clim_names ~ "Climate",
        variable %in% struct_names ~ "Structure",
        variable %in% ground_biodiv_names ~ "Ground Biodiversity",
        variable %in% sat_biodiv_names ~ "Satellite Biodiversity"
      )
      ) %>%
      glimpse()

    ## Extract important predictors
    predictors_bb <- dat_all %>%
      dplyr::filter(variable %in% names(dat)) %>%
      dplyr::filter(!var_type %in% c("Ground Biodiversity", "Satellite Biodiversity")) %>% # EXCLUDE BIODIVERSITY
      group_by(prediction, var_type) %>%
      mutate(quant_relaimpo = quantile(rel_importance, 0.60)) %>%
      dplyr::filter(rel_importance >= quant_relaimpo) %>% # relative importance above 75th quantile threshold within group of similar variables (for each EFP)
      dplyr::filter(rel_importance >= 0.01) %>% # relative importance above absolute minimum value
      # keep variables with relative importance above quantile threshold within each group (biodiversity, climate, structure),
      # and above 1% absolute relative importance
      ungroup() %>%
      dplyr::select(prediction, variable) %>%
      glimpse()


    ## Announce analysis
    txt <- glue::glue("==> A-posteriori analysis on {out_bb} biodiversity predictors, including leave-one-out cross validation.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}


    ## Do analysis via function
    relimp_all <- do_multimodel_relaimpo(dat_norm, efps_names, predictors_bb, savedata, eval_file, vers_out)


    ## Save
    if (savedata) {
      # model output and variable importance
      write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
    }


  } else {
    warning("Contrasting conditions. Subset biodiversity predictors AND subdivide into ground and satellite?")
  } # end conditions
  
  
} # end loop to repeat analysis for different raoQ inputs



### End ------------------------------------------------------------------------
toc()