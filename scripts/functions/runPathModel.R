### Arguments ------------------------------------------------------------------
# x_sem:          data frame of the input data
# model_struct:   model structure



###  Main Function Path Model --------------------------------------------------
runPathModel <- function(x_sem, model_struct, savedata = F, print_title = T) {
  ## Utilities ----
  require(corrplot)
  require(dplyr)
  require(lavaan)
  require(semPlot)
  
  
  ## Start ----
  print("-- Printing model structure")
  print(model_struct)
  
  print("-- Printing data frame")
  print(x_sem)
  
  print("-- Tuning path modelling Step 1")
  model_struct %>% # test plot 1
    lavaan::sem(sample.cov = cor(x_sem), sample.nobs = base::dim(x_sem)[1], fixed.x = FALSE) %>%
    semPlot::semPaths(whatLabels = "stand", layout = "spring")
  
  semMod1 <- lavaan::sem(model_struct, 
                 sample.cov  = cor(x_sem), 
                 sample.nobs = dim(x_sem)[1], 
                 fixed.x = FALSE,
                 bootstrap = TRUE)
  
  summary_SEM <- summary(semMod1, standardized = T, fit.measures = TRUE, rsquare = T)
  #varTable(semMod1)
  fitmeasuresSEM <- fitmeasures(semMod1)
  #fitmeasures(semMod1, c('cfi', 'rmsea', 'srmr'))
  semPaths( # test plot 2
    semMod1, what = 'std', nCharNodes = 6, sizeMan = 6, curveAdjacent = TRUE, style = "lisrel",
    edge.label.cex = 1.25, curvePivot = TRUE, fade = FALSE, layout = "tree2",
    rotation = 1
  )
  modelname <- strsplit(model_struct, spli = "\n")[[1]][2]
  
  
  ## Save model output --------------
  if (savedata) {
    save(semMod1, file = glue::glue("results/SEM/{modelname}_SEM_out", ".RDA"))
    save(fitmeasuresSEM, file = glue::glue("results/SEM/{modelname}_fitmeasuresSEM", ".RDA"))
    save(summary_SEM, file = glue::glue("results/SEM/{modelname}_summary_SEM", ".RDA"))
  }
  
  ## Other tests ----
  # Check the P-val of the regressors if very low, the hp are confirmed and paths are significant
  # Chi-square very significant p < 0.0 means that global fit is poor
  # RMSEA should be below 0.08 to be acceptable
  # CFI should be above 0.95
  # If these parameters are not satisfying we can still look if the modeled covariance matrix approaches the sampled (observed) covariance matrix
  
  fitted(semMod1)
  
  #-- Modelled correlation matrix
  inspect(semMod1, what = "cor.all")
  
  #-- Observed correlation matrix
  lavCor(semMod1)
  
  #-- Calculate the residuals of the correlation matrix (positive vals means model underprecit correlation, negative model overpredict correlation).
  # Values above abs(0.1) are worth looking at
  resid(semMod1, "cor")
  
  plot_matrix <- function(matrix_toplot){
    corrplot::corrplot(matrix_toplot, is.corr = FALSE,
                       type = 'lower',
                       order = "original",
                       tl.col = 'black', tl.cex = .75)
  }
  
  
  ## Correlation matrix ----
  if (savedata) {
    png(file = paste0("results/SEM/", modelname, "_corrplot_fitted_model.png"),
        width = 1600, height = 1600, res = 300)
    plot_matrix(resid(semMod1, "cor")$cov)
    dev.off()
  }
  
  
  
  ## Modification indices ----
  # -- Analysis of the modification indices to see if we can fix the misfit by freeing or adding one or more paths, 
  # we should look at the mi of the path and print only mi > 20
  
  output_modificationindices <- modificationindices(semMod1, minimum.value = 10)
  
  print("-- Print modification indices")
  print(output_modificationindices)
  
  if (savedata){
    save(output_modificationindices, file = paste0("results/SEM/", modelname, "_output_modificationindices_SEM", ".RDA"))
  }
  
  summary(semMod1, fit.measure = TRUE)
  
  
  ## Plot ----
  print("-- Printing paths with semPaths")
  
  if (savedata){
    png(file = paste0("results/SEM/", modelname, "_PlotPaths.png"), width= 1600, height = 1600, res = 300)
    
    # b <- gettextf('%.3f \n p=%.3f', table2$std.all, digits = table2$pvalue)
    
    semPaths(
      semMod1,
      #edgeLabels = b,
      what = 'stand',
      whatLabels = 'stand',
      style = 'ram',
      layout = 'tree',
      intercepts = FALSE,
      residuals = FALSE,
      nCharNodes = 0,
      sizeMan = 14,
      sizeMan2 = 7,
      ask = FALSE,
      title = print_title,
      # color = , # can be a color vector indicating the color for each node separately. Can also be a list contaning one or more of the following elements (using fuzzy matching), e.g.: list(man = "red", lat = "blue", int = "gray50"),
      # edge.color = , # a value indicating the color of all edges or a vector indicating the color of each edge. Useful for manually overwriting edge colors.
      reorder = TRUE,
      # nodeLabels = , # a vector or list to manually overwrite the node labels. Can include expressions.
      # edgeLabels = , # a vector or list to manually overwrite the edge labels. Can include expressions.
      edge.label.cex = 1,
      # esize = 11, # size of the largest edge
      # asize = 5, # size of the arrowhead
      edge.width = 1.5,
      normalize = T,
      posCol = "#00A0A0",
      negCol = "#A00000",
      trans = F, fade = F,
      curvePivot = TRUE,
      edge.label.position = 0.45,
    )
    if (print_title) {
      require(stringr)
      title_label <- stringr::str_extract(modelname, "[:graph:]+(?= ~)")
      title(title_label)
    }
    
    dev.off()
  }
  
  # model_complete %>% 
  #   lavaan::sem(sample.cov  = cor(x_sem), sample.nobs = dim(x_sem)[1]) %>% 
  #   lavaanPlot(node_options = list(shape = "box", fontname = "Helvetica"), 
  #              edge_options = list(color = "grey"), coefs = TRUE,covs=TRUE,
  #              stars = c("regress"))
  
  #fit<-lavaan(model_climate_only1,data=x_sem,auto.var=TRUE)
  
  # print("-- Printing plot with LavaanPlot")
  # png(file = paste0('plot/', modelname, "_LavaanPlotPaths.png"), width= 1600, height = 1600, res = 300)
  # lavaanPlot(model = semMod1, 
  #            #node_options = list(shape = "box", fontname = "Helvetica"), 
  #            #edge_options = list(color = "grey"), 
  #            coefs = TRUE, 
  #            covs = TRUE, stars = c("regress"), stand = TRUE)
  # dev.off()
  # 
  # lavaanPlot(model = semMod1, 
  #            node_options = list(shape = "box", fontname = "Helvetica"), 
  #            edge_options = list(color = "grey"), 
  #            coefs = TRUE, 
  #            covs = TRUE, stars = c("regress"), stand = TRUE)
  # 
  # savePlot(file = paste0(plotpath, modelname, "_LavaanPlotPaths.png"), type = "png", device=dev.cur())
  
  return(semMod1)
}
### Debug ----------------------------------------------------------------------
# debugonce(runPathModel)