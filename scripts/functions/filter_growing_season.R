filter_growing_season <- function(data = data, GPPd = "GPP_daily", tGPP = GSfilt) {
  GPPd <- rlang::sym(GPPd) # quote symbol
  output <- bigleaf::filter.growing.season(GPPd = data %>% dplyr::pull(!!GPPd), tGPP = tGPP)
  output <- as.logical(output)
  return(output)
}
# debugonce(filter_growing_season)