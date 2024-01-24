#### LOAD, PROCESS, AND MERGE BIODIVERSITY METRICS

### Function -------------------------------------------------------------------
extract_biodiv_metrics <- function(biodiv_indices = T, phenology = T, raoq = T, biodiv_in = "v02") {
  ## Utilities ----
  require(dplyr)
  require(readr)
  require(stringr)
  
  
  ## Data ----
  ## Ground biodiversity indices
  if (biodiv_indices) {dat_biodiv <- read_csv(glue::glue("data/inter/biodiv_indices_all_{biodiv_in}.csv"), show_col_types = F)}
  
  
  ## Ground and satellite phenology
  if (phenology) {dat_phenol <- read_csv("data/inter/phen_ground_sat.csv", show_col_types = F)}
  
  
  ## Satellite RaoQ
  if (raoq) {
    dat_raoq <- read_csv("data/input/Sentinel2/RaoQ/raoQ_S2_L2A_filtered_averaged.csv", show_col_types = F) %>%
      arrange(SITE_ID) %>% 
      glimpse()
  }
  # Explanation of RaoQ calculations ----
  ## 1) RaoQ calculated on every scene (i.e. at every date) by Javier Pacheco-Labrador
  ## Javier's explanation (might be outdated):
  # I computed RaoQ for the S2 bands (['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12']), NDVI and NIRv in 3x3 moving windows (I did not use all the pixels since I've seen that large windows reduce the correlation with the actual functional diversity). Then I computed the median and the standard deviation of the windows. I only kept the windows where all the pixels were available. For L2A, I only used the vegetation pixels.
  # Why NDVI and NIRv, this is a question I wanted to check. I wanted to know if a single index could get the same variability as the full band's set. In the case of your dataset, at least, RaoQ from the median spectral reflectance correlates more with the one from NIRv than NDVI. I do not know yet which one correlates more with functional diversity, that'd be my next check (I want to explore this modeling and understand better).
  #   
  # Regarding the contents:
  #
  # - 'Site'
  # - 'f_valid_samples': Fraction of the full image that could be used for analysis (it might be never 100 % if you got kept only usable values in circular ROIs).
  #   This the fraction refers to the square. You might need to re-normalize the 100 %  as (f_valid_samples / 0.785)
  # - RaoQ of the bands, I either used the median reflectance of the time series (RaoQ_S2median) of the reflectance of the pixels with max NDVI (RaoQ_S2ndvimax)
  #   - 'RaoQ_S2ndvimax_median'
  #   - 'RaoQ_S2ndvimax_std'
  #   - 'RaoQ_S2median_median'
  #   - 'RaoQ_S2median_std'
  # - For NDVI and NIRv I either used the maximum or the median value of each pixel in the time series
  #   - 'RaoQ_NDVImax_median'
  #   - 'RaoQ_NDVImax_std'
  #   - 'RaoQ_NDVImedian_median'
  #   - 'RaoQ_NDVImedian_std'
  #   - 'RaoQ_NIRvmax_median'
  #   - 'RaoQ_NIRvmax_std'
  #   - 'RaoQ_NIRvmedian_median'
  #   - 'RaoQ_NIRvmedian_std'
  # - I kept the median and standard deviation from the max and median NDVI and NIRv
  #   - 'NDVImax_median'
  #   - 'NDVImax_std'
  #   - 'NDVImedian_median'
  #   - 'NDVImedian_std'
  #   - 'NIRvmax_median'
  #   - 'NIRvmax_std'
  #   - 'NIRvmedian_median'
  #   - 'NIRvmedian_std'
  #
  # 2) Filtering of calculated RaoQ based on fraction of valid pixels in scene,
  # and on NDVI threshold (90th percentile over whole site timeseries/space)
  #
  # 3) Aggregation to mean values for each site
  
  
  ## Processing ----
  ## Filter raoq
  if (raoq) {
    dat_raoq <- dat_raoq %>% # remove sites with "Fraction of the full image that could be used" below threshold (same as for phenodiversity)
      # dplyr::filter(stringr::str_detect(SITE_ID, "[:upper:]{4}")) %>% # filter NEON
      dplyr::select(SITE_ID, contains("RaoQ") & !contains("std")) %>% # keep relevant metrics
      glimpse()
  }
  
  
  ## Merge ----
  dat_out <- tibble() # initialize
  if (biodiv_indices) {dat_out <- dat_out %>% bind_rows(dat_biodiv)}
  
  if (biodiv_indices == T & phenology == T) {
    dat_out <- dat_out %>% left_join(dat_phenol, by = "SITE_ID")
  } else if (biodiv_indices == F & phenology == T) {
    dat_out <- dat_out %>% bind_rows(dat_phenol)
  }
  
  if ((biodiv_indices == T | phenology == T) & raoq == T) {
    dat_out <- dat_out %>% left_join(dat_raoq, by = "SITE_ID")
  } else if (biodiv_indices == F & phenology == F & raoq == T) {
    dat_out <- dat_out %>% bind_rows(dat_raoq)
  }
  
  
  ## Output ----
  return(dat_out)
}



### Debugging ------------------------------------------------------------------
# debugonce(extract_biodiv_metrics)