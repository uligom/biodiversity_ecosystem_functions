#### Function for filtering, GPPsat calculation and more (Mirco Migliavacca)
# reorganized and improved, added additional information in output (Ulisse Gomarasca)

### Arguments -------------------------------------------------------------------
# data   = dataframe containing fluxnet data for single sites
# site   = character, name of the site
# QCfilt  = Integer or vector of integers from 0 to 4 corresponding to Quality
#           Checks to be retained. E.g. GQfilt = c(0, 1) filters out bad quality
#           data with quality flag set to 2, 3, or 4.
# GSfilt  = Number between 0 and 1, corresponding to Growing Season filter, i.e.
#           the relative minimum threshold (e.g. 0.3 = 30%) to be excluded from the
#           range of the GPP data.
# Pfilt   = Precipitation threshold used to identify a precipitation event (mm) (from filter.data function).
# partit = unused, character, either "GPP" or "GPP_DT"
            # NB: RECO_DT??? instead of RECO_NT?

## Changelog:
# 23.03.2021: added handling of empty data_subset dataframes with if-else statement
#             returning NA dataframe if empty
# 02.06.2021: quality filter changed to c(0, 1) instead of 1 only (should not change outcome).
#             added arguments for filtering thresholds and conditions.
# 29.06.2021: added GPP extraction



### Function -------------------------------------------------------------------
calc_GPPsat <- function(data, site, group = NULL, QCfilt = c(0, 1), GSfilt = 0.3, Pfilt = 0.1, SWfilt = 100, GPPsatfilt = 60) {
  ## Utilities ----
  require(bigleaf)
  require(dplyr)
  require(lubridate)
  require(rlang)
  require(tidyr)
  source("scripts/functions/check_empty_gaps.R")
  source("scripts/functions/myLRC.R")
  source("scripts/functions/rep_counts.R")
  
  
  ## Quote ----
  sites <- data %>% dplyr::pull(.data[[rlang::enquo(site)]]) %>% unique()
  if (!is.null(group)) {group <- rlang::sym(group)}
  
  
  ## Output by errors ----
  err_output <- tibble(SITE_ID    = sites,
                       YEAR       = NA_real_,
                       StartDate  = NA_real_,
                       EndDate    = NA_real_,
                       FiveDaySeq = NA_real_,
                       GPP_NT_day = NA_real_,
                       GPPsat     = NA_real_,
                       GPPsat_95  = NA_real_,
                       NEP_95     = NA_real_,
                       NEP_99     = NA_real_) # output when error is encountered
  
  
  ## Processing ----
  print(glue::glue("....computing LRC parameters for site {sites}."))
  
  ## Define 5-days moving window ----
  data$FiveDaySeq <- rep(c(1:ceiling(nrow(data)/5)), each = 48 * 5, length.out = nrow(data))
  
  
  ## Filtering ----
  # Filter daytime for GPP
  data <- data %>% 
    dplyr::filter(SW_IN > SWfilt) # filter instead of replacing NA only for GPP (as in previous script), but move generation of 5-days windows before

  
  ## Calculate photosynthetic capacity and related information ----
  # Subset for calculations:
  data_subset <- data %>% 
    dplyr::select(SITE_ID, DATETIME, YEAR, NEE, PPFD_IN_FROM_SWIN, RECO, FiveDaySeq) %>% 
    dplyr::rename(PPFD = PPFD_IN_FROM_SWIN, Reco = RECO) %>% 
    tidyr::drop_na() %>% 
    dplyr::left_join(data %>% dplyr::select(DATETIME, GPP) %>% tidyr::drop_na()) # join back GPP without influencing subset at previous step

  if (nrow(data_subset) != 0) {
    ## GPPsat and reference GPP ----
    # tic()
    # zzz <- data_subset %>% dplyr::filter(FiveDaySeq %in% c(37, 38))
    # myLRC(zzz %>% group_by(FiveDaySeq))
    # # GPPsat <- unlist(by(data_subset, data_subset$FiveDaySeq, myLRC))
    # toc()
    # 
    # tic()
    # dat_out <- data_subset %>% 
    #   dplyr::left_join(data %>% dplyr::select(DATETIME, GPP) %>% tidyr::drop_na()) %>% # join back GPP without influencing subset at previous step
    #   dplyr::group_by(FiveDaySeq) %>% 
    #   dplyr::mutate(GPPsat = myLRC(.),
    #                 GPP_day = mean(GPP, na.rm = T)
    #                 )# %>% 
    #   # dplyr::left_join(GPPsat)
    # toc()
    
    
    # initialize empty vectors
    GPPsat <- vector("list", length = length(unique(data_subset$FiveDaySeq)))
    GPP <- vector("list", length = length(unique(data_subset$FiveDaySeq)))
    StartDate <- data.frame("StartDate" = lubridate::POSIXct())
    EndDate <- data.frame("EndDate" = lubridate::POSIXct())
    YEAR <- vector("list", length = length(unique(data_subset$FiveDaySeq)))

    j <- 1 # row index
    for (i in unique(data_subset$FiveDaySeq)) {
      # calculate GPPsat:
      GPPsat[j] <- data_subset %>%
        dplyr::filter(FiveDaySeq == i) %>%
        myLRC()
      # GPP:
      GPP[j] <- data_subset %>% # append GPP
        dplyr::filter(FiveDaySeq == i) %>%
        summarize(GPP = mean(GPP, na.rm = T))
      # time frame:
      StartDate[j,1] <- data_subset$DATETIME[data_subset$FiveDaySeq == i][1]  # start date
      EndDate[j,1]   <- data_subset$DATETIME[data_subset$FiveDaySeq == i][length(data_subset$DATETIME[data_subset$FiveDaySeq == i])] # end date
      YEAR[j]        <- data_subset$YEAR[data_subset$FiveDaySeq == i][1]      # year (start date)
      j <- j + 1
    }
    GPP <- GPP %>% unlist(GPP)
    GPPsat <- GPPsat %>% unlist(GPPsat)
    YEAR <- YEAR %>% unlist(YEAR)

    outGPPsat <- cbind(GPP,
                       GPPsat,
                       StartDate,
                       EndDate,
                       YEAR
    ) %>%
      as.data.frame() %>%
      mutate(FiveDaySeq = unique(data_subset$FiveDaySeq)) %>%
      dplyr::filter(GPPsat < GPPsatfilt) %>% # NB: GPPsat > 60 excluded as outliers
      check_empty_gaps(start_seq = min(data_subset$YEAR, na.rm = T), end_seq = max(data_subset$YEAR, na.rm = T))


    ## GPPsat_95 ------
    GPPsat_95 <- outGPPsat %>%
      as.data.frame() %>%
      check_empty_gaps(start_seq = min(data_subset$YEAR, na.rm = T), end_seq = max(data_subset$YEAR, na.rm = T)) %>%
      group_by(!!group) %>%
      summarise(GPPsat95 = quantile(GPPsat, 0.95, na.rm = T)) # GPPsat quantile by years


    ## NEP ---------
    rmin <- 200 * 2.11  # minimum incoming radiation (PPFD)
    NEP <- data_subset %>%
      dplyr::filter(PPFD > rmin) %>%
      mutate(NEP = NEE * -1) %>%
      check_empty_gaps(start_seq = min(data_subset$YEAR, na.rm = T), end_seq = max(data_subset$YEAR, na.rm = T)) %>%
      group_by(!!group) %>%
      summarise(NEP95 = quantile(NEP, probs = 0.95, na.rm = TRUE),
                NEP99 = quantile(NEP, probs = 0.99, na.rm = TRUE)) %>%
      as.data.frame() # NEP quantile 95 and 99
    
    ## Output ---------
    return(tibble(SITE_ID    = sites,
                  YEAR       = outGPPsat$YEAR,
                  # StartDate  = outGPPsat$StartDate,
                  # EndDate    = outGPPsat$EndDate,
                  # FiveDaySeq = outGPPsat$FiveDaySeq,
                  GPP_NT_day = outGPPsat$GPP,
                  GPPsat     = outGPPsat$GPPsat,
                  GPPsat95  = rep_counts(GPPsat_95$GPPsat95, outGPPsat),
                  NEP95     = rep_counts(NEP$NEP95, outGPPsat),
                  NEP99     = rep_counts(NEP$NEP99, outGPPsat),
                  row.names  = NULL)
    )
  } else { # if no data is available after filtering
    warning(paste("Site", sites, "was skipped because of empty data."))
    ## Output ---------
    return(err_output)
  }
}

### Debug ----------------------------------------------------------------------
debugonce(calc_GPPsat)