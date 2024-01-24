### Modified function, original is bigleaf::filter.growing.season
# modified to relax the condition on how much data needs to be available in order for correct implementation
# Default behaves the same as original function.
# Argument min_length can be used to relax this condition.
# Modified by: Ulisse Gomarasca

filter.growing.season_relaxed <- function(GPPd, tGPP, ws = 15, min.int = 5, min_length = 0.5){
  
  if(sum(is.na(GPPd)) < min_length * length(GPPd)){
    
    growseas      <- rep(1,length(GPPd))
    GPP_threshold <- quantile(GPPd,probs=0.95,na.rm=TRUE)*tGPP
    
    ## smooth GPP
    GPPd_smoothed <- stats::filter(GPPd,method="convolution",filter=rep(1/ws,ws))
    
    ## set values at the beginning and end of the time series to the mean of the original values
    wsd <- floor(ws/2)
    GPPd_smoothed[1:wsd] <- mean(GPPd[1:(2*wsd)],na.rm=TRUE)
    GPPd_smoothed[(length(GPPd)-(wsd-1)):length(GPPd)] <- mean(GPPd[(length(GPPd)-(2*wsd-1)):length(GPPd)],na.rm=TRUE)
    
    # check for occurrence of missing values and set them to mean of the values surrounding them
    missing <- which(is.na(GPPd_smoothed))
    if (length(missing) > 0){
      if (length(missing) > 10){warning("Attention, there is a gap in 'GPPd' of length n = ",length(missing))}
      replace_val <- mean(GPPd_smoothed[max(1,missing[1] - 4):min((missing[length(missing)] + 4),length(GPPd_smoothed))],na.rm=TRUE)
      GPPd_smoothed[missing] <- replace_val
    }
    
    # filter daily GPP
    growseas[GPPd_smoothed < GPP_threshold] <- 0
    
    ## change short intervals to the surrounding values to avoid 'wrong' fluctuations
    intervals <- rle(growseas)
    short_int <- which(intervals$lengths <= min.int)
    
    if (length(short_int) > 0){
      start <- numeric()
      end   <- numeric()
      
      for (i in 1:length(short_int)){
        
        start[i] <- sum(intervals$lengths[1:short_int[i]-1]) + 1
        end[i]   <- start[i]+intervals$lengths[short_int[i]] - 1
        
        val <- unique(growseas[start[i]:end[i]])
        
        if (val == 0 & growseas[start[i]-1] == 1){
          growseas[start[i]:end[i]] <- 1   
        } else if (val == 1 & growseas[start[i]-1] == 0){
          growseas[start[i]:end[i]] <- 0
        }
      }
    }
    
    growseas <- as.integer(growseas)
    
  } else {
    
    warning("number of available GPPd data is less than half the total number of days per year. Filter is not applied!")
    growseas <- as.integer(rep(1,length(GPPd)))
    
  }
  
  return(growseas)
}