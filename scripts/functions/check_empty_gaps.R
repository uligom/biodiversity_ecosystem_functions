#### Function to check for empty site-years:
# check if filtering procedures have generated empty outputs for specific years
# If that is the case --> insert NA

# dat: dataset to be tested and modified (NA row filling for year-spanning gaps)
# dat needs a "YEAR" column

check_empty_gaps <- function(dat = data_test, start_seq = 1996, end_seq = 2008) {
  require(dplyr)
  nan_years <- setdiff(seq(start_seq, end_seq, by = 1), dat$YEAR)
  if (!is_empty(nan_years)) {
    dat <- dat %>%
      tibble() %>% 
      add_row(YEAR = nan_years) %>% 
      arrange(YEAR)}
  return(dat)
}


### Test:
# debugonce(check_empty_gaps)


## generate dataset for testing

# # - without gap in year sequence
# data_test <- data.frame(YEAR = c(1996:2008),
#                         morevar = c("a","b","c","d","e","f","g","h","i","j","k","l","m"))

# # - with 1-year gap in year sequence
# data_test <- data.frame(YEAR = c(1996:2001, 2003:2008),
#                         morevar = c("a","b","c","d","e","f","g","h","i","j","k","l"))

# # - with multi-year gaps in year sequence
# data_test <- data.frame(YEAR = c(1996:2001, 2004:2005, 2007:2008),
#                         morevar = c("a","b","c","d","e","f","g","h","i","j"))

# # - with gaps at start and/or end of year sequence
# data_test <- data.frame(YEAR = c(1997:2001, 2003:2007),
#                         morevar = c("a","b","c","d","e","f","g","h","i","j"))

# ## run function
# new_data_test <- check_empty_gaps(data_test)