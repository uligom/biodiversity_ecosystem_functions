#### Repeat values of group summary table over dataset containing duplicate grouping values
## Function to count number of entries per year and repeat input values for each year
# repeat values of group summary table over dataset containing duplicate grouping values

# data_x must have a value for each corresponding year in data_y
# data_y requires a 'YEAR' column

# Example: repeating NEE_99 yearly values (data_x) for the number of rows corresponding to that year in the GPPsat output (data_y)
rep_counts <- function(data_x = NEP_99$NEP_99, data_y = outGPPsat) {
  require(dplyr)
  counts <- data_y %>% tibble() %>% count(YEAR) %>% dplyr::select(n) %>% unlist() %>% as.integer()
  out <- data_x %>% rep(counts)
  return(out)
}

## Test:
# debugonce(rep_counts)
# 
# y <- data.frame(year = c(2001, 2001, 2001, 2002, 2002, 2003, 2003, 2003, 2003))
# rep_counts(1:3, y)