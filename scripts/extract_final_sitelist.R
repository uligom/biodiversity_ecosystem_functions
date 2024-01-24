#### COMBINE LISTS OF SITES FOR ALL ANALYSES

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
vers_in <- "v11.06.06"
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- vers_in



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(readr)        # read table format files
library(stringr)      # string manipulation



### Data -----------------------------------------------------------------------
file_path <- "results/multimodel_inference/"
file_list <- list.files(path = file_path,
           pattern = paste0("site_list_\\w+", eval(vers_in))
           )
for (ii in 1:length(file_list)) {
  if (ii == 1) {
    dat <- tibble()
  }
  dat <- dat %>% 
    bind_rows(read_csv(glue::glue("{file_path}{file_list[ii]}"), show_col_types = F))
}

dat <- dat %>% unique() %>% glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(dat, glue::glue("results/site_list_{vers_out}.csv"))
}


### End ------------------------------------------------------------------------