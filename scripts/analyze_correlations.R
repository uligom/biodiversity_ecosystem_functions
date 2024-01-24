#### VIs vs structure

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
# Data settings
# savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
dat_in <- "v06.06"
vers_out <- dat_in



### Utilities ------------------------------------------------------------------
## Packages
# library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(readr)        # tidy read/save
library(tidyr)        # reorganize tibbles



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("data/inter/data_efps_clim_struct_biodiv_{dat_in}.csv"), show_col_types = F) %>%
  glimpse()



### Correlation test -----------------------------------------------------------
# cor(x = dat$NDVI_max, y = dat$Rao_Q_NIRv, method = "spearman", use = "complete.obs")
cor.test(x = dat$NDVI_max, y = dat$Rao_Q_NIRv, method = "spearman")