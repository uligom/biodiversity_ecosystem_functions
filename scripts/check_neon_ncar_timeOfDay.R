#### CHECK NEON-NCAR TIME

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
# savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plots




### Data -----------------------------------------------------------------------
load(file = "data/input/NEON/NCAR-NEON_fluxes_expanded.RData") # load NCAR-NEON fluxes ('dat')



### Plot -----------------------------------------------------------------------
## Shortwave incoming radiation
dat %>% 
  mutate(TIMEOFDAY = format(as.POSIXct(DATETIME),   # Extract hours, minutes & seconds
                            format = "%H:%M:%S")
         ) %>% 
  group_by(SITE_ID, TIMEOFDAY) %>% 
  summarise(SW_IN = mean(SW_IN, na.rm = T), .groups = "drop") %>% 
  glimpse() %>% 
  ggplot() +
  geom_line(aes(TIMEOFDAY, SW_IN, colour = SITE_ID, group = SITE_ID), linewidth = 0.8) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270))