#### COMBINE TEA TRANSPIRATION ESTIMATES FOR NEON SITES ####

### Authors
# TEA partitioning by Jacob A. Nelson
# Script by Ulisse Gomarasca

### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Data settings
savedata <- T #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

path_in <- "//minerva/BGI/people/jnelson/Datasets/NEON_TEA/outputs"



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plots
library(lubridate)  # tidy dates
library(readr)      # read table format files
library(stringr)    # string manipulation



### Data -----------------------------------------------------------------------
sites <- stringr::str_extract(list.files(path_in), "[:upper:]{4}")

tea_t <- tibble()
for (ii in 1:length(sites)) {
  tea_t <- bind_rows(tea_t,
                     bind_cols(SITE_ID = sites[ii],
                               readr::read_csv(paste(path_in, list.files(path_in)[ii], sep = "/"), # read individual files
                                               show_col_types = F
                               ) %>%
                                 dplyr::rename(DATETIME = time) # rename for consistency and clarity
                     )
  )
}
tea_t %>% glimpse()



### Plot time series -----------------------------------------------------------
p_daily <- tea_t %>% 
  mutate(DATE = as_date(DATETIME)) %>% 
  group_by(SITE_ID, DATE) %>% 
  summarise(TEA_T_NT_75 = mean(TEA_T_NT_75, na.rm = T),
            TEA_E_NT_75 = mean(TEA_E_NT_75, na.rm = T),
            .groups = "drop"
            ) %>% 
  pivot_longer(cols = c(TEA_T_NT_75, TEA_E_NT_75)) %>% 
  glimpse() %>% 
  ggplot(aes(x = DATE, y = value, color = name), alpha = 0.5) +
  geom_line() +
  facet_wrap(. ~ SITE_ID) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 13),
        strip.background = element_blank()
  ) +
  NULL
p_daily



### Save -----------------------------------------------------------------------
if (savedata) {
  ## Data
  write_csv(tea_t, paste0("data/input/NEON/fluxes/TEA_transpiration_partitioned_fromJake.csv"))
  
  ## Plots
  ggsave(filename = "daily_T_vs_E.jpg",
         plot = p_daily, device = "jpeg", path = "results/timeseries",
         width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
}