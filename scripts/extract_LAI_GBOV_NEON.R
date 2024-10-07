#### EXTRACT LAI DATA FOR NEON SITES

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
# Data settings
savedata = as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved



### Utilities ------------------------------------------------------------------
library(dplyr); options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # plots
library(lubridate)  # dates
library(readr)      # tidy read/write
library(stringr)    # tidy string manipulation
library(tidyr)      # tidy

# # Other
# source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
## List files in folder
folder_path <- "data/input/NEON/LAI/RM07/" # folder path
gbov_lai_files <- list.files(folder_path, pattern = ".csv") # list of files


## Read single csv file and merge into single dataframe
dat <- tibble() # initialize

for (ii in 1:length(gbov_lai_files)) {
  ## Read single csv file
  dat_ii <- read_delim(glue::glue("{folder_path}{gbov_lai_files[ii]}"), delim = ";", show_col_types = F,
                       col_types = "ccccccdcddTdcddddddddddddddddddddddddii",
                       na = "-999")
  
  
  ## Merge data
  dat <- bind_rows(dat, dat_ii)
} # end ii loop

rm(dat_ii) # clean memory


## Extract SITE_ID ----
dat <- dat %>% 
  mutate(SITE_ID = str_extract(PLOT_ID, "[:upper:]{4}")) %>% 
  relocate(SITE_ID, .before = Site) %>% 
  glimpse()



### Plot -----------------------------------------------------------------------
dat_plot <- dat %>% 
  pivot_longer(contains("LAI") & !contains("err"), names_to = "LAI_variable", values_to = "LAI_values") %>%
  mutate(direction = str_extract(LAI_variable, pattern = "up|down") %>% 
           factor(levels = c("up", "down")),
         LAI_variable = str_replace(LAI_variable, pattern = "up|down", replacement = "") %>% 
           str_replace_all(pattern = "_", replacement = " ") %>% 
           str_trim("right") %>% 
           factor(levels = c("LAIe Miller", "LAI Miller", "LAIe Warren", "LAI Warren"))
  )

p_lai_all <- dat_plot %>% 
  ggplot() +
  geom_point(aes(x = TIME_IS, y = LAI_values, color = LAI_variable, shape = direction),
             alpha = 1, size = 1.5, na.rm = T) +
  facet_wrap(. ~ SITE_ID) +
  scale_color_brewer(palette = "Paired") +
  scale_shape_manual(values = c(down = 6, up = 2), drop = F) +
  guides(color = guide_legend(title = "LAI calculation", override.aes = list(size = 4)),
         shape = guide_legend(title = "Camera direction", override.aes = list(size = 4), reverse = T)) +
  ylab(expression(paste("LAI [",m^{2}," ", m^{-2},"]"))) +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.title.x = element_blank(), # remove empty space of xlab
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_blank()
        ) +
  NULL
p_lai_all

rm(dat_plot)



### Filter data ----------------------------------------------------------------
df_filt <- dat %>% 
  select(-Lat_IS, -Lon_IS, -Elevation, -Version) %>% 
  dplyr::filter(!down_flag %in% c(16, 32, 64)) %>% # remove flags with HIGH impact: Fixed pattern noise (64); Overexposure (32); Colour balance issues (16) 
  dplyr::filter(!down_flag %in% c(1, 2, 4, 8)) %>% # remove flags with MEDIUM impact: Variable illumination (8); Foreign objects present in FOV (i.e. equipment, water droplets on lens) (4); Less than 12 images available in ESU (2); Raw images not available, Joint Photographic Experts Group (JPEG) images processed instead (1)
  group_by(SITE_ID, PLOT_ID) %>%
  mutate(across(.cols = c(where(is.double), -contains("err"), -TIME_IS), .fns = ~ sd(.x, na.rm = T), .names = "{col}_std")) %>%
  ungroup() %>%
  dplyr::filter(LAI_Miller_up_err < LAI_Miller_up_std * 3 | is.na(LAI_Miller_up_std) | is.na(LAI_Miller_up_err),
                LAI_Warren_up_err < LAI_Warren_up_std * 3 | is.na(LAI_Warren_up_std) | is.na(LAI_Warren_up_err)
                ) %>% # filter outliers based on 3 x standard deviation
  glimpse()



### Aggregate and extract variables --------------------------------------------
# Calculate averages and maximum when appropriate
## 1) Aggregate to plot values ----
df_towerplot <- df_filt %>% 
  select(-TIME_IS) %>% 
  group_by(SITE_ID, PLOT_ID) %>%
  mutate(
    LAImax_Miller_up = quantile(LAI_Miller_up, probs = 0.9, na.rm = T), # take maximum height of each plot
    LAImax_Miller_up = if_else(condition = is.infinite(LAImax_Miller_up), true = NA_real_, false = LAImax_Miller_up), # convert -Inf to NA
    LAImax_Warren_up = quantile(LAI_Warren_up, probs = 0.9, na.rm = T), # take maximum height of each plot (shrubs)
    LAImax_Warren_up = if_else(condition = is.infinite(LAImax_Warren_up), true = NA_real_, false = LAImax_Warren_up), # convert -Inf to NA
    LAImax_Miller_down = quantile(LAI_Miller_down, probs = 0.9, na.rm = T), # take maximum height of each plot
    LAImax_Miller_down = if_else(condition = is.infinite(LAImax_Miller_down), true = NA_real_, false = LAImax_Miller_down), # convert -Inf to NA
    LAImax_Warren_down = quantile(LAI_Warren_down, probs = 0.9, na.rm = T), # take maximum height of each plot (shrubs)
    LAImax_Warren_down = if_else(condition = is.infinite(LAImax_Warren_down), true = NA_real_, false = LAImax_Warren_down) # convert -Inf to NA
  ) %>%
  summarise(
    across(.cols = c(where(is.double), -contains("max")), .fns = ~ mean(.x, na.rm = T)), # average
    across(.cols = c(where(is.double), -contains("max")), .fns = ~ if_else(.x, condition = is.nan(.x), true = NA_real_, false = .x)), # convert NaN to NA
    LAImax_Miller_up = unique(LAImax_Miller_up),
    LAImax_Warren_up = unique(LAImax_Warren_up),
    LAImax_Miller_down = unique(LAImax_Miller_down),
    LAImax_Warren_down = unique(LAImax_Warren_down),
    .groups = "drop"
  ) %>% 
  glimpse()


## 2) Aggregate to site values ----
df_site <- df_towerplot %>% 
  dplyr::select(-PLOT_ID) %>% 
  group_by(SITE_ID) %>%
  mutate(
    LAImax_Miller_up = quantile(LAImax_Miller_up, probs = 0.9, na.rm = T), # take maximum height of each plot
    LAImax_Miller_up = if_else(condition = is.infinite(LAImax_Miller_up), true = NA_real_, false = LAImax_Miller_up), # convert -Inf to NA
    LAImax_Warren_up = quantile(LAImax_Warren_up, probs = 0.9, na.rm = T), # take maximum height of each plot (shrubs)
    LAImax_Warren_up = if_else(condition = is.infinite(LAImax_Warren_up), true = NA_real_, false = LAImax_Warren_up), # convert -Inf to NA
    LAImax_Miller_down = quantile(LAI_Miller_down, probs = 0.9, na.rm = T), # take maximum height of each plot
    LAImax_Miller_down = if_else(condition = is.infinite(LAImax_Miller_down), true = NA_real_, false = LAImax_Miller_down), # convert -Inf to NA
    LAImax_Warren_down = quantile(LAI_Warren_down, probs = 0.9, na.rm = T), # take maximum height of each plot (shrubs)
    LAImax_Warren_down = if_else(condition = is.infinite(LAImax_Warren_down), true = NA_real_, false = LAImax_Warren_down) # convert -Inf to NA
  ) %>%
  summarise(
    across(.cols = c(where(is.double), -contains("max")), .fns = ~ mean(.x, na.rm = T)), # average
    across(.cols = c(where(is.double), -contains("max")), .fns = ~ if_else(.x, condition = is.nan(.x), true = NA_real_, false = .x)), # convert NaN to NA
    LAImax_Miller_up = unique(LAImax_Miller_up),
    LAImax_Warren_up = unique(LAImax_Warren_up),
    LAImax_Miller_down = unique(LAImax_Miller_down),
    LAImax_Warren_down = unique(LAImax_Warren_down),
    .groups = "drop"
  ) %>%
  glimpse()



### Select metric --------------------------------------------------------------
# df_out <- df_site %>% 
#   mutate(Miller_ratio = LAI_Miller_up / LAI_Miller_down,
#          Warren_ratio = LAI_Warren_up / LAI_Warren_down) %>%
#   glimpse()
# 
# df_out %>% summarise(Miller_ratio = mean(Miller_ratio, na.rm = T),
#                      Warren_ratio = mean(Warren_ratio, na.rm = T)
#                      )

df_out <- df_site %>%
  reframe(
    SITE_ID,
    LAImax = LAImax_Miller_up
  ) %>% 
  drop_na()



### Save -----------------------------------------------------------------------
if (savedata) {
  # Plot of LAI metrics
  ggplot2::ggsave(filename = "LAI_metrics.jpg", plot = p_lai_all, device = "jpeg",
                  path = "results/timeseries/", width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
  # NEON LAI
  write_csv(df_site, "data/input/NEON/LAI/LAI_GBOV_NEON_extracted.csv")
  write_csv(df_out, "data/input/NEON/LAI/LAImax_GBOV_NEON_extracted.csv")
}