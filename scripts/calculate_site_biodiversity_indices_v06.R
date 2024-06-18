#### PROCESS SITE DATA AND CALCULATE BIODIVERSITY INDICES

### Authors: Ulisse Gomarasca
### Version History ------------------------------------------------------------
# v02, 20.07.2023:  filtered out distributed plots, kept only tower plots
# v03, 17.10.2023:  added NEON eventID as unique identifier for measurement campaigns; for ICOS, built EVENT_ID;
#                   included "unknown" species with unique IDs; normalized total cover above 100%;
#                   added option to filter out subplots with low total cover;
# v04, 18.10.2023:  included species presence at 10 m2 subplots (complementary to 1 m2 subplots); low total cover NOT implemented
# v05, 31.10.2023:  included species presence at 100 m2 subplots
# v06, 31.10.2023:  excluded step where 90% dominant species were excluded



### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
igbp_subset <- "all" # should sites be subsetted based on vegetation type? ("all", or "forest")

# Data settings
savedata = as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- "v06"



### Utilities ------------------------------------------------------------------
library(betapart)
library(dplyr)
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)
library(lubridate) # dates
# library(neonUtilities)
# library(raster)
library(purrr)
library(readr)
library(readxl)
library(reshape)
library(stringr)
library(tictoc)
library(tidyr)
library(vegan)

## Own utilities
source("scripts/functions/min_max_norm.R")



### Data -----------------------------------------------------------------------
## ICOS ----
# From BIF
dat_icos <- map(.x = list.files(path = "data/input/ICOS/unzipped_all/", pattern = "ANCILLARY", full.names = T),
                .f = read_csv, show_col_types = F
                ) %>% 
  bind_rows() %>% 
  glimpse()

## ICOS species data at subplots
# if (igbp_subset == "all") {
#   grass_icos <- read_xlsx("data/input/ICOS/species_at_subplots/grasslands.xlsx", sheet = 2) # grasslands
#   mires_icos <- read_xlsx("data/input/ICOS/species_at_subplots/mires.xlsx", sheet = 2, col_types = c(
#     "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "guess", "numeric", "numeric"
#   )) # mires
# }
# forest_icos <- read_xlsx("data/input/ICOS/species_at_subplots/GRP_TREE - 20220804.xlsx", sheet = 1) # forests


## NEON species data at subplots ----
# Set global option to NOT convert all character variables to factors
options(stringsAsFactors = F)

# ## Unzip manually downloaded data (only first time)
# # Modify the file path to match the path to your zip file
# stackByTable("data/NEON/NEON_presence-cover-plant/NEON_presence-cover-plant.zip") # un-zips in same directory and stacks sites into single tables

## Load stacked data
dat_neon1m <- read_csv("data/input/NEON/NEON_presence-cover-plant/stackedFiles/div_1m2Data.csv", show_col_types = F) %>% glimpse()
dat_neon10m <- read_csv("data/input/NEON/NEON_presence-cover-plant/stackedFiles/div_10m2Data100m2Data.csv", show_col_types = F) %>% glimpse()

# Load metadata
meta_neon <- read_csv("data/input/NEON/neon_sites.csv", show_col_types = F) %>% glimpse()



### Process data ---------------------------------------------------------------
# The following variables need to be present:
# SITE_ID: chr
# PLOT_ID: chr
# COORDS?
# DATE: datetime
# SPECIES: chr
# PRESENCE: T/F
# COVER: double [0-1]
# OTHER: double
# SOURCE: chr (reference/DOI/PI)

# ## ICOS ----
# # Cover should be in range 0-100 (=> convert to 0-1)
# dat_icos <- dat_icos %>% # output <- your BADM tibble
#   dplyr::filter(VARIABLE != "SITE_ID") %>% # exclude redundant site name variable
#   dplyr::filter(VARIABLE_GROUP == "GRP_SPP") %>% # filter species information; NB: SPP_U DOES NOT CONTAIN ANY SPECIES NAME
#   tidyr::pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) %>% # re-structure data to tidy format, i.e. every column = one variable
#   dplyr::rename(DATE = SPP_DATE, SPECIES = SPP_O, COVER = SPP_O_PERC) %>%
#   mutate(
#     COVER = if_else(is.na(SPECIES), NA_character_, COVER), # set cover to NA where no single species is given
#     SPECIES = if_else(is.na(SPECIES), # extract species names from comments => only true for DE-Gri
#                       str_extract_all(SPP_COMMENT,
#                                       "[:upper:]{1}[:lower:]+ [:lower:]+"),
#                       SPECIES %>% as.list())
#   ) %>% 
#   unnest(cols = SPECIES) %>% 
#   mutate(
#     SPECIES = if_else(is.na(SPECIES), # extract species names from vegetation type => only true for DE-Gri
#                       SPP_O_VEGTYPE,
#                       SPECIES),
#     DATE = if_else(condition = DATE %>% as.double() < 9999, # correct 'only-year' entries
#                    true = paste0(DATE, "0101"),
#                    false = DATE
#     ),
#     DATE = if_else(condition = DATE %>% as.double() > 99999999, # correct 'incl. hour' entries
#                    true = str_sub(DATE, start = 1, end = 8),
#                    false = DATE
#     ),
#     DATE = DATE %>% lubridate::ymd(),
#     DATE = if_else(is.na(DATE), SPP_DATE_END %>% lubridate::ymd(), DATE), # gap-fill with end-of-measurement date
#     DATE = if_else(is.na(DATE),
#                    SPP_O_PERC_DATE_END %>% str_sub(1, 8) %>% lubridate::ymd(),
#                    DATE), # gap-fill with end-of-cover-measurement date
#     PRESENCE = if_else(!is.na(SPECIES), T, F),
#     COVER = COVER %>% as.double(),
#     SOURCE = "ICOS BIF"
#   ) %>% 
#   dplyr::mutate(SPECIES = str_extract(SPECIES, "[:upper:]{1}[:lower:]+[:blank:]*[:lower:]*")) %>% 
#   dplyr::filter(SPECIES != "Poaceae") %>% # manually exclude DE-Gri entry (redundant to extracted species names from comments)
#   # group_by(SITE_ID) %>% # for counting entries per group
#   # mutate(n_entries = n(),
#   #        COVER = if_else(SITE_ID == "DE-Gri", 100/n_entries, COVER)) %>% # doesn't make sense to make assumption on homogeneous cover
#   # ungroup() %>% 
#   # dplyr::select(-n_entries) %>% 
#   dplyr::filter((COVER != Inf & COVER != 0) | is.na(COVER)) %>% # remove infinite and zero values
#   dplyr::filter(SPP_O_PERC_STATISTIC == "Mean") %>% # filter only mean values
#   dplyr::select(SITE_ID, SPECIES, DATE, PRESENCE, COVER, SOURCE) %>%
#   dplyr::mutate(EVENT_ID = paste(SITE_ID, DATE, sep = "_"), .after = SITE_ID) %>% # add EVENT_ID
#   unique() %>%
#   glimpse()
# 
# 
# ## Pre-process ICOS ----
# # Correct wrong entries (manual):
# # DE-Hai: from 7 to 65%                                       => /100
# # DE-Kli: 100% Triticum aestivum (rotation cropland?)         => /100
# # FI-Var: 100% Pinus sylvestris                               => /100
# # IT-Lsn: 30% Vitis vinifera x 2?? 73% U and 27% overstory??  => /100
# # IT-Tor: from 0.5% to 34%                                    => /100
# # SE-Deg: from 0.0005% to 52% ???                             => /100
# 
# dat_icos <- dat_icos %>% 
#   mutate( # correct wrong entries
#     COVER = if_else(SITE_ID %in% c("DE-Hai", "DE-Kli", "FI-Var", "IT-Lsn", "IT-Tor", "SE-Deg"),
#                     COVER / 100,
#                     COVER)
#   ) %>% 
#   group_by(SITE_ID, EVENT_ID, SPECIES) %>% 
#   add_count() %>% 
#   mutate(
#     SPECIES = if_else( # "save" duplicates with different cover (2 Empetrum nigrum at SE-Deg site) by adding a unique ID to the species name
#       condition = n == 1,
#       true = SPECIES,
#       false = paste0(SPECIES, row_number())
#     )
#   ) %>% 
#   ungroup() %>% 
#   select(-n) %>% 
#   glimpse()
# 
# 

## NEON: merge 1m2 + 10m2 + 100m2 ----
dat_neon_merged <- dat_neon1m %>% 
  mutate(subplotID = paste0(subplotID, "0")) %>% 
  full_join(
    dat_neon10m %>% # include data from 10 m2 subplots
      dplyr::filter(additionalSpecies == "Y" | is.na(scientificName)) # keep entries where additional species are recorded at the 10 m2 scale
  ) %>% 
  # dplyr::filter(str_detect(subplotID, pattern = "[:digit:]{2}(?=.)")) %>% # exclude 100 m2 plots (since many subplots are nested in 100 m2 => problem of attribution when grouping); also, we can just focus on the 1-10 m2 scale)
  arrange(siteID, plotID, subplotID, eventID) %>% 
  glimpse()


## Filter NEON ----
dat_neon_filt <- dat_neon_merged %>%
  dplyr::filter(plotType == "tower") %>% # keep measurement within tower airshed
  dplyr::filter(divDataType == "plantSpecies" | is.na(divDataType)
                # | otherVariables %in% c("lichen", "moss", "otherNonVascular", "fungi", "biocrustMoss", "biocrustLightCyanobacteria", "biocrustLichen", "biocrustDarkCyanobacteria")
                ) %>% # remove non-species information and non-vascular plants or others
  mutate( # treat unidentified/unknown species
    scientificName = if_else( # "save" unknown plants with description as name
      condition = scientificName != "Unknown plant",
      true = scientificName,
      false = morphospeciesID
      )
    ) %>% 
  # mutate(
  #   scientificName = if_else( # "save" artificial duplicates of non-identified (e.g. 2 Poaceae sp.) by adding unique ID to scientificName
  #     condition = n == 1,
  #     true = scientificName,
  #     false = paste0(scientificName, row_number())
  #     )
  #   ) %>% 
  # ungroup() %>% 
  # select(-n) %>% 
  ## NB: Not worth it, since we can not differentiate artificial duplicates from actual duplicates and we would introduce higher than normal cover
  glimpse()


## Extract info NEON ----
dat_neon <- dat_neon_filt %>% 
  dplyr::rename(SITE_ID = siteID,
                PLOT_ID = plotID,
                SUBPLOT_ID = subplotID,
                EVENT_ID = eventID,
                DATE = endDate,
                SPECIES = scientificName,
                PRESENCE = targetTaxaPresent,
                COVER = percentCover
  ) %>% 
  mutate(
    #SPECIES = if_else(!is.na(SPECIES), SPECIES, otherVariables), # if non-vascular species are included
    PRESENCE = if_else(PRESENCE == "Y" | COVER > 0, TRUE, FALSE), # populate presence variable ## CHECKED: filtering conditions are correct
    COVER = COVER / 100, # transform to range 0-1
    SOURCE = "NEON (https://data.neonscience.org/data-products/DP1.10058.001)"
  ) %>% 
  dplyr::filter((PRESENCE == T & COVER > 0) | is.na(COVER)) %>% # exclude empty data; keep COVER = NA to count species at 10 m2
  dplyr::filter(!is.na(SPECIES)) %>% # exclude empty species
  dplyr::select(SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID, DATE, SPECIES, PRESENCE, COVER, SOURCE) %>%
  glimpse()


## NEON species data availability ----
neon_avail <- dat_neon %>% 
  group_by(SITE_ID) %>%
  summarise(
    Spp_start = min(DATE, na.rm = T),
    Spp_end = max(DATE, na.rm = T)
  ) %>% 
  ungroup()

## Save
if (savedata) {
  write_csv(neon_avail, "data/input/NEON/NEON_presence-cover-plant/species_data_availability.csv")
}


### Combine data ---------------------------------------------------------------
dat <- bind_rows(dat_neon, dat_icos) %>% 
  unique() %>%
  arrange(SITE_ID) %>% 
  glimpse()


## Vector of sites ----
SITES <- dat %>% pull(SITE_ID) %>% unique() # vector of sites



### Average over dates (events) ------------------------------------------------
# This step is needed to simply get rid of duplicates, which are due to either
# actual duplicate entries (=> unique()), or un-identified species (=> mean()).
dat <- dat %>%
  group_by(SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID, SPECIES) %>%
  dplyr::filter(DATE == max(DATE, na.rm = T)) %>% # keep last date within measurement event for each species at each subplot (removes duplicates)
  summarise( # additionally, average repeated measurements with different values within measurement event for each species at each subplot (removes duplicates with different cover)
    DATE = unique(DATE), # works, even though it would be best to test for contrasting values
    COVER = mean(COVER, na.rm = T),
    PRESENCE = unique(PRESENCE), # works, even though it would be best to test for contrasting values
    SOURCE = unique(SOURCE), # works, even though it would be best to test for contrasting values
    .groups = "drop"
    ) %>%
  glimpse()



# ### Plot-check coverage values -------------------------------------------------
# dat %>% 
#   ggplot(aes(SITE_ID, COVER)) +
#   geom_point(na.rm = T) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 270)) +
#   NULL
#   
# 
#
# ### Filter 90% dominant species ------------------------------------------------
# dat <- dat %>% 
#   ## Calculate Total Cover within group
#   group_by(SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID) %>% 
#   mutate(TOTCOVER = sum(COVER, na.rm = T), # NB: sometimes above 100% possibly due to entry errors --> e.g. dplyr::filter(SITE_ID == "SE-Deg")
#          TOTCOVER = if_else(is.nan(TOTCOVER), NA_real_, TOTCOVER), # convert NaN to NA
#          .after = COVER) %>%
#   ## Rescale cover where total cover is above 100% ---
#   mutate(
#     COVER = if_else(
#       condition = TOTCOVER > 1,
#       true = COVER / TOTCOVER, # rescale => X : COVER = 1 : TOTCOVER => X = COVER / TOTCOVER
#       false = COVER
#     ),
#     COVER = if_else(is.nan(COVER), NA_real_, COVER), # convert NaN to NA
#   ) %>%
#   ## Re-calculate Total Cover within group
#   mutate(TOTCOVER = sum(COVER, na.rm = T), # NB: sometimes above 100% possibly due to entry errors --> e.g. dplyr::filter(SITE_ID == "SE-Deg")
#          TOTCOVER = if_else(is.nan(TOTCOVER), NA_real_, TOTCOVER), # convert NaN to NA
#          COVER90 = TOTCOVER * 0.90,
#          .after = COVER
#          ) %>% #filter(SITE_ID == "MOAB") %>% drop_na(COVER) %>% arrange(desc(TOTCOVER))
#     # # arrange(desc(TOTCOVER), SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID) %>% glimpse() %>% # check maximum total coverage
#     # # filter(!is.na(COVER)) %>% arrange(TOTCOVER, SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID) %>% glimpse() %>% # check minimum total coverage (other than 0)
#     # # arrange(COVER, SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID) %>% glimpse() %>% # check minimum cover
#   # ## Exclude subplots where total cover is low
#   # # not enough species reported may cause biases
#   # dplyr::filter(is.na(COVER) | TOTCOVER >= 0.5) %>% # NOT implemented because low cover might be due to bare soil
#   ## Filter based on 90% of total cover
#   # exclude species beyond 90% cumulative cover
#   arrange(SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID, desc(COVER)) %>% 
#   mutate(CUMCOVER = cumsum(COVER), .after = COVER90) %>% # calculate stepwise cumulative sums of cover within groups
#   ungroup() %>% 
#   mutate(
#     DOMINANCE = case_when( # keep species up to 90% cumulative coverage (unless first species is already above 90%)
#       COVER == CUMCOVER ~ T,
#       CUMCOVER <= COVER90 ~ T,
#       COVER != CUMCOVER & CUMCOVER > COVER90 ~ F),
#     .after = CUMCOVER
#   ) %>%
#   dplyr::filter(is.na(DOMINANCE) | DOMINANCE) %>%
#   glimpse()
# 
# 
#
### Calculate biodiversity indices ---------------------------------------------
# Cf. Plant Ecology by Schulze, p. 750
## Alpha diversity ----
# NB: whole-site estimates
dat_biodiv <- dat %>%
  # A) total number of species at any given time (each date)
  group_by(SITE_ID, EVENT_ID) %>%
  mutate(n_spp = length(unique(SPECIES))) %>%
  ungroup() %>% 
  dplyr::filter(!is.na(COVER)) %>% # now we can exclude cover = NA. We NEED to exclude NA because: sum(c(NA, NA), na.rm = T) = 0 !!!
  # B) indexes at every subplot at each date
  group_by(SITE_ID, PLOT_ID, SUBPLOT_ID, EVENT_ID) %>%
  mutate(shannon = -sum(COVER * log(COVER), na.rm = T), # Shannon (1948) (incl. n_spp, cover) cf. Spellerberg & Fedor, 2003 for controversy on index name/reference
         simpson_index = sum(COVER^2, na.rm = T), # Simpson's index (1949)
         simpson_reciprocal = 1 / simpson_index, # Simpson's reciprocal index
         simpson_diversity = 1 - simpson_index # Simpson's index of diversity
         ) %>%
  ungroup() %>% 
  # C0) average for plot?
  # C) average for site
  group_by(SITE_ID) %>%
  summarise(n_spp = mean(n_spp, na.rm = T), # mean number of species over time
         shannon = mean(shannon, na.rm = T), # Shannon index for whole site (mean)
         pielou_evenness = if_else(n_spp != 1, # avoid dividing by 0
                                   shannon / log(n_spp), # Pielou's evenness index for whole site (mean)
                                   NA_real_),
         simpson_index = mean(simpson_index, na.rm = T), # Simpson index
         simpson_reciprocal = mean(simpson_reciprocal, na.rm = T), # Simpson reciprocal index
         simpson_diversity = mean(simpson_diversity, na.rm = T) # Simpson diversity for whole site (mean)
         ) %>%
  ungroup() %>%
  glimpse()

# Plot
dat_biodiv %>% 
  ggplot(aes(SITE_ID, n_spp)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 270)) +
  NULL



## Beta diversity ----
# Bray-Curtis dissimilarity
dat_bray <- dat %>%
  mutate(PLOT_ID = if_else(!is.na(SUBPLOT_ID), paste0(PLOT_ID, SUBPLOT_ID), PLOT_ID)) %>% # merge plot & subplot names for unique IDs
  dplyr::select(SITE_ID, PLOT_ID, SPECIES, PRESENCE) %>%
  nest(plotXspecies = c(PLOT_ID, SPECIES, PRESENCE)) %>%
  mutate(plotXspecies = map(.x = plotXspecies, .f = cast, formula = PLOT_ID ~ SPECIES, value = "PRESENCE", fill = 0), # convert to community matrix (https://www.r-bloggers.com/2012/07/r-for-ecologists-creating-a-site-x-species-matrix/)
         plotXspecies = map(.x = plotXspecies, .f = as_tibble), # convert to tibble
         plotXspecies = map(.x = plotXspecies, .f = mutate, PLOT_ID = NULL), # remove row name column
         bray_curtis = map(.x = plotXspecies, .f = vegdist, method = "bray"), # calculate bray-curtis matrix
         bray_curtis = map(.x = bray_curtis, .f = mean), # average bray-curtis matrix
         bray_curtis = unlist(bray_curtis), # extract from list
         bray_curtis = if_else(is.nan(bray_curtis), NA_real_, bray_curtis), # convert NaN to NA
         # bray_turnover = purrr::map(.x = plotXspecies %>% unlist(), .f = beta.multi.abund) # balanced variation component of Bray-Curtis multiple-site dissimilarity
         ) %>%
  glimpse()

dat_biodiv <- dat_biodiv %>% 
  left_join(dat_bray %>% dplyr::select(SITE_ID, bray_curtis), by = "SITE_ID")



### Plot correlation matrix ----------------------------------------------------
text_color <- "gray25"
p_corr <- dat_biodiv %>%
  # filter(str_detect(SITE_ID, "[:upper:]{4}")) %>% # NEON sites
  # filter(str_detect(SITE_ID, "[:upper:]{2}-")) %>% # ICOS sites
  dplyr::select(-SITE_ID) %>%
  cor(use = "complete.obs", method = "spearman") %>%
  ggcorrplot::ggcorrplot(method = "square", ggtheme = "theme_classic", show.diag = F,
                         type = "upper", colors = RColorBrewer::brewer.pal(3, "RdBu"),
                         lab = T, lab_col = text_color, lab_size = 8,
                         tl.cex = 24) +
  theme(axis.text.x  = element_text(color = text_color, size = 24, angle = 45),
        axis.text.y  = element_text(color = text_color, size = 24),
        axis.ticks   = element_blank(),
        legend.box.spacing = margin(0, 0, 0, 1.5, unit = "cm"), # box distance from plot panel
        legend.key.height = unit(4.65, "cm"), # height of legend bar
        legend.key.width = unit(3, "cm"), # width of legend bar
        legend.text  = element_text(color = text_color, size = 24),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid   = element_line(color = "gray75")
  ) +
  theme( # transparent background
    panel.background  = element_rect(fill = 'transparent'), # transparent panel bg
    plot.background   = element_rect(fill = 'transparent', color = NA), # transparent plot bg
    # panel.grid.major = element_blank(), # remove major gridlines
    panel.grid.minor  = element_blank(), # remove minor gridlines
    legend.background = element_rect(fill = 'transparent'), # transparent legend bg
    legend.box.background = element_rect(colour = 'transparent', fill = 'transparent') # transparent legend panel and legend box
  )
# print(p_corr)



### Remove unnecessary metrics -------------------------------------------------
dat_biodiv <- dat_biodiv %>% 
  dplyr::select(-simpson_index, -simpson_reciprocal) %>%
  glimpse()



### Save -----------------------------------------------------------------------
if (savedata) {
  # list of sites for further analysis
  write_csv(dat_biodiv %>% dplyr::select(SITE_ID), glue::glue("data/input/site_list_{igbp_subset}_{vers_out}.csv"))
  # biodiversity indices
  write_csv(dat_biodiv, glue::glue("data/inter/biodiv_indices_{igbp_subset}_{vers_out}.csv"))
  # # plot of correlations
  # ggsave(glue::glue(glue::glue("/biodiv_indices_correlations_{igbp_subset}.jpg")), plot = p_corr, device = "jpeg", path = "results",
  #        width = 508, height = 285.75, units = "mm", dpi = 300) # 1920 x  1080 px resolution (16:9)
}



### Test different outputs based on filtering ----------------------------------
bind_rows(
  read_csv("data/inter/biodiv_indices_all_v06.csv", show_col_types = F) %>% mutate(version = "v05 included 100m2 + no 90% dominance filter"),
  read_csv("data/inter/biodiv_indices_all_v05.csv", show_col_types = F) %>% mutate(version = "v05 included 100m2"),
  read_csv("data/inter/biodiv_indices_all_v04.csv", show_col_types = F) %>% mutate(version = "v04 included 10m2"),
  read_csv("data/inter/biodiv_indices_all_v03.csv", show_col_types = F) %>% mutate(version = "v03 filtered low totcover"),
  read_csv("data/inter/biodiv_indices_all_v02.csv", show_col_types = F) %>% mutate(version = "v02 tower plots"),
  read_csv("data/inter/biodiv_indices_all_v01.csv", show_col_types = F) %>% mutate(version = "v01 all plots")
) %>%
  drop_na() %>% 
  mutate(across(.cols = where(is.numeric), .fns = min_max_norm)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>% 
  glimpse() %>% 
  ggplot(aes(x = variable, y = value, color = version)) +
  geom_boxplot() +
  xlab("") + ylab("") +
  theme_bw()