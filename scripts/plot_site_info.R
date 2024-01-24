#### PLOT NEON SITE cut-outs at max NDVI

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Measure script run time
library(tictoc)
tic("Script run time")



### Options --------------------------------------------------------------------
product <- "2A"   # Sentinel 2 product level (2A or 1C)
radius <- 300     # radius of buffer around tower

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
vers_out <- "v06"
save_string <- glue::glue("{radius}m") # for different scenarios - - -  {product}_{radius}m_Avail{avail_rel}-{avail_abs}_noNegNDVI_OutlierRemX{n_removal}_DatElong{elong}x2_



### Utilities ------------------------------------------------------------------
## Packages
library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
library(lubridate)    # tidy date manipulation
library(patchwork)    # combine and arrange plots
library(plotbiomes)   # plot Whittaker climate space plot
library(readr)        # tidy read/save
library(rlang)        # quote/unguote
library(scales)       # package for rescaling
library(sf)           # shapefiles
library(stars)        # handle raster
library(stringr)      # manipulate strings
library(terra)        # raster manipulation
library(tidyr)        # reorganize tibbles
library(tictoc)       # time control

## Functions
# source("scripts/functions/plot_site_info.R") # outdated function to plot some site information
source("scripts/functions/min_max_norm.R")
source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("data/inter/data_efps_clim_struct_biodiv_{vers_out}.csv"), show_col_types = F) %>% 
  left_join(read_csv("data/input/NEON/neon_sites.csv", show_col_types = F), by = "SITE_ID") %>% 
  dplyr::select(-VEG_CLASS) %>% 
  relocate(latitude, longitude, .after = IGBP) %>% 
  glimpse()



### Sites ----------------------------------------------------------------------
# Single site data loaded within the loop
SITES <- dat %>% dplyr::select(SITE_ID)


### Plot -----------------------------------------------------------------------
for (ii in 1:nrow(SITES)) {
  tryCatch({
    ### Data for iith site -----------------------------------------------------
    dat_ii <- dat %>% dplyr::slice(ii)
    
    
    
    ### Load NDVI cut-outs -------------------------------------------------------
    cat("\n=== === === === === Plotting information for site", dat_ii$SITE_ID, "=== === === === ===\n")
    
    ## NDVI
    NDVIrast <- terra::rast(x = glue::glue("data/input/Sentinel2/NDVI/S2_{product}_NDVI_{dat_ii$SITE_ID}.tif"))
    # NDVIstars <- read_stars(glue::glue("data/input/Sentinel2/NDVI/S2_{product}_NDVI_{dat_ii$SITE_ID}.tif")) %>%
    #   setNames("NDVI") %>%
    #   st_set_dimensions(names = c("longitude", "latitude", "timestamp"))
    
    ## Date
    DOYrast <- terra::rast(glue::glue("data/input/Sentinel2/NDVI/S2_{product}_DOY_{dat_ii$SITE_ID}.tif"))
    
    YEARrast <- terra::rast(glue::glue("data/input/Sentinel2/NDVI/S2_{product}_YEAR_{dat_ii$SITE_ID}.tif"))
    
    
    
    ### Convert dates ------------------------------------------------------------
    ## Sentinel 2
    DOYvec <- DOYrast[1,1] %>% unlist() %>% unname()    # day of year vector
    YEARvec <- YEARrast[1,1] %>% unlist() %>% unname()  # year vector
    DATES <- strptime(paste(YEARvec, DOYvec), format = "%Y %j") %>% # dates vector
      as.character() %>% as.POSIXct()
    
    
    ## Clean memory
    rm(DOYrast, YEARrast)#, SNOWdoy, SNOWyear) # remove unnecessary data
    gc() # clean memory usage
    
    
    
    ### Region of interest -------------------------------------------------------
    circle <- sf::st_buffer(x = sf::st_sfc(sf::st_point(x = c(dat_ii$longitude, dat_ii$latitude)), crs = "EPSG:4326"), dist = radius) # circular region of interest of radius 'radius'
    
    
    
    ### Cut-out ------------------------------------------------------------------
    circle_vec <- terra::vect(circle) # convert to SpatVector
    
    dates <- DATES %>% tibble::as_tibble_col(column_name = "date") %>% # convert date vector to dataframe
      mutate(cell_id = seq(from = 1, to = length(DATES))) # add ID for joining
    
    
    
    ## Crop data region
    ndvi_dat <- terra::crop(NDVIrast, circle_vec, mask = T) %>% # select cut-out region
      terra::as.data.frame(cells = T, xy = T, na.rm = F) %>%
      tidyr::pivot_longer(cols = !c(cell, x, y), names_to = "name", values_to = "NDVIs2") %>%
      dplyr::rename(longitude = x, latitude = y) %>%
      group_by(name) %>%
      mutate(cell_id = if_else(str_detect(name, "S2_"),
                               true = str_extract(name, "[:digit:]+$") %>% as.integer(), # names of bands in the form "S2_1C_NDVI_AU-Wom_1"
                               false = cur_group_id() # names of bands in the form "20160119T104352_20160119T104347_T31UGR_20160119T104352_20160119T104347_T31UGR_ndvi"
      )
      ) %>%
      left_join(dates, by = "cell_id") %>% # add dates
      ungroup() %>%
      dplyr::select(-name) %>%
      mutate(DOY = lubridate::yday(date) %>% as.integer(), # add DOY
             YEAR = lubridate::year(date) %>% as.integer(), # add YEAR
             NDVIs2 = ifelse(is.nan(NDVIs2), NA_real_, NDVIs2), # convert NaN to NA
             cell = cell %>% as.integer() # convert to integer to save memory
      )
    
    # Remove pixels with no data (i.e. outside circle), since terra::crop keeps rectangular shape
    ndvi_dat <- ndvi_dat %>%
      group_by(cell) %>% # by pixel
      mutate(non_na = sum(!is.na(NDVIs2))) %>% # count non-NA entries
      ungroup() %>%
      filter(non_na > 0) %>% # exclude pixels that have no data (corners outside circle)
      select(-non_na)
    
    
    ## Condition to skip site if no data is available
    if (nrow(ndvi_dat) == 0) {
      warning(paste("Site", dat_ii$SITE_ID, "was skipped because of empty Sentinel 2 cut-outs."))
      return(err_output)
      next
    }
    
    ## Clean memory
    rm(NDVIrast)
    gc()
    
    
    ### Plotting data and options ----------------------------------------------
    ## Point specs ----
    point_border_color <- "white"
    point_fill_color <- "#EE4500"
    point_shape <- 21
    small_point_size <- 3
    main_point_size <- 6
    
    
    
    ## NDVI cut-outs ----
    ## Data
    set.seed(43824392)
    idt <- runif(42, min = 1, max = length(DATES)) %>% as.integer() %>% sort()
    dat_plot <- ndvi_dat %>%
      dplyr::filter(cell_id %in% idt) %>% # randomly subset timestamps
      dplyr::select(-cell_id, -cell) %>% 
      as.data.frame() %>%
      stars::st_as_stars(dims = c("longitude", "latitude", "date")) # convert to stars object for plotting
    
    ## Quote
    t_sym <- rlang::sym("date")
    
    ## Labels
    date_labs <- DATES[idt] %>% as.character()
    names(date_labs) <- idt %>% as.character()
    
    ## Plot
    p_ndvi <- ggplot() +
      stars::geom_stars(data = dat_plot) + # plot random subsets
      coord_equal() +
      facet_wrap(. ~ eval_tidy(t_sym), # subplots of random timestamps
                 labeller = labeller(t_sym = date_labs),
                 nrow = 6, ncol = 7,
                 ) + # title date for each subplot
      scale_fill_viridis_c(option = "viridis",
                           breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                           na.value = "#E2A2BE") +
      geom_sf(aes(color = "Cut-out"), data = circle, show.legend = "point", inherit.aes = F,
              fill = NA, size = 10, linewidth = 1, shape = 21, stroke = 1.5) + # plot region of interest
      scale_color_discrete(type = "black") +
      labs(subtitle = "Random NDVI Timestamps", fill = "NDVI", colour = "") +
      theme_void() +
      # theme(legend.background = element_rect(fill = "transparent"),
      #       legend.box.background = element_rect(fill = "transparent"),
      #       panel.background = element_rect(fill = "transparent"),
      #       panel.grid.major = element_blank(),
      #       panel.grid.minor = element_blank(),
      #       plot.background = element_rect(fill = "transparent", color = NA)
      #       ) + # transparent background
      NULL
    
    
    ## NDVI max composite ----
    dat_plot <- ndvi_dat %>%
      group_by(cell) %>% 
      dplyr::filter(NDVIs2 == max(NDVIs2, na.rm = T)) %>% # filter peak used for Rao's Q calculations
      dplyr::filter(date == max(date)) %>% # keep last date in case of duplicate NDVI max values
      ungroup() %>% 
      dplyr::select(-cell_id, -cell, -date, -DOY, -YEAR) %>% 
      as.data.frame() %>%
      stars::st_as_stars(dims = c("longitude", "latitude")) # convert to stars object for plotting
    
    p_ndvi_max <- ggplot() +
      stars::geom_stars(data = dat_plot) + # plot random subsets
      coord_equal() +
      scale_fill_viridis_c(option = "viridis",
                           breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                           na.value = "#E2A2BE") +
      geom_sf(aes(color = "Cut-out"), data = circle, show.legend = "point", inherit.aes = F,
              fill = NA, size = 10, linewidth = 1, shape = 21, stroke = 1.5) + # plot region of interest
      scale_color_discrete(type = "black") +
      labs(subtitle = "Max of NDVI", fill = "NDVI", colour = "") +
      theme_void() +
      NULL
    
    
    ## NDVI timeseries ----
    dat_plot <- ndvi_dat %>%
      group_by(date) %>% 
      summarise(NDVI = mean(NDVIs2, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(NDVI = if_else(is.nan(NDVI), NA_real_, NDVI)) # convert NaN to NA
    
    p_ndvi_season <- dat_plot %>% 
      ggplot(aes(x = date, y = NDVI)) +
      geom_line(color = "gray50", linewidth = 0.75, na.rm = T) +
      geom_point(aes(fill = NDVI), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
      scale_fill_viridis_c(option = "viridis",
                           breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                           na.value = "#E2A2BE") +
      labs(subtitle = "NDVI Time-series", x = "", y = "NDVI") +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            legend.position = "none"
            ) +
      NULL
    
    
    ## Plot map ----
    p_map <- dat_ii %>% 
      ggplot2::ggplot(aes(longitude, latitude)) +
      ggplot2::borders("world", fill = "#5A7F95", alpha = 0.5, xlim = c(-160, -80), ylim = c(20, 70)) +  # world borders
      ggplot2::geom_point(color = point_fill_color, shape = 10, size = main_point_size, stroke = 1.5) + # points
      # ggplot2::scale_color_manual(values = CatCol_igbp) + # color palette
      ylab("") + xlab("") + # title
      # guides(color = guide_legend(title = "Rao's Q (NDVImax)")) + # legend titles
      theme_void() +
      NULL
    
    
    ## Temperature-Precipitation scatterplot ----
    # hull <- dat %>% slice(chull(x = Temp, y = Precip))
    p_climate <- dat %>%
      ggplot(aes(x = Temp, y = Precip)) +
      geom_polygon(data = plotbiomes::Whittaker_biomes,
                   aes(x = temp_c, y = precp_cm * 10, fill = biome), # add biome polygons
                   colour = "gray98", linewidth = 1 # adjust polygon borders
                   ) +
      scale_fill_manual(name   = "Whittaker biomes",
                        breaks = names(Ricklefs_colors),
                        labels = names(Ricklefs_colors),
                        values = Ricklefs_colors) + # fill the polygons with predefined colors
      # geom_polygon(data = hull, alpha = 0.5, fill = "gray75") + # add convex hulls of points
      geom_point(color = point_border_color, fill = "gray50", shape = point_shape, size = small_point_size, na.rm = T) +
      geom_point(data = dat_ii, aes(x = Temp, y = Precip), color = point_border_color, fill = point_fill_color, shape = 21, size = main_point_size, na.rm = T) +
      xlab("Temperature [°C]") + ylab("Precipitation [mm]") + # axis title
      theme_bw() +
      # theme(legend.position = "none") +
      NULL
    
    
    ## Ground biodiversity distribution ----
    # Point data for ii site
    dat_point <- dat_ii %>% select(shannon, simpson_diversity, bray_curtis) %>% 
      pivot_longer(cols = c(shannon, simpson_diversity, bray_curtis), names_to = "groundiv_name", values_to = "groundiv_value")
    
    # Rearranged data for multiple curves
    dat_plot <- dat %>%
      pivot_longer(cols = c(shannon, simpson_diversity, bray_curtis), names_to = "groundiv_name", values_to = "groundiv_value")
    
    
    ## Plot
    p_groundiv <- dat_plot %>% 
      ggplot(aes(x = groundiv_value, y = groundiv_name, color = groundiv_name)) +
      geom_boxplot(linewidth = 1, outlier.shape = 3, outlier.size = main_point_size, outlier.stroke = 1.5) +
      geom_jitter(height = 0.1, alpha = 0.5, color = "gray50", size = small_point_size, show.legend = F) +
      # stat_density(data = dat_plot, aes(x = groundiv_value, color = groundiv_name), linewidth = 1, geom = "line", position = "identity", show.legend = T) +
      scale_color_manual(values = Five_colorblind[2:4]) +
      guides(color = guide_legend(title = "Ground biodiversity", reverse = T)) +
      geom_point(data = dat_point, # add point for shannon index
                 color = point_border_color, fill = point_fill_color, shape = point_shape, size = main_point_size, na.rm = T) +
      xlab("") + ylab("") + # axis title
      theme_bw() +
      theme(axis.text.y = element_blank(),  # remove y axis labels
            axis.ticks.y = element_blank()  # remove y axis ticks
            ) +
      NULL
    
    ## Clean memory
    rm(dat_point); gc()
      
    
    
    ## GPPsat-RaoQ scatterplots ----
    # lm0 <- dat %>% lm(formula = "GPPsat ~ RaoQ_S2_median")
    # dat_plot <- dat %>%
      # pivot_longer(cols = c(RaoQ_NDVI_S2_median, RaoQ_S2_median), names_to = "satdiv_name", values_to = "satdiv_value") %>% 
      # mutate(satdiv_name = factor(satdiv_name, labels = c("RaoQ[NDVImax]", "RaoQ[S2]")))
    
    # dat_ii_plot <- dat_ii %>% 
      # pivot_longer(cols = c(RaoQ_NDVI_S2_median, RaoQ_S2_median), names_to = "satdiv_name", values_to = "satdiv_value") %>% 
      # mutate(satdiv_name = factor(satdiv_name, labels = c("RaoQ[NDVImax]", "RaoQ[S2]")))
    
    # Bands ----
    m1 <- dat %>% drop_na(GPPsat, RaoQ_S2_median) %>% lm(formula = "GPPsat ~ RaoQ_S2_median")
    p_scatter1 <- dat %>% 
      ggplot(aes(x = RaoQ_S2_median, y = GPPsat)) +
      # facet_grid(rows = "satdiv_name", scales = "free_x", labeller = label_parsed) +
      geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
      geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
      scale_fill_discrete(type = CatCol_igbp, guide = guide_legend(title = "IGBP class")) +
      geom_point(data = dat_ii, color = point_border_color, fill = point_fill_color, shape = point_shape, size = main_point_size, na.rm = T) +
      labs(caption = glue::glue(
        "y = {round(m1$coefficients[2], digits = 2)}x + {round(m1$coefficients[1], digits = 2)};   R^2 = {round(summary(m1)$r.squared, digits = 3) * 100}%;   n = {dat %>% drop_na(GPPsat, RaoQ_S2_median) %>% nrow()}")
      ) +
      xlab("RaoQ from all bands") + ylab("GPPsat") + # axis title
      theme_bw() +
      # theme(strip.background = element_rect(fill = NA)) +
      NULL
    
    
    # NDVI ----
    m1 <- dat %>% drop_na(GPPsat, RaoQ_NDVI_S2_median) %>% lm(formula = "GPPsat ~ RaoQ_NDVI_S2_median")
    p_scatter2 <- dat %>% 
      ggplot(aes(x = RaoQ_NDVI_S2_median, y = GPPsat)) +
      geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
      geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
      scale_fill_discrete(type = CatCol_igbp, guide = guide_legend(title = "IGBP class")) +
      geom_point(data = dat_ii, color = point_border_color, fill = point_fill_color, shape = point_shape, size = main_point_size, na.rm = T) +
      labs(caption = glue::glue(
        "y = {round(m1$coefficients[2], digits = 2)}x + {round(m1$coefficients[1], digits = 2)};   R^2 = {round(summary(m1)$r.squared, digits = 3) * 100}%;   n = {dat %>% drop_na(GPPsat, RaoQ_NDVI_S2_median) %>% nrow()}")
      ) +
      xlab("RaoQ from NDVI") + ylab("GPPsat") + # axis title
      theme_bw() +
      NULL
    
    
    # NIRv ----
    m1 <- dat %>% drop_na(GPPsat, RaoQ_NIRv_S2_median) %>% lm(formula = "GPPsat ~ RaoQ_NIRv_S2_median")
    p_scatter3 <- dat %>% 
      ggplot(aes(x = RaoQ_NIRv_S2_median, y = GPPsat)) +
      geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
      geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
      scale_fill_discrete(type = CatCol_igbp, guide = guide_legend(title = "IGBP class")) +
      geom_point(data = dat_ii, color = point_border_color, fill = point_fill_color, shape = point_shape, size = main_point_size, na.rm = T) +
      labs(caption = glue::glue(
        "y = {round(m1$coefficients[2], digits = 2)}x + {round(m1$coefficients[1], digits = 2)};   R^2 = {round(summary(m1)$r.squared, digits = 3) * 100}%;   n = {dat %>% drop_na(GPPsat, RaoQ_NIRv_S2_median) %>% nrow()}")
      ) +
      xlab("RaoQ from NIRv") + ylab("GPPsat") + # axis title
      theme_bw() +
      NULL
    
    rm(m1) # clean memory
    
    
    ## Ground-div-RaoQ scatterplot ----
    lm0 <- dat %>% drop_na(n_spp, RaoQ_S2_median) %>% lm(formula = "n_spp ~ RaoQ_S2_median")
    p_scatter_ground <- dat %>%
      ggplot(aes(x = n_spp, y = RaoQ_S2_median)) +
      geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
      geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
      scale_fill_discrete(type = CatCol_igbp, guide = guide_legend(title = "IGBP class")) +
      geom_point(data = dat_ii, color = point_border_color, fill = point_fill_color, shape = point_shape, size = main_point_size, na.rm = T) +
      labs(caption = glue::glue("y = {round(lm0$coefficients[2], digits = 2)}x + {round(lm0$coefficients[1], digits = 2)};   R^2 = {round(summary(lm0)$r.squared, digits = 3) * 100}%;   n = {dat %>% drop_na(GPPsat, RaoQ_S2_median) %>% nrow()}")) +
      xlab("Species richness") + ylab("Rao's Q on S2 bands") + # axis title
      theme_bw() +
      NULL
    
    rm(lm0) # clean memory
    
    
    
    ### Correlation matrix ---------------------------------------------------------
    text_color <- "gray25"
    corr_mat <- dat %>%
      dplyr::select(-SITE_ID, -IGBP, -latitude, -longitude) %>% # remove non-numeric
      dplyr::select(GPPsat, NEP95, Gsmax, CUEeco90, WUE,
                    n_spp, shannon, simpson_diversity, bray_curtis,
                    RaoQ_S2_median, RaoQ_NDVI_S2_median, RaoQ_NIRv_S2_median,
                    NDVImax
                    ) %>% # include essentials
      mutate(across(.cols = where(is.double), .fns = min_max_norm)) %>% # normalize
      cor(use = "complete.obs", method = "spearman")
    
    # Plot
    p_corr <- corr_mat %>%
      ggcorrplot::ggcorrplot(method = "square", ggtheme = "theme_classic", show.diag = F,
                             type = "full", colors = RColorBrewer::brewer.pal(5, "RdBu")[c(1, 3, 5)],
                             # p.mat = NULL, sig.level = 0.05, insig = "pch", # for p-values (need to feed matrix of p-values to 'p.mat')
                             lab = F, lab_col = text_color, lab_size = 150 / nrow(dat_norm)
      ) +
      # Visuals
      scale_x_discrete(position = "top") +
      theme_bw() +
      theme(
        axis.text.x  = element_blank(),
        # axis.text.y  = element_text(color = text_color, size = 22),
        axis.ticks   = element_blank(),
        axis.title = element_blank(),
        # legend.box.spacing = margin(0, 0, 0, 1, unit = "cm"), # box distance from plot panel
        # legend.key.height = unit(2.5, "cm"), # height of legend bar
        # legend.key.width = unit(1.5, "cm"),  # width of legend bar
        # legend.text  = element_text(color = text_color, size = 24),
        legend.title = element_blank(),
        # panel.background = element_blank(),
        # panel.grid   = element_line(color = "gray75")
      ) +
      # theme( # transparent background
      #   panel.background  = element_rect(fill = 'transparent'), # transparent panel bg
      #   plot.background   = element_rect(fill = 'transparent', color = NA), # transparent plot bg
      #   # panel.grid.major = element_blank(), # remove major gridlines
      #   panel.grid.minor  = element_blank(), # remove minor gridlines
      #   legend.background = element_rect(fill = 'transparent'), # transparent legend bg
      #   legend.box.background = element_rect(colour = 'transparent', fill = 'transparent') # transparent legend panel and legend box
      # ) +
      NULL
    
    if (savedata) {
      ggsave(glue::glue("/spearman_{vers_out}.jpg"),
             plot = p_corr, device = "jpeg", path = "results/metrics_corr",
             width = 400, height = 300, units = "mm", dpi = 300)
    }
    
    
    
    ### Combine plots (patchwork) ----------------------------------------------
    p_right <- (p_ndvi / (p_ndvi_season | p_corr)) + plot_layout(heights = c(1.618, 1))
    p_centre <- (p_scatter1 / p_scatter2 / p_scatter3) + plot_layout(guides = 'collect')
    p_left <- (p_map / p_climate / p_groundiv) + plot_layout(heights = c(1, 1, 1))
    
    ## Site information card
    p_final <- (p_right | p_centre | p_left) +
      plot_layout(widths = c(1.618, 1, 1)) +
      # plot_layout(guides = 'collect') +
      plot_annotation(title = glue::glue("Site {dat_ii$SITE_ID} ({dat_ii$IGBP})"),
                      tag_levels = "a") &
      theme(title = element_text(size = 24, face = "bold"),
            text = element_text(size = 16),
            legend.key.size = unit(10, "mm"),
            plot.margin = unit(c(0, 0, 0, 0), "cm") # remove margins around individual plots
            )
    p_final
    
    
    
    ### Save -------------------------------------------------------------------
    if (savedata == T) {
      ggsave(filename = glue::glue("results/S2_NDVI/site_showcase_{dat_ii$SITE_ID}_{save_string}_{vers_out}.jpg"),
             plot = p_final, device = "jpeg",
             width = 508, height = 285.75, units = "mm", dpi = 300)
    }
    
    
    ## Clean memory ----
    rm(dat_plot) # remove unnecessary data
    gc() # clean memory usage
    
    
  }, error = function(err) {
    warning(paste0("Unforeseen error, skipping plotting of site ", as.character(SITES[ii,]), "."))
  }
  ) # end tryCatch
} # end of loop to plot each site



### End ------------------------------------------------------------------------
toc()
print("End of script.")