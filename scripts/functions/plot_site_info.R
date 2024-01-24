#### Function 
plot_site_info <- function(
    data = NULL, site = "SITE_ID", longitude = "longitude", latitude = "latitude",
    # raoq = "RaoQ_S2ndvimax", gppsat = "GPPsat",
    future_env = list(product, radius, savedata, save_string),
    transparent_plots = F
) {
  ### Utilities ----------------------------------------------------------------
  require(sf)     # shapefiles
  require(stars)  # raster manipulation (works better for plotting)
  require(terra)  # raster manipulation
  
  
  
  #### Function input ####
  ### Arguments to values ----------------------------------------------------
  # site <- data %>% dplyr::pull(.data[[site]])
  # igbp <- data %>% dplyr::pull(IGBP)
  # longitude = data %>% dplyr::pull(.data[[longitude]])
  # latitude = data %>% dplyr::pull(.data[[latitude]])
  # raoq <- data %>% dplyr::pull(.data[[raoq]])
  # ggpsat <- data %>% dplyr::pull(.data[[gppsat]])
  site <- "ABBY" # for testing
  igbp <- "ENF" # for testing
  longitude <- -122.3303200 # for testing
  latitude <- 45.76244 # for testing
  
  product     <- future_env[[1]]
  radius      <- future_env[[2]]
  savedata    <- future_env[[3]]
  save_string <- future_env[[4]]
  
  rm(future_env)
  
  ## Output by errors ----
  err_output <- ggplot() # output when error is encountered
  
  
  #### Main function ####
  # tryCatch({
    ### Load data --------------------------------------------------------------
    cat("\n=== === === === === Plotting information for site", site, "=== === === === ===\n")
    
    ## NDVI
    NDVIrast <- rast(x = glue::glue("data/input/Sentinel2/NDVI/S2_{product}_NDVI_{site}.tif"))
    # NDVIstars <- read_stars(glue::glue("data/input/Sentinel2/NDVI/S2_{product}_NDVI_{site}.tif")) %>%
    #   setNames("NDVI") %>%
    #   st_set_dimensions(names = c("longitude", "latitude", "timestamp"))
    
    ## Date
    DOYrast <- rast(glue::glue("data/input/Sentinel2/NDVI/S2_{product}_DOY_{site}.tif"))

    YEARrast <- rast(glue::glue("data/input/Sentinel2/NDVI/S2_{product}_YEAR_{site}.tif"))
    
    
    
    ### Convert dates ----------------------------------------------------------
    ## Sentinel 2
    DOYvec <- DOYrast[1,1] %>% unlist() %>% unname()    # day of year vector
    YEARvec <- YEARrast[1,1] %>% unlist() %>% unname()  # year vector
    DATES <- strptime(paste(YEARvec, DOYvec), format = "%Y %j") %>% # dates vector
      as.character() %>% as.POSIXct()


    ## Clean memory
    rm(DOYrast, YEARrast)#, SNOWdoy, SNOWyear) # remove unnecessary data
    gc() # clean memory usage



    ### Region of interest -----------------------------------------------------
    circle <- sf::st_buffer(x = sf::st_sfc(sf::st_point(x = c(longitude, latitude)), crs = "EPSG:4326"), dist = radius) # circular region of interest of radius 'radius'
    
    
    
    ### Cut-out ----------------------------------------------------------------
    circle_vec <- terra::vect(circle) # convert to SpatVector

    dates <- DATES %>% tibble::as_tibble_col(column_name = "date") %>% # convert date vector to dataframe
      mutate(cell_id = seq(from = 1, to = length(DATES))) # add ID for joining



    ## Crop data region ----
    ## Sentinel 2
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

    
    ## Condition to skip site if no data is available ----
    if (nrow(ndvi_dat) == 0) {
      warning(paste("Site", site, "was skipped because of empty Sentinel 2 cut-outs."))
      return(err_output)
      next
    }

    ## Clean memory ----
    rm(NDVIrast)
    gc()


    ### Plot spatial -----------------------------------------------------------
    ## Data for plotting
    set.seed(43824392)
    idt <- runif(36, min = 1, max = length(DATES)) %>% as.integer() %>% sort()
    ndvi_plot <- ndvi_dat %>%
      dplyr::filter(cell_id %in% idt) %>% # randomly subset timestamps
      dplyr::select(-cell_id, -cell) %>%
      as.data.frame() %>%
      stars::st_as_stars(dims = c("longitude", "latitude", "date")) # convert to stars object for plotting
    
    
    ## Quote
    t_sym <- rlang::sym("date")

    
    ## Labels
    date_labs <- DATES[idt] %>% as.character()
    names(date_labs) <- idt %>% as.character()
    
    
    # ## Plotting colors
    # purple_colblind <- "#B22D6E"
    # gold_colblind <- "#FDB71E"
    # teal_colblind <- "#1EB8B3"
    # orange_colblind <- "#F15620"
    # myCol <- c("#B22D6E", "#F15620", "#FDB71E", "#73C03C", "#1EB8B3", "#3C368E")
    
    
    ## Plot NDVI cut-out ----
    p_ndvi <- ggplot() +
      stars::geom_stars(data = ndvi_plot) + # plot random subsets
      coord_equal() +
      facet_wrap(. ~ eval_tidy(t_sym), # subplots of random timestamps
                 labeller = labeller(t_sym = date_labs)) + # title date for each subplot
      scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "#E2A2BE") +
      geom_sf(aes(color = "region of interest"), data = circle, show.legend = "polygon", inherit.aes = F,
              fill = NA, size = 1) + # plot region of interest
      scale_color_discrete(type = "black") +
      #guides(colour = guide_legend(override.aes = list(shape = 21))) + # change legend shape (NOT WORKING) https://stackoverflow.com/questions/13456765/ggplot2-custom-legend-shapes
      labs(fill = "NDVI", colour = "") +
      theme_void() +
      # theme(legend.background = element_rect(fill = "transparent"),
      #       legend.box.background = element_rect(fill = "transparent"),
      #       panel.background = element_rect(fill = "transparent"),
      #       panel.grid.major = element_blank(),
      #       panel.grid.minor = element_blank(),
      #       plot.background = element_rect(fill = "transparent", color = NA)
      #       ) + # transparent background
      NULL
    
    
    ## Plot map ----
    p_map <- data %>% 
      ggplot2::ggplot(aes(longitude, latitude)) +
      ggplot2::borders("world", fill = "#5A7F95", alpha = 0.5, xlim = c(-160, -80), ylim = c(20, 70)) +  # world borders
      ggplot2::geom_point(color = "#EE4500", shape = 10, size = 14, stroke = 1.5) + # points
      # ggplot2::scale_color_manual(values = CatCol_igbp) + # color palette
      ylab("") + xlab("") + # title
      # guides(color = guide_legend(title = "Rao's Q (NDVImax)")) + # legend titles
      theme_classic() +
      NULL
    
    
    ## Plot scatterplots ----
    p_scatter <- ggplot() +
      geom_point() +
      NULL
    
    
    
    ## Save
    if (savedata == T) {
      ggsave(filename = glue::glue("results/S2_NDVI/site_showcase_{site}_{save_string}_{vers_out}.jpg"),
             plot = p_space, device = "jpeg",
             width = 400, height = 300, units = "mm", dpi = 300)
      # ggsave(filename = glue::glue("results/S2_NDVI/cutouts1_{site}_r{radius}m_{vers_out}.png"),
      #        plot = p_space, device = "jpeg", bg = "transparent",
      #        width = 400, height = 300, units = "mm", dpi = 300)
    }


    ## Clean memory
    rm(ndvi_plot) # remove unnecessary data
    gc() # clean memory usage



    # # ### Plot temporal ------------------------------------------------------------
    # # ## Data for plotting
    # # set.seed(6314)
    # # idx <- runif(36, min = 1, max = max(dat_cut$cell)) %>% as.integer()
    # # idx
    # # 
    # # ## Data
    # # ndvi_plot <- dat_cut %>%
    # #   dplyr::filter(cell %in% idx) # randomly subset pixel
    # # 
    # # ## Plot data extent
    # # dat_idx <- ndvi_plot %>% 
    # #   group_by(cell) %>% 
    # #   summarize(NDVImean = mean(NDVI, na.rm = T),
    # #             longitude = x %>% unique(),
    # #             latitude = y %>% unique()) %>%
    # #   dplyr::select(-cell) %>% 
    # #   as.data.frame() %>%
    # #   st_as_stars(dims = c("longitude", "latitude")) # convert to stars object for plotting
    # # 
    # # p_idx <- ggplot() +
    # #   stars::geom_stars(data = dat_idx) + # plot random subsets
    # #   scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white") +
    # #   geom_sf(data = circle, show.legend = "polygon", inherit.aes = F,
    # #           color = "black", fill = NA, size = 1) + # plot region of interest
    # #   geom_sf(data = point0, show.legend = "point", shape = 2, size = 7, stroke = 2) +
    # #   labs(title = glue::glue("Random pixels ({site}).")) +
    # #   theme_classic() +
    # #   theme(axis.text = element_text(size = 24),
    # #         axis.text.x = element_text(angle = 270),
    # #         title = element_text(size = 32)) +
    # #   NULL
    # # 
    # # ## Labels
    # # pix_labs <- paste0("pixel ", ndvi_plot$cell %>% unique())
    # # names(pix_labs) <- ndvi_plot$cell %>% unique()
    # # 
    # # ## Plot
    # # p_date <- plot_sat_time(data = ndvi_plot, timestamp = date, y = NDVI, labels = pix_labs)
    # # p_doy <- plot_sat_time(data = ndvi_plot, timestamp = doy, y = NDVI, labels = pix_labs)
    # # p_agg <- ndvi_plot %>% 
    # #   group_by(cell, doy) %>% 
    # #   mutate(NDVIdoy = mean(NDVI, na.rm = T)) %>% 
    # #   plot_sat_time(timestamp = doy, y = NDVIdoy, labels = pix_labs)
    # # 
    # # ## Save
    # # if (savedata == T) {
    # #   ggsave(filename = glue::glue("results/S2_NDVI/randpix4timeseries_{site}_r{radius}m_{vers_out}.jpg"),
    # #          plot = p_idx, device = "jpeg",
    # #          width = 400, height = 300, units = "mm", dpi = 300)
    # #   ggsave(filename = glue::glue("results/S2_NDVI/timeseries_date_{site}_r{radius}m_{vers_out}.jpg"),
    # #          plot = p_date, device = "jpeg",
    # #          width = 500, height = 281.25, units = "mm", dpi = 300)
    # #   ggsave(filename = glue::glue("results/S2_NDVI/timeseries_doy_{site}_r{radius}m_{vers_out}.jpg"),
    # #          plot = p_doy, device = "jpeg",
    # #          width = 500, height = 281.25, units = "mm", dpi = 300)
    # #   ggsave(filename = glue::glue("results/S2_NDVI/timeseries_mean_{site}_r{radius}m_{vers_out}.jpg"),
    # #          plot = p_agg, device = "jpeg",
    # #          width = 500, height = 281.25, units = "mm", dpi = 300)
    # # }
    # # 
    # # 
    # # 
    ### Output -----------------------------------------------------------------
    return(p_space)
    
    
    
    ### End of function --------------------------------------------------------
  # }, error = function(err) {
  #   warning(paste0("Unforeseen error in the calculation of phenodiv, skipping site ", site, "."))
  #   return(err_output)
  # }
  # ) # end tryCatch
} # end function

#### Debug ---------------------------------------------------------------------
debugonce(plot_site_info)