#### ALL Built up
# Housing units per WUI
if(!file.exists(file.path(dir_cleaned_wui_all_built_up_rds, 'all_cleaned_wui_built_up.rds'))) {
  ############## convert the GDB to GPKG and clean the points with no coordinates ############################
  gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  pblapply(gdbs,
           FUN = function(i, usa_shp, dir_raw_all_built_up_gpkg) {
             require(sf)
             require(tidyverse)
             
             outname <- basename(i) %>%
               gsub('.gdb', '.gpkg', .)
             
             if(!file.exists(file.path(dir_raw_all_built_up_gpkg, outname))) {
               
               layers <- tibble::data_frame(name = sf::st_layers(i)$name)
               
               state_ztrax <- apply(unique(layers), 1, 
                                    function(j) {
                                      sf::st_read(i, layer = j) %>%
                                        sf::st_transform(sf::st_crs(usa_shp)) %>%
                                        dplyr::filter(geom_wkt != 'POINT(0 0)')
                                    })
               
               final_ztrax <- do.call(rbind, state_ztrax) 
               
               sf::st_write(final_ztrax, file.path(dir_raw_all_built_up_gpkg, outname))
             }
           },
           usa_shp = usa_shp,
           dir_raw_all_built_up_gpkg = dir_raw_all_built_up_gpkg,
           cl = cl)
  
  stopCluster(cl)
  
  ############## spatial join the wui dataframe to the zillow data ############################
  
  # find the number of built-up units and build-up area by census block group and built year
  gpkgs <- list.files(dir_raw_all_built_up_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/16)
  registerDoParallel(cl)
  
  ztrax_all_built_up <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
  
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 18) %>%
      unlist
    
    if(!file.exists(file.path(dir_wui_all_built_up_rds, paste0(filename, '_ztrax_all_built_up_wui_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        filter(YearBuilt != 0) %>%
        st_join(., wui, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(blk10, yearbuilt) %>%
        summarize(all_build_up_count = n(),
                  all_build_up_intensity_sqm = sum(BdAreaSqFt)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_wui_all_built_up_rds,  paste0(filename, '_ztrax_all_built_up_wui_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax residential dataframes ############################
  rdss <- list.files(dir_wui_all_built_up_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_wui_decades <- pblapply(rdss,
                                  FUN = function(x, dir_cleaned_wui_all_built_up_rds) {
                                    require(tidyverse)
                                    
                                    filename <- strsplit(x, "\\.|/|_") %>%
                                      lapply(`[`, 9) %>%
                                      unlist
                                    
                                    if(!file.exists(file.path(dir_cleaned_wui_all_built_up_rds, paste0(filename, '_cleaned_wui_all_built_up.rds')))) {
                                      imported <- read_rds(x) 
                                      
                                      blk_grps <- imported %>%
                                        group_by(blk10) %>%
                                        summarise() %>%
                                        ungroup() %>%
                                        droplevels() %>%
                                        slice(rep(1:n(), each = 4)) %>%
                                        mutate(decade = as.factor(rep(c(1990, 2000, 2010, 2015), times = length(unique(blk10)))),
                                               decade_built = as.factor(decade))
                                      
                                      cleaned_wui_decades <- imported %>%
                                        filter(yearbuilt != 0) %>%
                                        group_by(blk10, yearbuilt) %>%
                                        summarise(all_build_up_count = sum(all_build_up_count),
                                                  all_build_up_intensity_sqm = sum(all_build_up_intensity_sqm)) %>%
                                        mutate() %>%
                                        mutate(all_build_up_count = cumsum(all_build_up_count),
                                               all_build_up_intensity_sqm = cumsum(all_build_up_intensity_sqm),
                                               decade = case_when(
                                                 yearbuilt > 1800 & yearbuilt <= 1990 ~ 1990,
                                                 yearbuilt > 1990 & yearbuilt <= 2000 ~ 2000,
                                                 yearbuilt > 2000 & yearbuilt <= 2010 ~ 2010,
                                                 TRUE ~ 2015),
                                               decade = as.factor(decade)) %>%
                                        group_by(blk10, decade) %>%
                                        summarise(all_build_up_count = max(all_build_up_count),
                                                  all_build_up_intensity_sqm = max(all_build_up_intensity_sqm)) %>%
                                        ungroup() %>%
                                        full_join(., blk_grps, by = c('decade', 'blk10')) %>%
                                        droplevels() %>%
                                        mutate(decade = as.factor(decade),
                                               blk10 = as.factor(blk10),
                                               decade_built = as.integer(decade_built)) %>%
                                        arrange(desc(blk10, decade_built)) %>%
                                        fill(everything())
                                      
                                      cleaned_wui_decades %>%
                                        write_rds(., file.path(dir_cleaned_wui_all_built_up_rds, paste0(filename, '_cleaned_wui_all_built_up.rds')))
                                    }
                                  },
                                  dir_cleaned_wui_all_built_up_rds = dir_cleaned_wui_all_built_up_rds,
                                  cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_wui_decades <- do.call(rbind, cleaned_wui_decades) %>%
    na.omit()  %>%
    mutate(year = decade) %>%
    dplyr::select(blk10, year, all_build_up_count, all_build_up_intensity_sqm)
  
  cleaned_wui_decades %>%
    write_rds(., file.path(dir_cleaned_wui_all_built_up_rds, 'all_cleaned_wui_all_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_wui_decades <- read_rds(file.path(dir_cleaned_wui_all_built_up_rds, 'all_cleaned_wui_built_up.rds'))
}

# Housing units per FPA
if(!file.exists(file.path(cleaned_ztrax_fpa_out, 'all_cleaned_fpa_residential.rds'))) {
  
  #bind all of these together in one dataframe
  gpkgs <- list.files(zpoints, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/5)
  registerDoParallel(cl)
  
  ztrax_residential <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 12) %>%
      unlist
    
    if(!file.exists(file.path(ztrax_fpa_out, paste0(filename, '_ztrax_residential_fpa_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., fpa_bae_wui, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        count() 
      
      write_rds(imported,  
                file.path(ztrax_fpa_out,  paste0(filename, '_ztrax_residential_fpa_points.rds')))
    }
  }
  stopCluster(cl)
  
  # Aggregate and clean all ztrax residential dataframes
  rdss <- list.files(ztrax_fpa_out, pattern = ".rds", full.names = TRUE)
  
  fpa_years <- as.data.frame(fpa_bae_wui) %>%
    mutate(yearbuilt = discovery_year) %>%
    dplyr::select(fpa_id, yearbuilt, discovery_year) %>% as_tibble() %>%
    distinct(., .keep_all = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_decades <- pblapply(rdss,
                                  FUN = function(x, cleaned_ztrax_fpa_out, fpa_years) {
                                    require(tidyverse)
                                    
                                    filename <- strsplit(x, "\\.|/|_") %>%
                                      lapply(`[`, 9) %>%
                                      unlist
                                    
                                    imported <- read_rds(x) 
                                    
                                    blk_grps <- ungroup(imported) %>%
                                      group_by(fpa_id) %>%
                                      summarise() %>%
                                      ungroup() %>%
                                      droplevels() %>%
                                      slice(rep(1:n(), each = 2016-1992)) %>%
                                      mutate(yearbuilt = (rep(1992:2015, times = length(unique(fpa_id)))),
                                             year_built = (yearbuilt),
                                             fpa_id = factor(fpa_id))
                                    
                                    cleaned_fpa_decades <- ungroup(imported) %>%
                                      filter(yearbuilt != 0) %>%
                                      mutate(fpa_id = factor(fpa_id)) %>%
                                      group_by(fpa_id, yearbuilt) %>%
                                      summarise(n = sum(n)) %>%
                                      mutate(built_up = cumsum(n)) %>%
                                      filter(yearbuilt > 1990) %>%
                                      group_by(fpa_id, yearbuilt) %>%
                                      summarise(built_up = max(built_up)) %>%
                                      ungroup() %>%
                                      full_join(., blk_grps, by = c('fpa_id', 'yearbuilt')) %>%
                                      arrange(fpa_id, yearbuilt) %>%
                                      fill(everything(), .direction = 'up') %>%
                                      left_join(., fpa_years, by = c('fpa_id', 'yearbuilt')) %>%
                                      mutate(residential_built_up = ifelse(year_built == discovery_year, built_up, NA)) %>%
                                      na.omit()
                                    
                                    cleaned_fpa_decades %>%
                                      write_rds(., file.path(cleaned_ztrax_fpa_out, paste0(filename, '_cleaned_fpa_residential.rds')))
                                  },
                                  cleaned_ztrax_fpa_out = cleaned_ztrax_fpa_out,
                                  fpa_years = fpa_years,
                                  cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_decades <- do.call(rbind, cleaned_fpa_decades) %>%
    na.omit()  %>%
    mutate(year = year_built,
           threatened_residential_built_up = residential_built_up,
           fpa_id = factor(fpa_id)) %>%
    dplyr::select(fpa_id, year, threatened_residential_built_up)
  
  cleaned_fpa_decades %>%
    write_rds(., file.path(cleaned_ztrax_fpa_out, 'all_cleaned_fpa_residential.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_decades <- read_rds(file.path(cleaned_ztrax_fpa_out, 'all_cleaned_fpa_residential.rds'))
}


#### Residential Housing ONLY
# Housing units per WUI
if(!file.exists(file.path(cleaned_ztrax_out, 'all_cleaned_wui_residential.rds'))) {
  ztrax_gdb_to_shp <- function(i, usa_shp, zpoints) {
    require(sf)
    require(tidyverse)
    
    outname <- basename(i) %>%
      gsub('.gdb', '.gpkg', .)
    
    if(!file.exists(file.path(zpoints, outname))) {
      
      layers <- sf::st_layers(i)
      layers <- tibble::data_frame(name = layers$name)
      
      state_ztrax <- apply(unique(layers), 1, 
                           function(j) {
                             sf::st_read(i, layer = j) %>%
                               sf::st_transform(sf::st_crs(usa_shp)) %>%
                               dplyr::filter(geom_wkt != 'POINT(0 0)') %>%
                               dplyr::filter(., grepl('AG101|AG102|AG103|AG104|AG107|AG108|PP199|RI|RR', LU_stdcode))
                           })
      
      final_ztrax <- do.call(rbind, state_ztrax) 
      
      sf::st_write(final_ztrax, file.path(zpoints, outname))
    }
  }
  
  gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  pblapply(gdbs,
           FUN = ztrax_gdb_to_shp,
           usa_shp = usa_shp,
           zpoints = zpoints,
           cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  gpkgs <- list.files(zpoints, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/16)
  registerDoParallel(cl)
  
  ztrax_residential <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 12) %>%
      unlist
    
    if(!file.exists(file.path(ztrax_res_out, paste0(filename, '_ztrax_residential_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., wui, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(blk10, yearbuilt) %>%
        count() 
      
      write_rds(imported,  
                file.path(ztrax_res_out,  paste0(filename, '_ztrax_residential_points.rds')))
    }
  }
  stopCluster(cl)
  
  # Aggregate and clean all ztrax residential dataframes
  rdss <- list.files(ztrax_res_out, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_wui_decades <- pblapply(rdss,
                                  FUN = function(x, cleaned_ztrax_out) {
                                    require(tidyverse)
                                    
                                    filename <- strsplit(x, "\\.|/|_") %>%
                                      lapply(`[`, 9) %>%
                                      unlist
                                    
                                    imported <- read_rds(x) 
                                    
                                    blk_grps <- imported %>%
                                      group_by(blk10) %>%
                                      summarise() %>%
                                      ungroup() %>%
                                      droplevels() %>%
                                      slice(rep(1:n(), each = 4)) %>%
                                      mutate(decade = as.factor(rep(c(1990, 2000, 2010, 2015), times = length(unique(blk10)))),
                                             decade_built = as.factor(decade))
                                    
                                    cleaned_wui_decades <- imported %>%
                                      filter(yearbuilt != 0) %>%
                                      group_by(blk10, yearbuilt) %>%
                                      summarise(n = sum(n)) %>%
                                      mutate() %>%
                                      mutate(built_up = cumsum(n),
                                             decade = case_when(
                                               yearbuilt > 1800 & yearbuilt <= 1990 ~ 1990,
                                               yearbuilt > 1990 & yearbuilt <= 2000 ~ 2000,
                                               yearbuilt > 2000 & yearbuilt <= 2010 ~ 2010,
                                               TRUE ~ 2015),
                                             decade = as.factor(decade)) %>%
                                      group_by(blk10, decade) %>%
                                      summarise(built_up = max(built_up)) %>%
                                      ungroup() %>%
                                      full_join(., blk_grps, by = c('decade', 'blk10')) %>%
                                      droplevels() %>%
                                      mutate(decade = as.factor(decade),
                                             blk10 = as.factor(blk10),
                                             decade_built = as.integer(decade_built)) %>%
                                      arrange(desc(blk10, decade_built)) %>%
                                      fill(everything())
                                    
                                    cleaned_wui_decades %>%
                                      write_rds(., file.path(cleaned_ztrax_out, paste0(filename, '_cleaned_wui_residential.rds')))
                                  },
                                  cleaned_ztrax_out = cleaned_ztrax_out,
                                  cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_wui_decades <- do.call(rbind, cleaned_wui_decades) %>%
    na.omit()  %>%
    mutate(year = decade,
           residential_built_up = built_up) %>%
    dplyr::select(blk10, year, residential_built_up)
  
  cleaned_wui_decades %>%
    write_rds(., file.path(cleaned_ztrax_out, 'all_cleaned_wui_residential.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_wui_decades <- read_rds(file.path(cleaned_ztrax_out, 'all_cleaned_wui_residential.rds'))
}

# Housing units per FPA
if(!file.exists(file.path(cleaned_ztrax_fpa_out, 'all_cleaned_fpa_residential.rds'))) {
  
  #bind all of these together in one dataframe
  gpkgs <- list.files(zpoints, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/5)
  registerDoParallel(cl)
  
  ztrax_residential <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 12) %>%
      unlist
    
    if(!file.exists(file.path(ztrax_fpa_out, paste0(filename, '_ztrax_residential_fpa_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., fpa_bae_wui, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        count() 
      
      write_rds(imported,  
                file.path(ztrax_fpa_out,  paste0(filename, '_ztrax_residential_fpa_points.rds')))
    }
  }
  stopCluster(cl)
  
  # Aggregate and clean all ztrax residential dataframes
  rdss <- list.files(ztrax_fpa_out, pattern = ".rds", full.names = TRUE)
  
  fpa_years <- as.data.frame(fpa_bae_wui) %>%
    mutate(yearbuilt = discovery_year) %>%
    dplyr::select(fpa_id, yearbuilt, discovery_year) %>% as_tibble() %>%
    distinct(., .keep_all = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_decades <- pblapply(rdss,
                                  FUN = function(x, cleaned_ztrax_fpa_out, fpa_years) {
                                    require(tidyverse)
                                    
                                    filename <- strsplit(x, "\\.|/|_") %>%
                                      lapply(`[`, 9) %>%
                                      unlist
                                    
                                    imported <- read_rds(x) 
                                    
                                    blk_grps <- ungroup(imported) %>%
                                      group_by(fpa_id) %>%
                                      summarise() %>%
                                      ungroup() %>%
                                      droplevels() %>%
                                      slice(rep(1:n(), each = 2016-1992)) %>%
                                      mutate(yearbuilt = (rep(1992:2015, times = length(unique(fpa_id)))),
                                             year_built = (yearbuilt),
                                             fpa_id = factor(fpa_id))
                                    
                                    cleaned_fpa_decades <- ungroup(imported) %>%
                                      filter(yearbuilt != 0) %>%
                                      mutate(fpa_id = factor(fpa_id)) %>%
                                      group_by(fpa_id, yearbuilt) %>%
                                      summarise(n = sum(n)) %>%
                                      mutate(built_up = cumsum(n)) %>%
                                      filter(yearbuilt > 1990) %>%
                                      group_by(fpa_id, yearbuilt) %>%
                                      summarise(built_up = max(built_up)) %>%
                                      ungroup() %>%
                                      full_join(., blk_grps, by = c('fpa_id', 'yearbuilt')) %>%
                                      arrange(fpa_id, yearbuilt) %>%
                                      fill(everything(), .direction = 'up') %>%
                                      left_join(., fpa_years, by = c('fpa_id', 'yearbuilt')) %>%
                                      mutate(residential_built_up = ifelse(year_built == discovery_year, built_up, NA)) %>%
                                      na.omit()
                                    
                                    cleaned_fpa_decades %>%
                                      write_rds(., file.path(cleaned_ztrax_fpa_out, paste0(filename, '_cleaned_fpa_residential.rds')))
                                  },
                                  cleaned_ztrax_fpa_out = cleaned_ztrax_fpa_out,
                                  fpa_years = fpa_years,
                                  cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_decades <- lapply(list.files(cleaned_ztrax_fpa_out, pattern = ".rds", full.names = TRUE),
                                FUN = function(x)
                                  imported <- read_rds(x))
  
  cleaned_fpa_decades <- do.call(rbind, cleaned_fpa_decades) %>%
    na.omit()  %>%
    mutate(year = year_built,
           threatened_residential_built_up = residential_built_up,
           fpa_id = factor(fpa_id)) %>%
    dplyr::select(fpa_id, year, threatened_residential_built_up)
  
  cleaned_fpa_decades %>%
    write_rds(., file.path(cleaned_ztrax_fpa_out, 'all_cleaned_fpa_residential.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_decades <- read_rds(file.path(cleaned_ztrax_fpa_out, 'all_cleaned_fpa_residential.rds')) %>%
    left_join(fpa_wui, ., by = 'fpa_id')
}
