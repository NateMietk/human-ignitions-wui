
if (!exists("grids")) {
  if (!file.exists(file.path(fishnet_path, "grids.gpkg"))) {
    grids <- sf::st_make_grid(usa_shp, cellsize = 2600, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('grid_id' = 1:length(.))) %>%
      sf::st_intersection(., st_union(usa_shp)) %>%
      st_cast("POLYGON")
    
    sf::st_write(grids, file.path(fishnet_path, "grids.gpkg"), driver = "GPKG")

    system(paste0("aws s3 sync ", fishnet_path, " ", s3_bounds_prefix, "/fishnet"))
  } else {
    grids <- sf::st_read(file.path(fishnet_path, "grids.gpkg"))
  }
}

for(i in 1:10) {

  print(paste0('Starting run: ', i))
  
  set.seed(paste0('1234',i))
  slim_fire <- fpa_fire %>%
    sample_n(., 2000)
  
  slim_grid <- grids %>%
    st_join(., slim_fire) %>%
    filter(!is.na(FPA_ID)) %>%
    dplyr::select(FPA_ID, grid_id)
  
  if(!file.exists(file.path(accuracy_assessment_dir, paste0('jittered_points_2_6km_grid_', i, '.gpkg')))) {
    library(foreach)
    library(doParallel)
    
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl)
    
    # Using the r5a.4xlarge EC2 instnace
    
    sampled_points <- foreach(x = unique(slim_fire$FPA_ID), .combine = rbind,
                              .packages = c('foreach', 'tidyverse', 'raster', 'sf', 'velox', 'stars')) %dopar% {
                                require(tidyverse)
                                require(sf)
                                split <- slim_fire %>%
                                  dplyr::filter(FPA_ID == x)
                                split$FPA_ID <- droplevels(split$FPA_ID)
                                
                                grid_join <- slim_grid %>%
                                  dplyr::filter(FPA_ID == x)
                                grid_join$FPA_ID <- droplevels(grid_join$FPA_ID)
                                
                                samp <- st_sample(grid_join, 1)
                                if(st_geometry_type(samp) == "POINT" && length(samp) > 0 && !is.null(samp) && !is.na(samp)) {
                                  samp_df <- samp %>%
                                    st_sf() %>%
                                    mutate(FPA_ID = as.data.frame(split)$FPA_ID)
                                } 
                                return(samp_df)
                              }
    
    parallel::stopCluster(cl)
    
    print(paste0('Finished sampling: ', i))
    
    sampled_points <- sampled_points %>%
      left_join(., as.data.frame(slim_fire) %>% dplyr::select(-geom), by = 'FPA_ID') 
    sampled_points %>%
      st_write(., file.path(accuracy_assessment_dir, paste0('jittered_points_2_6km_grid_', i, '.gpkg')),
               delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  } else {
    sampled_points <- st_read(file.path(accuracy_assessment_dir, paste0('jittered_points_2_6km_grid_', i, '.gpkg')))
  }

  bae_raw <- slim_fire %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt((FIRE_SIZE*4046.86)/pi)) %>%
    st_parallel(., st_buffer, n_cores = parallel::detectCores(), dist = .$RADIUS) %>%
    st_transform(proj_ea)
  
  bae_sampled <- sampled_points %>%
    st_transform(proj_ed) %>%
    mutate(RADIUS = sqrt((FIRE_SIZE*4046.86)/pi)) %>%
    st_parallel(., st_buffer, n_cores = parallel::detectCores(), dist = .$RADIUS) %>%
    st_transform(proj_ea)
  
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  if(!file.exists(file.path(accuracy_assessment_dir, paste0('ztrax_raw_gridded_', i, '.rds')))) {
    pboptions(type = 'txt', use_lb = TRUE)
    cl <- makeCluster(getOption("cl.cores", parallel::detectCores()/2))
    
    ztrax_raw <- pblapply(gpkgs,
                          FUN = intersect_ztrax,
                          mask = bae_raw,
                          usa = usa_shp,
                          cl = cl)
    
    stopCluster(cl)
    
    ztrax_raw <- do.call(rbind, ztrax_raw) %>%
      left_join(., as.data.frame(slim_fire) %>% rename_all(tolower) %>% dplyr::select(fpa_id, discovery_year), by = 'fpa_id') %>%
      filter(yearbuilt == discovery_year)
    
    ztrax_raw %>%
      write_rds(., file.path(accuracy_assessment_dir, paste0('ztrax_raw_gridded_', i, '.rds')))
    }
    
  if(!file.exists(file.path(accuracy_assessment_dir, paste0('ztrax_sampled_gridded_', i, '.rds')))) {
    
    pboptions(type = 'txt', use_lb = TRUE)
    cl <- makeCluster(getOption("cl.cores", parallel::detectCores()/2))
    
    ztrax_sampled <- pblapply(gpkgs,
                              FUN = intersect_ztrax,
                              mask = bae_sampled,
                              usa = usa_shp,
                              cl = cl)
    
    stopCluster(cl)
    
    ztrax_sampled <- do.call(rbind, ztrax_sampled) %>%
      left_join(., as.data.frame(slim_fire) %>% rename_all(tolower) %>% dplyr::select(fpa_id, discovery_year), by = 'fpa_id') %>%
      filter(yearbuilt == discovery_year)
    
    ztrax_sampled %>%
      write_rds(., file.path(accuracy_assessment_dir, paste0('ztrax_sampled_gridded_', i, '.rds')))
    }
  }
