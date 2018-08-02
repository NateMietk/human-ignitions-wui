
# Prep data by subsetting and transforming 
gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

pblapply(gdbs, FUN = subset_ztrax, usa_shp = usa_shp, 
         out_dir = dir_raw_ztrax_gpkg, cl = cl)
system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
stopCluster(cl)

# Built up units per WUI block groups
if(!file.exists(file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/16))
  
  cleaned_wui <- pblapply(gpkgs,
                          FUN = intersect_ztrax,
                          mask = wui,
                          which_dataset = '1',
                          out_dir_cleaned = dir_cleaned_wui_ztrax_rds,
                          out_name_cleaned = '_cleaned_wui_built_up.rds', 
                          out_dir = dir_wui_ztrax_rds,
                          out_name = '_ztrax_wui.rds', 
                          cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_wui_all <- do.call(rbind, cleaned_wui) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(blk10, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_wui_all %>%
    write_rds(., file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
  
  
} else {
  
  cleaned_wui_all <- read_rds(file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_build_up.rds'))
}

# Built up units per ICS 209 block groups
if(!file.exists(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_ics <- pblapply(gpkgs,
                          FUN = intersect_ztrax,
                          mask = ics209_bae,
                          which_dataset = '3',
                          out_dir_cleaned = dir_cleaned_ics_ztrax_rds,
                          out_name_cleaned = '_cleaned_ics_built_up.rds', 
                          out_dir = dir_ics_ztrax_rds,
                          out_name = '_ztrax_ics.rds', 
                          cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_ics_all <- do.call(rbind, cleaned_ics) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(incident_unique_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_ics_all %>%
    write_rds(., file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
    
  } else {
    
    cleaned_ics_all <- read_rds(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_build_up.rds'))
  }

# Built up units per ICS 209 block groups with 250m buffer
if(!file.exists(file.path(dir_cleaned_ics_250m_ztrax_rds, 'all_cleaned_ics_250m_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_ics_250m <- pblapply(gpkgs,
                          FUN = intersect_ztrax,
                          mask = ics209_bae_250m,
                          which_dataset = '3',
                          out_dir_cleaned = dir_cleaned_ics_250m_ztrax_rds,
                          out_name_cleaned = '_cleaned_ics_250m_built_up.rds', 
                          out_dir = dir_ics_250m_ztrax_rds,
                          out_name = '_ztrax_ics_250m.rds', 
                          cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_ics_250m_all <- do.call(rbind, cleaned_ics_250m) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(incident_unique_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_ics_250m_all %>%
    write_rds(., file.path(dir_cleaned_ics_250m_ztrax_rds, 'all_cleaned_ics_250m_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
  
} else {
  
  cleaned_ics_250m_all <- read_rds(file.path(dir_cleaned_ics_250m_ztrax_rds, 'all_cleaned_ics_250m_built_up.rds'))
}

# Built up units per FPA perimeters
if(!file.exists(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/5))
  
  cleaned_fpa <- pblapply(gpkgs,
                          FUN = intersect_ztrax,
                          mask = bae,
                          which_dataset = '2',
                          out_dir_cleaned = dir_cleaned_fpa_ztrax_rds,
                          out_name_cleaned = '_cleaned_fpa_built_up.rds', 
                          out_dir = dir_fpa_ztrax_rds,
                          out_name = '_ztrax_fpa.rds', 
                          cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_all <- do.call(rbind, cleaned_fpa) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_fpa_all %>%
    write_rds(., file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
  
} else {
  
  cleaned_fpa_all <- read_rds(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_build_up.rds'))
}

# Built up units per FPA 250m perimeters
if(!file.exists(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/6))
  
  cleaned_fpa_250m <- pblapply(gpkgs,
                               FUN = intersect_ztrax,
                               mask = fpa_250m,
                               which_dataset = '2',
                               out_dir_cleaned = dir_cleaned_fpa_250m_ztrax_rds,
                               out_name_cleaned = '_cleaned_fpa_250m_built_up.rds', 
                               out_dir = dir_fpa_250m_ztrax_rds,
                               out_name = '_ztrax_fpa_250m.rds', 
                               cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_250m_all <- do.call(rbind, cleaned_fpa_250m) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_fpa_250m_all %>%
    write_rds(., file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
  
} else {
  
  cleaned_fpa_250m_all <- read_rds(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_build_up.rds'))
}

# Built up units per FPA 500m perimeters
if(!file.exists(file.path(dir_cleaned_fpa_500m_ztrax_rds, 'all_cleaned_fpa_500m_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/8))
  
  cleaned_fpa_500m <- pblapply(gpkgs,
                               FUN = intersect_ztrax,
                               mask = fpa_500m,
                               which_dataset = '2',
                               out_dir_cleaned = dir_cleaned_fpa_500m_ztrax_rds,
                               out_name_cleaned = '_cleaned_fpa_500m_built_up.rds', 
                               out_dir = dir_fpa_500m_ztrax_rds,
                               out_name = '_ztrax_fpa_500m.rds', 
                               cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_500m_all <- do.call(rbind, cleaned_fpa_500m) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_fpa_500m_all %>%
    write_rds(., file.path(dir_cleaned_fpa_500m_ztrax_rds, 'all_cleaned_fpa_500m_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
  
} else {
  
  cleaned_fpa_500m_all <- read_rds(file.path(dir_cleaned_fpa_500m_ztrax_rds, 'all_cleaned_fpa_500m_build_up.rds'))
}

# Built up units per FPA 1000m perimeters
if(!file.exists(file.path(dir_cleaned_fpa_1000m_ztrax_rds, 'all_cleaned_fpa_1000m_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/8))
  
  cleaned_fpa_1000m <- pblapply(gpkgs,
                                FUN = intersect_ztrax,
                                mask = fpa_1000m,
                                which_dataset = '2',
                                out_dir_cleaned = dir_cleaned_fpa_1000m_ztrax_rds,
                                out_name_cleaned = '_cleaned_fpa_1000m_built_up.rds', 
                                out_dir = dir_fpa_1000m_ztrax_rds,
                                out_name = '_ztrax_fpa_1000m.rds', 
                                cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_1000m_all <- do.call(rbind, cleaned_fpa_1000m) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_fpa_1000m_all %>%
    write_rds(., file.path(dir_cleaned_fpa_1000m_ztrax_rds, 'all_cleaned_fpa_1000m_built_up.rds'))
  
  system(paste0('aws s3 sync ', anthro_out, " ", s3_anthro_prefix))
  
} else {
  
  cleaned_fpa_1000m_all <- read_rds(file.path(dir_cleaned_fpa_1000m_ztrax_rds, 'all_cleaned_fpa_1000m_build_up.rds'))
}