# Global variables ------------------------------------------------------
fpa <- as_tibble(as.data.frame(fpa_fire)) %>%
  dplyr::select(FPA_ID, DISCOVERY_YEAR)

bae <- bae %>%
  dplyr::select(-FIRE_SIZE_km2) %>%
  left_join(., fpa, by = 'FPA_ID')

fpa_years <- as.data.frame(bae) %>%
  setNames(tolower(names(.))) %>%
  mutate(yearbuilt = discovery_year) %>%
  dplyr::select(fpa_id, yearbuilt, discovery_year) %>% as_tibble() %>%
  distinct(., .keep_all = TRUE)

fpa_250m_years <- as.data.frame(fpa_250m) %>%
  setNames(tolower(names(.))) %>%
  mutate(yearbuilt = discovery_year) %>%
  dplyr::select(fpa_id, yearbuilt, discovery_year) %>% as_tibble() 

# Prep data by subsetting and transforming --------------------------------
gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

pblapply(gdbs, FUN = subset_ztrax, usa_shp = usa_shp, 
         out_dir = dir_raw_ztrax_gpkg, cl = cl)
system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
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
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
  
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
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-ics/anthro")
  
  
} else {
  
  cleaned_ics_all <- read_rds(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_build_up.rds'))
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
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-fpa/anthro")
  
} else {
  
  cleaned_fpa_all <- read_rds(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_build_up.rds'))
}

# Built up units per FPA 250m perimeters
if(!file.exists(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_built_up.rds'))) {
  
  # find the number of built-up units and build-up area by census block group, built year, and built class
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/5))
  
  cleaned_fpa <- pblapply(gpkgs,
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
  cleaned_fpa_250m_all <- do.call(rbind, cleaned_fpa) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_250m_id, year, built_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_fpa_250m_all %>%
    write_rds(., file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-fpa/anthro")
  
} else {
  
  cleaned_fpa_250m_all <- read_rds(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_build_up.rds'))
}
