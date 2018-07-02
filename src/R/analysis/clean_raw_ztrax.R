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


# ------------------------------- All built up ------------------------------------------------------------
# Prep data by subsetting and transforming --------------------------------
gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

pblapply(gdbs,
         FUN = function(i, usa_shp, dir_raw_abu_gpkg) {
           require(sf)
           require(tidyverse)
           
           outname <- basename(i) %>%
             gsub('.gdb', '.gpkg', .)
           
           if(!file.exists(file.path(dir_raw_abu_gpkg, outname))) {

             layers <- tibble::data_frame(name = sf::st_layers(i)$name)

             state_ztrax <- apply(unique(layers), 1,
                                  FUN = function(j, i, usa_shp) {
                                    state_ztrax <- sf::st_read(i, layer = j) %>%
                                      dplyr::filter(geom_wkt != 'POINT(0 0)') %>%
                                      dplyr::filter(!(str_detect(LU_stdcode, 'VL|AG101|AG102|AG103|AG104|AG107|AG108|PP199|RI|RR'))) %>%
                                      sf::st_transform(sf::st_crs(usa_shp))
                                  }, 
                                  i = i,
                                  usa_shp = usa_shp
             )
             state_ztrax <- do.call(rbind, state_ztrax)
             
             sf::st_write(state_ztrax, file.path(dir_raw_abu_gpkg, outname))
           }
         },
         usa_shp = usa_shp,
         dir_raw_abu_gpkg = dir_raw_abu_gpkg,
         cl = cl)

stopCluster(cl)

# Extract by WUI and FPA --------------------------------
# All built up units per FPA
if(!file.exists(file.path(dir_cleaned_wui_abu_rds, 'all_cleaned_wui_all_build_up.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  # find the number of built-up units and build-up area by census block group and built year
  gpkgs <- list.files(dir_raw_abu_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/16))
  
  pblapply(gpkgs,
           FUN = function(x, dir_wui_abu_rds, wui) {
             require(sf)
             require(tidyverse)
             
             filename <- strsplit(x, "\\.|/|_") %>%
               lapply(`[`, 18) %>%
               unlist
             
             if(!file.exists(file.path(dir_wui_abu_rds, paste0(filename, '_ztrax_abu_wui_points.rds')))) {
               
               imported <- sf::st_read(x) %>%
                 st_join(., wui, join = st_intersects) %>%
                 setNames(tolower(names(.))) %>%
                 as.data.frame() %>%
                 group_by(blk10, yearbuilt) %>%
                 summarise(abu_build_up_count = n(),
                           abu_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
               write_rds(imported,  
                         file.path(dir_wui_abu_rds,  paste0(filename, '_ztrax_abu_wui_points.rds')))
             }
           },
           dir_wui_abu_rds = dir_wui_abu_rds,
           wui = wui,
           cl = cl)
  
  stopCluster(cl)
  
  
  ############## Aggregate and clean all ztrax all_build_up dataframes ############################
  rdss <- list.files(dir_wui_abu_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_wui_abu <- pblapply(rdss,
                              FUN = function(x, dir_cleaned_wui_abu_rds) {
                                require(tidyverse)
                                
                                filename <- strsplit(x, "\\.|/|_") %>%
                                  lapply(`[`, 15) %>%
                                  unlist
                                
                                if(!file.exists(file.path(dir_cleaned_wui_abu_rds, paste0(filename, '_cleaned_wui_all_build_up.rds')))) {
                                  imported <- read_rds(x)
                                  
                                  blk_grps <- imported %>%
                                    group_by(blk10) %>%
                                    summarise() %>%
                                    ungroup() %>%
                                    droplevels() %>%
                                    slice(rep(1:n(), each = 2016-1992)) %>%
                                    mutate(yearbuilt = (rep(1992:2015, times = length(unique(blk10)))),
                                           year_built = (yearbuilt),
                                           blk10 = factor(blk10))
                                  
                                  cleaned_wui_abu <- ungroup(imported) %>%
                                    filter(yearbuilt != 0) %>%
                                    mutate(blk10 = factor(blk10)) %>%
                                    group_by(blk10, yearbuilt) %>%
                                    summarise(abu_build_up_count = sum(abu_build_up_count),
                                              abu_build_up_intensity_sqm = sum(abu_build_up_intensity_sqm)) %>%  
                                    mutate(abu_build_up_count = cumsum(abu_build_up_count),
                                           abu_build_up_intensity_sqm = cumsum(abu_build_up_intensity_sqm)) %>%
                                    filter(yearbuilt >= 1992) %>%
                                    group_by(blk10, yearbuilt) %>%
                                    summarise(abu_build_up_count = max(abu_build_up_count),
                                              abu_build_up_intensity_sqm = max(abu_build_up_intensity_sqm)) %>%                                      ungroup() %>%
                                    full_join(., blk_grps, by = c('blk10', 'yearbuilt')) %>%
                                    arrange(blk10, yearbuilt) %>%
                                    fill(everything(), .direction = 'up') 
                                  
                                  cleaned_wui_abu %>%
                                    write_rds(., file.path(dir_cleaned_wui_abu_rds, paste0(filename, '_cleaned_wui_all_build_up.rds')))
                                }
                              },
                              dir_cleaned_wui_abu_rds = dir_cleaned_wui_abu_rds,
                              cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_wui_abu <- do.call(rbind, cleaned_wui_abu) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(blk10, year, abu_build_up_count, abu_build_up_intensity_sqm)
  
  cleaned_wui_abu %>%
    write_rds(., file.path(dir_cleaned_wui_abu_rds, 'all_cleaned_wui_all_build_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
  
} else {
  
  cleaned_wui_abu <- read_rds(file.path(dir_cleaned_wui_abu_rds, 'all_cleaned_wui_all_build_up.rds'))
}


if(!file.exists(file.path(dir_cleaned_fpa_abu_rds, 'all_cleaned_fpa_all_built_up.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  gpkgs <- list.files(dir_raw_abu_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/7)
  registerDoParallel(cl)
  
  ztrax_all_built_up <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 18) %>%
      unlist
    
    if(!file.exists(file.path(dir_fpa_abu_rds, paste0(filename, '_ztrax_all_built_up_fpa_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., bae, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        summarise(abu_build_up_count = n(),
                  abu_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_fpa_abu_rds,  paste0(filename, '_ztrax_all_built_up_fpa_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax all_built_up dataframes ############################
  rdss <- list.files(dir_fpa_abu_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_decades_abu <- pblapply(rdss,
                                      FUN = function(x, dir_cleaned_fpa_abu_rds, fpa_years) {
                                        require(tidyverse)
                                        
                                        filename <- strsplit(x, "\\.|/|_") %>%
                                          lapply(`[`, 15) %>%
                                          unlist
                                        
                                        imported <- read_rds(x) 
                                        
                                        blk_grps <- ungroup(imported) %>%
                                          group_by(fpa_id) %>%
                                          summarise() %>%
                                          ungroup() %>%
                                          droplevels() %>%
                                          slice(rep(1:n(), each = 2016-1984)) %>%
                                          mutate(yearbuilt = (rep(1984:2015, times = length(unique(fpa_id)))),
                                                 year_built = (yearbuilt),
                                                 fpa_id = factor(fpa_id))
                                        
                                        cleaned_fpa_decades <- ungroup(imported) %>%
                                          filter(yearbuilt != 0) %>%
                                          mutate(fpa_id = factor(fpa_id)) %>%
                                          group_by(fpa_id, yearbuilt) %>%
                                          summarise(abu_build_up_count = sum(abu_build_up_count),
                                                    abu_build_up_intensity_sqm = sum(abu_build_up_intensity_sqm)) %>%  
                                          mutate(abu_build_up_count = cumsum(abu_build_up_count),
                                                 abu_build_up_intensity_sqm = cumsum(abu_build_up_intensity_sqm)) %>%
                                          filter(yearbuilt > 1983) %>%
                                          group_by(fpa_id, yearbuilt) %>%
                                          summarise(abu_build_up_count = max(abu_build_up_count),
                                                    abu_build_up_intensity_sqm = max(abu_build_up_intensity_sqm)) %>%                                      ungroup() %>%
                                          full_join(., blk_grps, by = c('fpa_id', 'yearbuilt')) %>%
                                          arrange(fpa_id, yearbuilt) %>%
                                          fill(everything(), .direction = 'up') %>%
                                          left_join(., fpa_years, by = c('fpa_id', 'yearbuilt')) %>%
                                          mutate(abu_build_up_count = ifelse(year_built == discovery_year, abu_build_up_count, NA),
                                                 abu_build_up_intensity_sqm = ifelse(year_built == discovery_year, abu_build_up_intensity_sqm, NA)) %>%
                                          na.omit()
                                        
                                        cleaned_fpa_decades %>%
                                          write_rds(., file.path(dir_cleaned_fpa_abu_rds, paste0(filename, '_cleaned_fpa_all_built_up.rds')))
                                      },
                                      dir_cleaned_fpa_abu_rds = dir_cleaned_fpa_abu_rds,
                                      fpa_years = fpa_years,
                                      cl = cl)
  
  stopCluster(cl)
  
  
  #bind all of these together in one dataframe
  cleaned_fpa_decades_abu <- do.call(rbind, cleaned_fpa_decades_abu) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, abu_build_up_count, abu_build_up_intensity_sqm)
  
  cleaned_fpa_decades_abu %>%
    write_rds(., file.path(dir_cleaned_fpa_abu_rds, 'all_cleaned_fpa_all_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_decades_abu <- read_rds(file.path(dir_cleaned_fpa_abu_rds, 'all_cleaned_fpa_all_built_up.rds'))
}

# All built up units per FPA with 250m buffer
if(!file.exists(file.path(dir_cleaned_fpa_250m_abu_rds, 'all_cleaned_fpa_250m_all_built_up.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  gpkgs <- list.files(dir_raw_abu_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/6)
  registerDoParallel(cl)
  
  ztrax_all_built_up <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 18) %>%
      unlist
    
    if(!file.exists(file.path(dir_fpa_250m_abu_rds, paste0(filename, '_ztrax_all_built_up_fpa_250m_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., fpa_250m, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        summarise(abu_build_up_count = n(),
                  abu_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_fpa_250m_abu_rds,  paste0(filename, '_ztrax_all_built_up_fpa_250m_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax all_built_up dataframes ############################
  rdss <- list.files(dir_fpa_250m_abu_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_250m_decades_abu <- pblapply(rdss,
                                           FUN = function(x, dir_cleaned_fpa_250m_abu_rds, fpa_250m_years) {
                                             require(tidyverse)
                                             
                                             filename <- strsplit(x, "\\.|/|_") %>%
                                               lapply(`[`, 16) %>%
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
                                             
                                             cleaned_fpa_250m_decades <- ungroup(imported) %>%
                                               filter(yearbuilt != 0) %>%
                                               mutate(fpa_id = factor(fpa_id)) %>%
                                               group_by(fpa_id, yearbuilt) %>%
                                               summarise(abu_build_up_count = sum(abu_build_up_count),
                                                         abu_build_up_intensity_sqm = sum(abu_build_up_intensity_sqm)) %>%  
                                               mutate(abu_build_up_count = cumsum(abu_build_up_count),
                                                      abu_build_up_intensity_sqm = cumsum(abu_build_up_intensity_sqm)) %>%
                                               filter(yearbuilt > 1991) %>%
                                               group_by(fpa_id, yearbuilt) %>%
                                               summarise(abu_build_up_count = max(abu_build_up_count),
                                                         abu_build_up_intensity_sqm = max(abu_build_up_intensity_sqm)) %>%                                      ungroup() %>%
                                               full_join(., blk_grps, by = c('fpa_id', 'yearbuilt')) %>%
                                               arrange(fpa_id, yearbuilt) %>%
                                               fill(everything(), .direction = 'up')  %>%
                                               left_join(., fpa_250m_years, by = c('fpa_id', 'yearbuilt')) %>%
                                               mutate(abu_build_up_count = ifelse(year_built == discovery_year, abu_build_up_count, NA),
                                                      abu_build_up_intensity_sqm = ifelse(year_built == discovery_year, abu_build_up_intensity_sqm, NA)) %>%
                                               filter(!is.na(abu_build_up_count) | !is.na(abu_build_up_intensity_sqm)) 
                                             
                                             
                                             if(nrow(cleaned_fpa_250m_decades) > 0) {
                                               cleaned_fpa_250m_decades %>%
                                                 write_rds(., file.path(dir_cleaned_fpa_250m_abu_rds, paste0(filename, '_cleaned_fpa_250m_all_built_up.rds')))
                                             } 
                                           },
                                           dir_cleaned_fpa_250m_abu_rds = dir_cleaned_fpa_250m_abu_rds,
                                           fpa_250m_years = fpa_250m_years,
                                           cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_250m_decades_abu <- do.call(rbind, cleaned_fpa_250m_decades_abu) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, abu_build_up_count, abu_build_up_intensity_sqm)
  
  cleaned_fpa_250m_decades_abu %>%
    write_rds(., file.path(dir_cleaned_fpa_250m_abu_rds, 'all_cleaned_fpa_250m_all_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_250m_decades_abu <- read_rds(file.path(dir_cleaned_fpa_250m_abu_rds, 'all_cleaned_fpa_250m_all_built_up.rds'))
}        

#------------------------------- Residential Housing  -------------------------------------------
# Prep data by subsetting and transforming --------------------------------
gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

pblapply(gdbs,
         FUN = function(i, usa_shp, dir_raw_res_gpkg) {
           require(sf)
           require(tidyverse)
           
           outname <- basename(i) %>%
             gsub('.gdb', '.gpkg', .)
           
           if(!file.exists(file.path(dir_raw_res_gpkg, outname))) {
             
             layers <- tibble::data_frame(name = sf::st_layers(i)$name)
             
             state_ztrax <- apply(unique(layers), 1,
                                  FUN = function(j, i, usa_shp) {
                                    state_ztrax <- sf::st_read(i, layer = j) %>%
                                      dplyr::filter(geom_wkt != 'POINT(0 0)') %>%
                                      dplyr::filter(grepl('AG101|AG102|AG103|AG104|AG107|AG108|PP199|RI|RR', LU_stdcode)) %>%
                                      sf::st_transform(sf::st_crs(usa_shp))
                                  }, 
                                  i = i,
                                  usa_shp = usa_shp
             )
             state_ztrax <- do.call(rbind, state_ztrax)
             
             sf::st_write(state_ztrax, file.path(dir_raw_res_gpkg, outname))
           }
         },
         usa_shp = usa_shp,
         dir_raw_res_gpkg = dir_raw_res_gpkg,
         cl = cl)

stopCluster(cl)

# Extract by WUI and FPA --------------------------------
if(!file.exists(file.path(dir_cleaned_wui_res_rds, 'all_cleaned_wui_residential.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  # find the number of built-up units and build-up area by census block group and built year
  gpkgs <- list.files(dir_raw_res_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()/16))
  
  pblapply(gpkgs,
           FUN = function(x, dir_wui_res_rds, wui) {
             require(sf)
             require(tidyverse)
             
             filename <- strsplit(x, "\\.|/|_") %>%
               lapply(`[`, 14) %>%
               unlist
             
             if(!file.exists(file.path(dir_wui_res_rds, paste0(filename, '_ztrax_res_wui_points.rds')))) {
               
               imported <- sf::st_read(x) %>%
                 st_join(., wui, join = st_intersects) %>%
                 setNames(tolower(names(.))) %>%
                 as.data.frame() %>%
                 group_by(blk10, yearbuilt) %>%
                 summarise(res_build_up_count = n(),
                           res_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
               write_rds(imported,  
                         file.path(dir_wui_res_rds,  paste0(filename, '_ztrax_res_wui_points.rds')))
             }
           },
           dir_wui_res_rds = dir_wui_res_rds,
           wui = wui,
           cl = cl)
  
  stopCluster(cl)
  
  
  ############## Aggregate and clean all ztrax residential dataframes ############################
  rdss <- list.files(dir_wui_res_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_wui_residental <- pblapply(rdss,
                                     FUN = function(x, dir_cleaned_wui_res_rds) {
                                       require(tidyverse)
                                       
                                       filename <- strsplit(x, "\\.|/|_") %>%
                                         lapply(`[`, 11) %>%
                                         unlist
                                       
                                       if(!file.exists(file.path(dir_cleaned_wui_res_rds, paste0(filename, '_cleaned_wui_residential.rds')))) {
                                         imported <- read_rds(x)
                                         
                                         blk_grps <- imported %>%
                                           group_by(blk10) %>%
                                           summarise() %>%
                                           ungroup() %>%
                                           droplevels() %>%
                                           slice(rep(1:n(), each = 2016-1992)) %>%
                                           mutate(yearbuilt = (rep(1992:2015, times = length(unique(blk10)))),
                                                  year_built = (yearbuilt),
                                                  blk10 = factor(blk10))
                                         
                                         cleaned_wui_residental <- ungroup(imported) %>%
                                           filter(yearbuilt != 0) %>%
                                           mutate(blk10 = factor(blk10)) %>%
                                           group_by(blk10, yearbuilt) %>%
                                           summarise(res_build_up_count = sum(res_build_up_count),
                                                     res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%  
                                           mutate(res_build_up_count = cumsum(res_build_up_count),
                                                  res_build_up_intensity_sqm = cumsum(res_build_up_intensity_sqm)) %>%
                                           filter(yearbuilt >= 1992) %>%
                                           group_by(blk10, yearbuilt) %>%
                                           summarise(res_build_up_count = max(res_build_up_count),
                                                     res_build_up_intensity_sqm = max(res_build_up_intensity_sqm)) %>%                                      ungroup() %>%
                                           full_join(., blk_grps, by = c('blk10', 'yearbuilt')) %>%
                                           arrange(blk10, yearbuilt) %>%
                                           fill(everything(), .direction = 'up') 
                                         
                                         cleaned_wui_residental %>%
                                           write_rds(., file.path(dir_cleaned_wui_res_rds, paste0(filename, '_cleaned_wui_residential.rds')))
                                       }
                                     },
                                     dir_cleaned_wui_res_rds = dir_cleaned_wui_res_rds,
                                     cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_wui_residental <- do.call(rbind, cleaned_wui_residental) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(blk10, year, res_build_up_count, res_build_up_intensity_sqm)
  
  cleaned_wui_residental %>%
    write_rds(., file.path(dir_cleaned_wui_res_rds, 'all_cleaned_wui_residential.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
  
} else {
  
  cleaned_wui_residental <- read_rds(file.path(dir_cleaned_wui_res_rds, 'all_cleaned_wui_residential.rds'))
}

# Housing units per FPA
if(!file.exists(file.path(dir_cleaned_fpa_res_rds, 'all_cleaned_fpa_residential.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  gpkgs <- list.files(dir_raw_res_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/5)
  registerDoParallel(cl)
  
  ztrax_residential <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 14) %>%
      unlist
    
    if(!file.exists(file.path(dir_fpa_res_rds, paste0(filename, '_ztrax_residential_fpa_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., bae, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        summarise(res_build_up_count = n(),
                  res_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_fpa_res_rds,  paste0(filename, '_ztrax_residential_fpa_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax residential dataframes ############################
  rdss <- list.files(dir_fpa_res_rds, pattern = ".rds", full.names = TRUE)
  
  fpa_years <- as.data.frame(bae) %>%
    setNames(tolower(names(.))) %>%
    mutate(yearbuilt = discovery_year) %>%
    dplyr::select(fpa_id, yearbuilt, discovery_year) %>% as_tibble() %>%
    distinct(., .keep_all = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_decades <- pblapply(rdss,
                                  FUN = function(x, dir_cleaned_fpa_res_rds, fpa_years) {
                                    require(tidyverse)
                                    
                                    filename <- strsplit(x, "\\.|/|_") %>%
                                      lapply(`[`, 11) %>%
                                      unlist
                                    
                                    imported <- read_rds(x) 
                                    
                                    blk_grps <- ungroup(imported) %>%
                                      group_by(fpa_id) %>%
                                      summarise() %>%
                                      ungroup() %>%
                                      droplevels() %>%
                                      slice(rep(1:n(), each = 2016-1984)) %>%
                                      mutate(yearbuilt = (rep(1984:2015, times = length(unique(fpa_id)))),
                                             year_built = (yearbuilt),
                                             fpa_id = factor(fpa_id))
                                    
                                    cleaned_fpa_decades <- ungroup(imported) %>%
                                      filter(yearbuilt != 0) %>%
                                      mutate(fpa_id = factor(fpa_id)) %>%
                                      group_by(fpa_id, yearbuilt) %>%
                                      summarise(res_build_up_count = sum(res_build_up_count),
                                                res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%  
                                      mutate(res_build_up_count = cumsum(res_build_up_count),
                                             res_build_up_intensity_sqm = cumsum(res_build_up_intensity_sqm)) %>%
                                      filter(yearbuilt > 1983) %>%
                                      group_by(fpa_id, yearbuilt) %>%
                                      summarise(res_build_up_count = max(res_build_up_count),
                                                res_build_up_intensity_sqm = max(res_build_up_intensity_sqm)) %>%                                      ungroup() %>%
                                      full_join(., blk_grps, by = c('fpa_id', 'yearbuilt')) %>%
                                      arrange(fpa_id, yearbuilt) %>%
                                      fill(everything(), .direction = 'up') %>%
                                      left_join(., fpa_years, by = c('fpa_id', 'yearbuilt')) %>%
                                      mutate(res_build_up_count = ifelse(year_built == discovery_year, res_build_up_count, NA),
                                             res_build_up_intensity_sqm = ifelse(year_built == discovery_year, res_build_up_intensity_sqm, NA)) %>%
                                      na.omit()
                                    
                                    cleaned_fpa_decades %>%
                                      write_rds(., file.path(dir_cleaned_fpa_res_rds, paste0(filename, '_cleaned_fpa_residential.rds')))
                                  },
                                  dir_cleaned_fpa_res_rds = dir_cleaned_fpa_res_rds,
                                  fpa_years = fpa_years,
                                  cl = cl)
  
  stopCluster(cl)
  
  
  #bind all of these together in one dataframe
  cleaned_fpa_decades <- do.call(rbind, cleaned_fpa_decades) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, res_build_up_count, res_build_up_intensity_sqm)
  
  cleaned_fpa_decades %>%
    write_rds(., file.path(dir_cleaned_fpa_res_rds, 'all_cleaned_fpa_residential.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_decades <- read_rds(file.path(dir_cleaned_fpa_res_rds, 'all_cleaned_fpa_residential.rds'))
}

# Housing units per FPA with 250m buffer
if(!file.exists(file.path(dir_cleaned_fpa_250m_res_rds, 'all_cleaned_fpa_250m_residential.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  gpkgs <- list.files(dir_raw_res_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/6)
  registerDoParallel(cl)
  
  ztrax_residential <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 14) %>%
      unlist
    
    if(!file.exists(file.path(dir_fpa_250m_res_rds, paste0(filename, '_ztrax_residential_fpa_250m_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., fpa_250m, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        summarise(res_build_up_count = n(),
                  res_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_fpa_250m_res_rds,  paste0(filename, '_ztrax_residential_fpa_250m_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax residential dataframes ############################
  rdss <- list.files(dir_fpa_250m_res_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_250m_decades <- pblapply(rdss,
                                       FUN = function(x, dir_cleaned_fpa_250m_res_rds, fpa_250m_years) {
                                         require(tidyverse)
                                         
                                         filename <- strsplit(x, "\\.|/|_") %>%
                                           lapply(`[`, 12) %>%
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
                                         
                                         cleaned_fpa_250m_decades <- ungroup(imported) %>%
                                           filter(yearbuilt != 0) %>%
                                           mutate(fpa_id = factor(fpa_id)) %>%
                                           group_by(fpa_id, yearbuilt) %>%
                                           summarise(res_build_up_count = sum(res_build_up_count),
                                                     res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%  
                                           mutate(res_build_up_count = cumsum(res_build_up_count),
                                                  res_build_up_intensity_sqm = cumsum(res_build_up_intensity_sqm)) %>%
                                           filter(yearbuilt > 1991) %>%
                                           group_by(fpa_id, yearbuilt) %>%
                                           summarise(res_build_up_count = max(res_build_up_count),
                                                     res_build_up_intensity_sqm = max(res_build_up_intensity_sqm)) %>%                                      ungroup() %>%
                                           full_join(., blk_grps, by = c('fpa_id', 'yearbuilt')) %>%
                                           arrange(fpa_id, yearbuilt) %>%
                                           fill(everything(), .direction = 'up')  %>%
                                           left_join(., fpa_250m_years, by = c('fpa_id', 'yearbuilt')) %>%
                                           mutate(res_build_up_count = ifelse(year_built == discovery_year, res_build_up_count, NA),
                                                  res_build_up_intensity_sqm = ifelse(year_built == discovery_year, res_build_up_intensity_sqm, NA)) %>%
                                           filter(!is.na(res_build_up_count) | !is.na(res_build_up_intensity_sqm)) 
                                         
                                         
                                         if(nrow(cleaned_fpa_250m_decades) > 0) {
                                           cleaned_fpa_250m_decades %>%
                                             write_rds(., file.path(dir_cleaned_fpa_250m_res_rds, paste0(filename, '_cleaned_fpa_250m_residential.rds')))
                                         } 
                                       },
                                       dir_cleaned_fpa_250m_res_rds = dir_cleaned_fpa_250m_res_rds,
                                       fpa_250m_years = fpa_250m_years,
                                       cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_250m_decades <- do.call(rbind, cleaned_fpa_250m_decades) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, res_build_up_count, res_build_up_intensity_sqm)
  
  cleaned_fpa_250m_decades %>%
    write_rds(., file.path(dir_cleaned_fpa_250m_res_rds, 'all_cleaned_fpa_250m_residential.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_250m_decades <- read_rds(file.path(dir_cleaned_fpa_250m_res_rds, 'all_cleaned_fpa_250m_residential.rds'))
}                            


# Combine and explore -----------------------------------------------------

cleaned_fpa_decades %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count),
            res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = log(res_build_up_count))) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_fpa_250m_decades %>%
  mutate(quant = quantile(res_build_up_count, probs = 0.9999)) %>%
  filter(res_build_up_count < quant) %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count),
            res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = res_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 
