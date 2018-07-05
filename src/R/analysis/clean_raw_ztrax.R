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
  
  pblapply(gpkgs,
           FUN = intersect_ztrax,
           mask = wui,
           which_dataset = '1',
           out_dir = dir_wui_ztrax_rds,
           out_name = '_ztrax_wui.rds', 
           cl = cl)
  
  stopCluster(cl)
  
  # Aggregate and clean all ztrax all_build_up dataframes 
  rdss <- list.files(dir_wui_ztrax_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_wui <- pblapply(rdss,
                          FUN = aggregate_ztrax,
                          out_name = '_cleaned_wui_built_up.rds', 
                          which_dataset = '1',
                          out_dir = dir_cleaned_wui_ztrax_rds,
                          cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_wui_all <- do.call(rbind, cleaned_wui) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(blk10, year, built_up_class, build_up_count, build_up_intensity_sqm)
  
  cleaned_wui_all %>%
    write_rds(., file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
  
} else {
  
  cleaned_wui_all <- read_rds(file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_all_build_up.rds'))
}


if(!file.exists(file.path(dir_cleaned_fpaztraxrds, 'all_cleaned_fpa_all_built_up.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/7)
  registerDoParallel(cl)
  
  ztrax_all_built_up <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 18) %>%
      unlist
    
    if(!file.exists(file.path(dir_fpa_ztrax_rds, paste0(filename, '_ztrax_all_built_up_fpa_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., bae, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        summarise(abu_build_up_count = n(),
                  abu_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_fpa_ztrax_rds,  paste0(filename, '_ztrax_all_built_up_fpa_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax all_built_up dataframes ############################
  rdss <- list.files(dir_fpa_ztrax_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_decades_abu <- pblapply(rdss,
                                      FUN = function(x, dir_cleaned_fpa_ztrax_rds, fpa_years) {
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
                                          write_rds(., file.path(dir_cleaned_fpa_ztrax_rds, paste0(filename, '_cleaned_fpa_all_built_up.rds')))
                                      },
                                      dir_cleaned_fpa_ztrax_rds = dir_cleaned_fpa_ztrax_rds,
                                      fpa_years = fpa_years,
                                      cl = cl)
  
  stopCluster(cl)
  
  
  #bind all of these together in one dataframe
  cleaned_fpa_decades_abu <- do.call(rbind, cleaned_fpa_decades_abu) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, abu_build_up_count, abu_build_up_intensity_sqm)
  
  cleaned_fpa_decades_abu %>%
    write_rds(., file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_all_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_abu <- read_rds(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_all_built_up.rds'))
}

# All built up units per FPA with 250m buffer
if(!file.exists(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_all_built_up.rds'))) {
  ############## spatial join the wui dataframe to the zillow data ############################
  
  gpkgs <- list.files(dir_raw_ztrax_gpkg, pattern = ".gpkg", full.names = TRUE)
  
  cl <- makeCluster(detectCores()/6)
  registerDoParallel(cl)
  
  ztrax_all_built_up <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
    require(sf)
    require(tidyverse)
    
    filename <- strsplit(x, "\\.|/|_") %>%
      lapply(`[`, 18) %>%
      unlist
    
    if(!file.exists(file.path(dir_fpa_250m_ztrax_rds, paste0(filename, '_ztrax_all_built_up_fpa_250m_points.rds')))) {
      
      imported <- sf::st_read(x) %>%
        st_join(., fpa_250m, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        group_by(fpa_id, yearbuilt) %>%
        summarise(abu_build_up_count = n(),
                  abu_build_up_intensity_sqm = sum(bdareasqft)*0.092903) 
      
      write_rds(imported,  
                file.path(dir_fpa_250m_ztrax_rds,  paste0(filename, '_ztrax_all_built_up_fpa_250m_points.rds')))
    }
  }
  stopCluster(cl)
  
  ############## Aggregate and clean all ztrax all_built_up dataframes ############################
  rdss <- list.files(dir_fpa_250m_ztrax_rds, pattern = ".rds", full.names = TRUE)
  
  pboptions(type = 'txt', use_lb = TRUE)
  cl <- makeCluster(getOption("cl.cores", detectCores()))
  
  cleaned_fpa_250m_decades_abu <- pblapply(rdss,
                                           FUN = function(x, dir_cleaned_fpa_250m_ztrax_rds, fpa_250m_years) {
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
                                                 write_rds(., file.path(dir_cleaned_fpa_250m_ztrax_rds, paste0(filename, '_cleaned_fpa_250m_all_built_up.rds')))
                                             } 
                                           },
                                           dir_cleaned_fpa_250m_ztrax_rds = dir_cleaned_fpa_250m_ztrax_rds,
                                           fpa_250m_years = fpa_250m_years,
                                           cl = cl)
  
  stopCluster(cl)
  
  #bind all of these together in one dataframe
  cleaned_fpa_250m_decades_abu <- do.call(rbind, cleaned_fpa_250m_decades_abu) %>%
    na.omit()  %>%
    mutate(year = yearbuilt) %>%
    dplyr::select(fpa_id, year, abu_build_up_count, abu_build_up_intensity_sqm)
  
  cleaned_fpa_250m_decades_abu %>%
    write_rds(., file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_all_built_up.rds'))
  
  system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
  
} else {
  
  cleaned_fpa_250m_abu <- read_rds(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_all_built_up.rds'))
}        


# Combine and explore -----------------------------------------------------
clean_wui <- wui_df %>%
  dplyr::select(blk10, house_units_2000, house_units_2010) %>%
  gather(key = year, built_up_count = c('house_units_2000', 'house_units_2010'), -blk10) %>%
  mutate(blk10 = as.factor(blk10),
         year = str_sub(year, start= -4),
         built_up_count = `-blk10`,
         class2 = as.factor('Silvis')) %>%
  dplyr::select(-`-blk10`) %>%
  mutate(year = as.integer(year))

cleaned_wui_residential <- cleaned_wui_residential %>%
  filter(year %in% c(2000, 2010)) %>%
  mutate(blk10 = as.factor(blk10),
         class1 = 'Residential')


cleaned_wui_residential %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count)) %>%
  ggplot(aes(x = year, y = res_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE) +
  theme_pub() 

test <- clean_wui %>%
  left_join(., cleaned_wui_residential, by = c('blk10', 'year')) %>%
  # na.omit() %>%
  mutate(class1 = ifelse(is.na(class1), 'Residential', as.character(class1)),
         res_build_up_count = ifelse(is.na(res_build_up_count), 0, res_build_up_count)) %>%
  dplyr::select(-res_build_up_intensity_sqm, -class1, -class2) %>%
  gather('class', value = value, -blk10, -year) %>%
  mutate(built_up_class = ifelse(class == 'built_up_count', 'SILVIS', 'Residential'),
         build_up_count = value) %>%
  group_by(year, built_up_class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = year, y = build_up_count, group = built_up_class, fill = built_up_class)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE) +
  theme_pub() 

cleaned_fpa_250m_residential %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count),
            res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = (res_build_up_count))) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_fpa_residential %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count),
            res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = res_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_fpa_250m_abu %>%
  group_by(year) %>%
  summarise(abu_build_up_count = sum(abu_build_up_count),
            abu_build_up_intensity_sqm = sum(abu_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = (abu_build_up_count))) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_fpa_abu %>%
  group_by(year) %>%
  summarise(abu_build_up_count = sum(abu_build_up_count),
            abu_build_up_intensity_sqm = sum(abu_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = abu_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_wui_abu %>%
  group_by(year) %>%
  summarise(abu_build_up_count = sum(abu_build_up_count),
            abu_build_up_intensity_sqm = sum(abu_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = abu_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

################################################################################
clean_res_wui <- cleaned_wui_residential %>%
  mutate(build_up_count = res_build_up_count,
         build_up_intensity_sqm = res_build_up_intensity_sqm,
         built_up_class = as.factor('Residential')) %>%
  dplyr::select(-res_build_up_count, -res_build_up_intensity_sqm)

clean_res_fpa <- cleaned_fpa_residential %>%
  mutate(build_up_count = res_build_up_count,
         build_up_intensity_sqm = res_build_up_intensity_sqm,
         built_up_class = as.factor('Residential')) %>%
  dplyr::select(-res_build_up_count, -res_build_up_intensity_sqm)

clean_res_250 <- cleaned_fpa_250m_residential %>%
  mutate(build_up_count = res_build_up_count,
         build_up_intensity_sqm = res_build_up_intensity_sqm,
         built_up_class = as.factor('Residential')) %>%
  dplyr::select(-res_build_up_count, -res_build_up_intensity_sqm)

clean_abu_wui <- cleaned_wui_abu %>%
  mutate(build_up_count = abu_build_up_count,
         build_up_intensity_sqm = abu_build_up_intensity_sqm,
         built_up_class = as.factor('Non-Residential')) %>%
  dplyr::select(-abu_build_up_count, -abu_build_up_intensity_sqm)

clean_abu_fpa <- cleaned_fpa_abu %>%
  mutate(build_up_count = abu_build_up_count,
         build_up_intensity_sqm = abu_build_up_intensity_sqm,
         built_up_class = as.factor('Non-Residential')) %>%
  dplyr::select(-abu_build_up_count, -abu_build_up_intensity_sqm)

clean_abu_250 <- cleaned_fpa_250m_abu %>%
  mutate(build_up_count = abu_build_up_count,
         build_up_intensity_sqm = abu_build_up_intensity_sqm,
         built_up_class = as.factor('Non-Residential')) %>%
  dplyr::select(-abu_build_up_count, -abu_build_up_intensity_sqm)

bind_rows(clean_abu_wui, clean_res_wui) %>%
  group_by(year, built_up_class) %>%
  summarise(build_up_count = sum(build_up_count),
            build_up_intensity_sqm = sum(build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = build_up_count, fill = built_up_class, group = built_up_class)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_fpa_residential %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count),
            res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = res_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

cleaned_wui_residential %>%
  group_by(year) %>%
  summarise(res_build_up_count = sum(res_build_up_count),
            res_build_up_intensity_sqm = sum(res_build_up_intensity_sqm)) %>%
  ggplot(aes(x = year, y = res_build_up_count)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 


