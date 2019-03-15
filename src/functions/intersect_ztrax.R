intersect_ztrax <- function(x, donuts, usa = usa_shp, out_dir_cleaned, out_name_cleaned, 
                            out_dir, out_name, which_dataset, perimeter = FALSE) {
  require(sf)
  require(tidyverse)
  
  filename <- x %>%
    basename() %>%
    str_extract(., "[[:digit:]]+")
  
  key_state <- usa %>%
    filter(statefp == filename) %>%
    dplyr::select(statefp)
  
  row_index <- which(grepl(filename, usa$statefp))
  neighbors <- spdep::poly2nb(as(usa, "Spatial"), queen = FALSE) 
  neighbors <- usa[neighbors[[row_index]], "statefp"]
  
  key_state <- do.call('rbind', list(key_state, neighbors))
  unique_statefp <- unique(neighbors$statefp) %>%
    paste(collapse = "|")
  
  unique_states <- key_state %>%
    left_join(., as.data.frame(usa) %>% dplyr::select(-geometry) , by = 'statefp') 
  unique_states <-  unique(unique_states$stusps) %>%
    paste(collapse = ", ")
  
  neighbor_list <- list.files(dir_raw_ztrax_gpkg, pattern = unique_statefp, full.names = TRUE)
  
  ztrax_list <- lapply(neighbor_list, function(x) {
    dfs <- st_read(x)
    return(dfs)
  })
  
  ztrax_pts <- do.call('rbind', ztrax_list)
  
  if (which_dataset == '1') {
    if(!file.exists(file.path(out_dir, paste0(filename, out_name)))) {
      
      imported <- donuts %>%
        sf::st_join(., ztrax_pts, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        dplyr::mutate(yearbuilt = ifelse(yearbuilt == 0, 0,
                                         ifelse(yearbuilt != 0 & yearbuilt <= 1990, 1990, yearbuilt))) %>%
        dplyr::group_by(blk10, built_class, yearbuilt) %>%
        dplyr::summarise(build_up_count = n(),
                         build_up_intensity_sqm = sum(bdareasqft)*0.092903) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      imported %>%
        readr::write_rds(., file.path(out_dir,  paste0(filename, out_name)))
    } else {
      imported <- read_rds(file.path(out_dir,  paste0(filename, out_name)))
    }
    
    if(!file.exists(file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))) {
      
      cleaned <- imported %>%
        mutate(blk10 = factor(blk10)) %>%
        group_by(blk10, built_class, yearbuilt) %>%
        summarise(build_up_count = sum(build_up_count),
                  build_up_intensity_sqm = sum(build_up_intensity_sqm)) %>%
        mutate(build_up_count = cumsum(build_up_count),
               build_up_intensity_sqm = cumsum(build_up_intensity_sqm)) %>%
        ungroup() %>%
        complete(nesting(blk10, built_class), yearbuilt = c(0, 1990:2015)) %>%
        group_by(blk10, built_class) %>%
        fill(everything(), .direction = 'up') %>%
        ungroup() %>%
        group_by(blk10, built_class) %>%
        fill(everything(), .direction = 'down')
      
      cleaned %>%
        write_rds(., file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))
    } else {
      cleaned <- read_rds(file.path(out_dir_cleaned,  paste0(filename, out_name_cleaned)))
    }
    return(cleaned)
  }
  
  if (which_dataset == '2') {
    if (!file.exists(file.path(out_dir, paste0(filename, out_name)))) {
      
      imported <- donuts %>%
        sf::st_join(., ztrax_pts, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        dplyr::mutate(yearbuilt = ifelse(yearbuilt == 0, 0,
                                         ifelse(yearbuilt != 0 & yearbuilt <= 1990, 1990, yearbuilt))) %>%
        dplyr::group_by(fpa_id, built_class, yearbuilt) %>%
        dplyr::summarise(build_up_count = n(),
                         build_up_intensity_sqm = sum(bdareasqft)*0.092903) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      readr::write_rds(imported,
                       file.path(out_dir,  paste0(filename, out_name)))
    } else {
      imported <- read_rds(file.path(out_dir,  paste0(filename, out_name)))
    }
    
    if(!file.exists(file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))) {
      
      cleaned <- ungroup(imported) %>%
        mutate(fpa_id = factor(fpa_id)) %>%
        group_by(fpa_id, built_class, yearbuilt) %>%
        summarise(build_up_count = sum(build_up_count),
                  build_up_intensity_sqm = sum(build_up_intensity_sqm)) %>%
        mutate(build_up_count = cumsum(build_up_count),
               build_up_intensity_sqm = cumsum(build_up_intensity_sqm)) %>%
        ungroup() %>%
        complete(nesting(fpa_id, built_class), yearbuilt = c(0, 1990:2015)) %>%
        group_by(fpa_id, built_class) %>%
        fill(everything(), .direction = 'up') %>%
        ungroup() %>%
        group_by(fpa_id, built_class) %>%
        fill(everything(), .direction = 'down')
      
      cleaned %>%
        write_rds(., file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))
    } else {
      cleaned <- read_rds(file.path(out_dir_cleaned,  paste0(filename, out_name_cleaned)))
    }
    return(cleaned)
  }
  
  if (which_dataset == '3') {
    if (!file.exists(file.path(out_dir, paste0(filename, out_name)))) {
      
      imported <- donuts %>%
        sf::st_join(., ztrax_pts, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        dplyr::mutate(yearbuilt = ifelse(yearbuilt == 0, 0,
                                         ifelse(yearbuilt != 0 & yearbuilt <= 1999, 1999, yearbuilt))) %>%
        dplyr::group_by(incident_unique_id, built_class, yearbuilt) %>%
        dplyr::summarise(build_up_count = n(),
                         build_up_intensity_sqm = sum(bdareasqft)*0.092903) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      readr::write_rds(imported,
                       file.path(out_dir,  paste0(filename, out_name)))
    } else {
      imported <- read_rds(file.path(out_dir,  paste0(filename, out_name)))
    }
    
    if(!file.exists(file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))) {
      
      cleaned <- ungroup(imported) %>%
        mutate(incident_unique_id = factor(incident_unique_id)) %>%
        group_by(incident_unique_id, built_class, yearbuilt) %>%
        summarise(build_up_count = sum(build_up_count),
                  build_up_intensity_sqm = sum(build_up_intensity_sqm)) %>%
        mutate(build_up_count = cumsum(build_up_count),
               build_up_intensity_sqm = cumsum(build_up_intensity_sqm)) %>%
        ungroup() %>%
        complete(nesting(incident_unique_id, built_class), yearbuilt = c(0, 1999:2014)) %>%
        group_by(incident_unique_id, built_class) %>%
        fill(everything(), .direction = 'up') %>%
        ungroup() %>%
        group_by(incident_unique_id, built_class) %>%
        fill(everything(), .direction = 'down')
      
      cleaned %>%
        write_rds(., file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))
    } else {
      cleaned <- read_rds(file.path(out_dir_cleaned,  paste0(filename, out_name_cleaned)))
    }
    return(cleaned)
  }
  
  if (which_dataset == '4') {
    if (!file.exists(file.path(out_dir, paste0(filename, out_name)))) {
      
      imported <- donuts %>%
        sf::st_join(., ztrax_pts, join = st_intersects) %>%
        setNames(tolower(names(.))) %>%
        as.data.frame() %>%
        dplyr::mutate(yearbuilt = ifelse(yearbuilt == 0, 0,
                                         ifelse(yearbuilt != 0 & yearbuilt <= 1990, 1990, yearbuilt))) %>%
        dplyr::group_by(mtbs_id, built_class, yearbuilt) %>%
        dplyr::summarise(build_up_count = n(),
                         build_up_intensity_sqm = sum(bdareasqft)*0.092903) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      readr::write_rds(imported,
                       file.path(out_dir,  paste0(filename, out_name)))
    } else {
      imported <- read_rds(file.path(out_dir,  paste0(filename, out_name)))
    }
    
    if(!file.exists(file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))) {
      
      cleaned <- ungroup(imported) %>%
        mutate(mtbs_id = factor(mtbs_id)) %>%
        group_by(mtbs_id, built_class, yearbuilt) %>%
        summarise(build_up_count = sum(build_up_count),
                  build_up_intensity_sqm = sum(build_up_intensity_sqm)) %>%
        mutate(build_up_count = cumsum(build_up_count),
               build_up_intensity_sqm = cumsum(build_up_intensity_sqm)) %>%
        ungroup() %>%
        complete(nesting(mtbs_id, built_class), yearbuilt = c(0, 1990:2015)) %>%
        group_by(mtbs_id, built_class) %>%
        fill(everything(), .direction = 'up') %>%
        ungroup() %>%
        group_by(mtbs_id, built_class) %>%
        fill(everything(), .direction = 'down')
      
      cleaned %>%
        write_rds(., file.path(out_dir_cleaned, paste0(filename, out_name_cleaned)))
    } else {
      cleaned <- read_rds(file.path(out_dir_cleaned,  paste0(filename, out_name_cleaned)))
    }
    return(cleaned)
  }
  
}
