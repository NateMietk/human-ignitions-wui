intersect_ztrax <- function(x, mask, usa) {
  
  require(sf)
  require(tidyverse)
  
  filename <- x %>%
    basename() %>%
    str_extract(., "[[:digit:]]+")
  
  bae_state <- usa %>%
    filter(statefp == filename) %>%
    dplyr::select(statefp) %>%
    st_intersection(., mask)
  
  if(nrow(bae_state) != 0) {
    
    ztrax_pts <- st_read(x) %>%
      st_intersection(., bae_state)
    
    if(nrow(ztrax_pts) == 0) {
      
      cleaned <- bae_state %>%
        as.data.frame() %>%
        rename_all(tolower) %>%
        dplyr::select(fpa_id) %>%
        mutate(built_class = NA_character_,
               yearbuilt = 0,
               build_up_count = 0,
               build_up_intensity_sqm = 0) %>%
        as_tibble()
      
    } else {
      imported <- ztrax_pts %>%
        rename_all(tolower) %>%
        as.data.frame() %>%
        dplyr::mutate(yearbuilt = ifelse(yearbuilt == 0, 0,
                                         ifelse(yearbuilt != 0 & yearbuilt <= 1990, 1990, yearbuilt))) %>%
        dplyr::group_by(fpa_id, built_class, yearbuilt) %>%
        dplyr::summarise(build_up_count = n(),
                         build_up_intensity_sqm = sum(bdareasqft)*0.092903) %>%
        dplyr::ungroup()
      
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
        fill(everything(), .direction = 'down') %>%
        dplyr::ungroup()
    }
  return(cleaned)
  }
}
