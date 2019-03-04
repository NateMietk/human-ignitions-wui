extract_ztrax <- function(shp_in, rst_in) {
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  
  shp_out <- foreach(i = unique(shp_in$FPA_ID), .combine = rbind, .packages = c('tidyverse', 'raster', 'sf', 'velox')) %dopar% {
    fire_event <- shp_in %>%
      filter(FPA_ID == i) 
    fire_buff <- fire_event %>% st_buffer(2000)
    fire_year <- fire_event$DISCOVERY_YEAR
    
    rst_event <- subset(rst_in,
                        which(getZ(rst_in) == fire_year)) %>%
      raster::crop(fire_buff)
    
    rst_extract <- velox::velox(rst_event)$extract(sp = fire_event, fun = function(x) sum(x, na.rm = TRUE),
                                                   small = TRUE, df = TRUE) %>%
      as_tibble() 
    # Rename to the raster layer names
    colnames(rst_extract) <- c('ID_sp', 'ztrax')
    
    fire_event_extract <- as.data.frame(rst_extract) %>%
      mutate(FPA_ID = as.data.frame(fire_event)$FPA_ID) %>%
      as_tibble() %>%
      dplyr::select(FPA_ID, ztrax)
    return(fire_event_extract)
  }
  parallel::stopCluster(cl)
  return(shp_out)
}
