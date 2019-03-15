
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

set.seed(12345)
slim_fire <- fpa_fire %>%
  dplyr::sample_frac(0.1)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

sampled_points <- pblapply(unique(slim_fire$FPA_ID), FUN = function(x, fire, grid) {
  require(tidyverse)
  require(sf)
  split <- fire %>%
    dplyr::filter(FPA_ID == x) 
  
  joined <- split %>%
    st_join(., grid) %>%
    dplyr::select(FPA_ID, grid_id)
  
  grid_join <- grid %>%
    filter(grid_id == joined$grid_id)
  
  samp <- st_sample(grid_join, 1)
  if(st_geometry_type(samp) == "POINT") {
    samp_df <- samp %>%
      st_sf() %>%
      mutate(FPA_ID = as.data.frame(split)$FPA_ID)
  } else {
    print(x)
  }
  return(samp_df)
}, grid = grids, 
fire = slim_fire,
cl = cl)

stopCluster(cl)

full_sampled_points <- do.call('rbind', sampled_points) 




ztrax_pts