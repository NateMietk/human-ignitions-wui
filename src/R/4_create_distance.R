
# Calculate the distance of each fire point to Urban boundary.
if (!file.exists(file.path(wui_out, "fpa_urban_dist.gpkg"))) {
  urban_only <- wui %>%
    filter(class == "Urban") %>%
    st_make_valid()  %>%
    st_par(., st_transform, n_cores = ncores, crs = proj_ed) %>%
    group_by(Class) %>%
    summarize() 
  
  # Create distance to urban layers
  dist_tbl_urban <- st_distance(st_geometry(st_transform(fpa_fire, crs = proj_ed)), st_geometry(urban_only)) %>%
    as.tibble() %>%
    mutate(dis_to_urban_m = value,
           dis_to_urban_km = (dis_to_urban_m)*0.001,
           FPA_ID = fpa_fire$FPA_ID) %>%
    select(FPA_ID, dis_to_urban_m, dis_to_urban_km)
  
  fpa_urban_dist <- st_transform(fpa_fire, crs = proj_ed) %>%
    right_join(., dist_tbl_urban, by = "FPA_ID")
  
  st_write(fpa_urban_dist, file.path(wui_out, "fpa_urban_dist.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  fpa_urban_dist <- st_read(file.path(wui_out, "fpa_urban_dist.gpkg"))
}

# Calculate the distance of each fire point to WUI boundary.
if (!file.exists(file.path(wui_out, "fpa_wui_dist.gpkg"))) {
  wui_only <- wui %>%
    filter(Class == "WUI")  %>%
    st_transform(crs = proj_ed) 
  
  # Create distance to wui layers
  dist_tbl_wui <-  st_distance(st_geometry(st_transform(fpa_fire, crs = proj_ed)), st_geometry(wui_only)) %>%
    as.tibble() %>%
    mutate(dis_to_wui_m = value,
           dis_to_wui_km = (dis_to_wui_m)*0.001,
           FPA_ID = fpa_fire$FPA_ID) %>%
    select(FPA_ID, dis_to_wui_m, dis_to_wui_km)
  
  fpa_wui_dist <- st_transform(fpa_fire, crs = proj_ed) %>%
    right_join(., dist_tbl, by = "FPA_ID")
  
  st_write(fpa_wui_dist, file.path(wui_out, "fpa_wui_dist.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  fpa_wui_dist <- st_read(file.path(wui_out, "fpa_wui_dist.gpkg"))
}



