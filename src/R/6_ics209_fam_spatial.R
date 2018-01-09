
# Clip the ICS-209 data to the CONUS and remove unknown cause
if(!file.exists(file.path(ics_spatial, "ics209_conus.gpkg"))) {
  # Make the cleaned ICS-209 data spatial
  fam_clean_pt <- st_par(fam_clean, st_as_sf, n_cores = ncores,
                         coords = c("long", "lat"),
                         crs = "+init=epsg:2163") %>%
    st_par(., st_transform, n_cores = ncores, crs = "+init=epsg:2163")
  conus_209 <- st_par(fam_clean_pt, st_intersection, n_cores = ncores, y = st_union(usa_shp)) %>%
    dplyr::select(-state) %>%
    st_par(., st_intersection, n_cores = ncores, y = state_ecoregion)
  
  fwrite(conus_209, file.path(ics_outtbls, "ics209_conus.csv", sep = ","))
  
  st_write(conus_209, file.path(ics_spatial, "ics209_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  conus_209 <- st_read(file.path(ics_spatial, "ics209_conus.gpkg"))
}

# Spatially join the 209 data to the WUI
if(!file.exists(file.path(ics_spatial, "ics209_wui_conus.gpkg"))) {
  wui_shp <- st_read(dsn = file.path(ics_spatial, "ics209_wui_conus.gpkg"),
                   layer = "wui_conus", quiet= TRUE) %>%
    st_transform(proj_ea)

  wui_209 <- st_intersection(conus_209, wui_shp)
  names(wui_209) %<>% tolower

  # Write out the shapefile.
  st_write(wui_209, file.path(ics_spatial, "ics209_wui_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else{
  wui_209 <- st_read(file.path(ics_spatial, "ics209_wui_conus.gpkg"))
}
