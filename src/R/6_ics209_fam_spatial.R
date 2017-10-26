source("src/R/ics209_fam_cleaning.R")

# Make the cleaned ICS-209 data spatial
fam_clean_pt <- st_as_sf(fam_clean, coords = c("long", "lat"),
                         crs = "+proj=longlat +datum=WGS84")

# Clip the ICS-209 data to the CONUS and remove unknown cause
conus_209 <- st_par(fam_clean_pt, st_intersection, n_cores = ncores, y = st_union(usa_shp))

erratics <- st_par(fam_clean_pt, st_difference, n_cores = ncores, y = st_union(usa_shp))

write_csv(conus_209, path = "data/ics209/output_tbls/ics209_conus.csv")
write_csv(erratics, path = "data/ics209/output_tbls/ics209_erratics.csv")

# Write out the shapefile.
st_write(conus_209, file.path(prefix, "anthro", "ics209_conus.gpkg"),
         driver = "GPKG",
         update=TRUE)

wui_shp <- st_read(dsn = file.path(prefix, "anthro", "wui_state_eco.gpkg"),
                   layer = "wui_state_eco", quiet= TRUE) %>%
  st_transform("+proj=longlat +datum=WGS84")

wui_209 <- st_intersection(conus_209, wui_shp)

# Write out the shapefile.
st_write(wui_209, file.path(prefix, "anthro", "ics209_wui_conus.gpkg"),
         driver = "GPKG",
         update=TRUE)


