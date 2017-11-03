proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #USA_Contiguous_Equidistant_Conic
ncores <- detectCores()/2

# Calculate the distance of each fire point to Urban boundary.
urban_only <- wui %>%
  filter(class == "Urban") %>%
  st_make_valid()  %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ed) %>%
  group_by(Class) %>%
  summarize()

wui_only <- wui %>%
  filter(Class == "WUI") %>%
  st_union()

wui_ed <- wui_only %>%
  st_transform(crs = proj_ed)

if (!file.exists(file.path(wui_out, "urban_only.gpkg"))) {
  st_write(urban_only, file.path(wui_out, "urban_only.gpkg"),
           driver = "GPKG",
           update=TRUE)}

if (!file.exists(file.path(wui_out, "wui_only.gpkg"))) {
  st_write(wui_only, file.path(wui_out, "wui_only.gpkg"),
           driver = "GPKG",
           update=TRUE)}

fpa_fire_dist <- fpa_fire %>%
  st_transform(crs = proj_ed)

# Create distance to urban layers
dist_tbl_urban <- st_distance(st_geometry(fpa_fire_dist), st_geometry(urban_only)) %>%
  as.tibble() %>%
  mutate(dis_to_urban_m = value,
         dis_to_urban_km = (dis_to_urban_m)*0.001,
         FPA_ID = fpa_fire_dist$FPA_ID) %>%
  select(FPA_ID, dis_to_urban_m, dis_to_urban_km)

fpa_urban_dist <- fpa_fire_dist %>%
  right_join(., dist_tbl_urban, by = "FPA_ID")

if (!file.exists(file.path(wui_out, "fpa_urban_dist.gpkg"))) {
  st_write(fpa_urban_dist, file.path(wui_out, "fpa_urban_dist.gpkg"),
           driver = "GPKG",
           update=TRUE)}

# Create distance to wui layers
dist_tbl_wui <-  st_distance(st_geometry(fpa_fire_dist), st_geometry(wui_only)) %>%
  as.tibble() %>%
  mutate(dis_to_wui_m = value,
         dis_to_wui_km = (dis_to_wui_m)*0.001,
         FPA_ID = fpa_fire_dist$FPA_ID) %>%
  select(FPA_ID, dis_to_wui_m, dis_to_wui_km)

fpa_wui_dist <- fpa_fire_dist %>%
  right_join(., dist_tbl, by = "FPA_ID")

if (!file.exists(file.path(wui_out, "fpa_wui_dist.gpkg"))) {
  st_write(fpa_wui_dist, file.path(wui_out, "fpa_wui_dist.gpkg"),
           driver = "GPKG",
           update=TRUE)}
