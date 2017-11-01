# Load helper functions for external script
source("src/functions/helper_functions.R")
source("src/functions/make_grid.R")

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  dplyr::select(STUSPS)

names(usa_shp) %<>% tolower

states <- as(usa_shp, "Spatial")
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))

# Dissolve to the USA Boundary
conus <- usa_shp %>%
  st_union()

# Import the Level 3 Ecoregions
ecoreg <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  dplyr::select(US_L3CODE, US_L3NAME, NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME)
names(ecoreg) %<>% tolower

# Create Fishnets ---------------------------------------------------------
# 50k Fishnet
fishnet_50k <- st_make_grid(usa_shp, cellsize = 50000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fish50' = 1:length(.))) %>%
  st_intersection(., conus)

# 25k Fishnet
fishnet_25k <- st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fish25' = 1:length(.))) %>%
  st_intersection(., conus)

# 10k Fishnet
fishnet_10k <- st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fish10' = 1:length(.))) %>%
  st_intersection(., conus)

hex_grid_50k <- make_grid(as(conus, "Spatial"), type = "hexagonal", cell_width = 50000,
                          cell_area = 1250000000, clip = FALSE) %>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex50 = row_number())

hex_grid_25k <- make_grid(as(conus, "Spatial"), type = "hexagonal", cell_width = 25000,
                          cell_area = 625000000, clip = FALSE) %>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex25 = row_number())

# Try to summarize distance from WUI using hexagonal
hex_grid_400k <- make_grid(as(conus, "Spatial"), type = "hexagonal",
                          cell_area = 1000000000000, clip = TRUE)%>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex400 = row_number())

# Intersects the region
bounds <- st_intersection(usa_shp, ecoreg) %>%
  st_intersection(., fishnet_50k) %>%
  st_intersection(., fishnet_25k) %>%
  st_intersection(., fishnet_10k) %>%
  st_intersection(., hex_grid_400k) %>%
  st_intersection(., hex_grid_50k) %>%
  st_intersection(., hex_grid_25k)

#Import the Wildland-Urban Interface and process
wui <- st_read(dsn = file.path(wui_prefix, "us_wui_2010.gdb"),
               layer = "us_wui_2010") %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 0.001) %>%
  mutate(Class = classify_wui(WUICLASS10)) %>%
  filter(Class %in% c("Urban" ,"WUI", "VLD", "Wildlands")) %>%
  st_transform("+init=epsg:2163") %>%
  st_make_valid()

if (!file.exists(file.path(wui_out, "wui_conus.gpkg"))) {
  st_write(wui, file.path(wui_out, "wui_conus.gpkg"),
         driver = "GPKG",
         update=TRUE)}

wui <- st_read(dsn = file.path(wui_out, "wui_conus.gpkg")) %>%
  mutate(area_km2 = as.numeric(st_area(geom))/1000000)

wui_bounds <- bounds %>%
  st_make_valid() %>%
  st_intersection(., wui) %>%
  st_make_valid() %>%
  mutate(wui_area_m2 = as.numeric(st_area(geometry))/1000000)

if (!file.exists(file.path(wui_out, "wui_bounds.gpkg"))) {
  st_write(wui_bounds,
           file.path(wui_out, "wui_bounds.gpkg"),
         driver = "GPKG",
         update=TRUE)}

# Clean the FPA database class
fpa_fire <- st_read(dsn = file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb"),
                     layer = "Fires", quiet= FALSE) %>%
  filter(!(STATE %in% c("Alaska", "Hawaii", "Puerto Rico") & FIRE_SIZE >= 0.1)) %>%
  dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME,
                FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_DESCR, FIRE_SIZE, STATE) %>%
  mutate(IGNITION = ifelse(STAT_CAUSE_DESCR == "Lightning", "Lightning", "Human"),
         FIRE_SIZE_m2 = FIRE_SIZE*4046.86,
         FIRE_SIZE_km2 = FIRE_SIZE_m2/1000000,
         FIRE_SIZE_ha = FIRE_SIZE_m2*10000,
         DISCOVERY_DAY = day(DISCOVERY_DATE),
         DISCOVERY_MONTH = month(DISCOVERY_DATE),
         DISCOVERY_YEAR = FIRE_YEAR)  %>%
  st_make_valid()
fpa_fire <- st_transform(fpa_fire, "+init=epsg:2163")

if (!file.exists(file.path(fpa_out, "fpa_conus.gpkg"))) {
  st_write(fpa_fire, file.path(fpa_out, "fpa_conus.gpkg"),
         driver = "GPKG",
         update=TRUE)}

fpa_wui <- fpa_fire %>%
  st_intersection(., wui) %>%
  st_intersection(., state_eco_fish) %>%
  st_make_valid()

if (!file.exists(file.path(fpa_out, "fpa_wui_conus.gpkg"))) {
  st_write(fpa_wui, file.path(fpa_out, "fpa_wui_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)}

#Clean and prep the MTBS data to match the FPA database naming convention

mtbs_fire <- st_read(dsn = mtbs_prefix,
                     layer = "mtbs_perims_1984-2015_DD_20170815", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%
  filter(Year >= 1992) %>%
  mutate(MTBS_ID = Fire_ID,
         MTBS_FIRE_NAME = Fire_Name,
         MTBS_DISCOVERY_YEAR = Year,
         MTBS_DISCOVERY_DAY = day(ig_date),
         MTBS_DISCOVERY_MONTH = month(ig_date),
         MTBS_DISCOVERY_DOY = yday(ig_date)) %>%
  dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DOY, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR) %>%
  merge(., as.data.frame(fpa_fire), by = c("MTBS_ID", "MTBS_FIRE_NAME"), all = FALSE) %>%
  dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, MTBS_DISCOVERY_DOY, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH,
                MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION)  %>%
  st_make_valid()

if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
  st_write(mtbs_fire, file.path(mtbs_out, "mtbs_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)}

mtbs_wui <- mtbs_fire %>%
   st_intersection(., wui) %>%
  st_make_valid() %>%
  st_intersection(., state_eco_fish) %>%
  st_make_valid() %>%
  mutate(ClArea_m2 = as.numeric(st_area(Shape)),
         ClArea_km2 = ClArea_m2/1000000)

if (!file.exists(file.path(mtbs_out, "mtbs_wui.gpkg"))) {
  st_write(mtbs_wui, file.path(mtbs_out, "mtbs_wui.gpkg"),
           driver = "GPKG",
           update=TRUE)}
