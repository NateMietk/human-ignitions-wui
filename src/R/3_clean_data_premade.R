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
  st_sf('geometry' = ., data.frame('fishid25k' = 1:length(.))) %>%
  st_centroid(.) %>%
  st_intersection(., conus)

fishnet_25k <- as(fishnet_25k, "Spatial")
fs25_df <- SpatialPointsDataFrame(fishnet_25k, fishnet_25k@data)
fs25_df$id <- row.names(fs25_df)
fs25_df <- data.frame(fs25_df)

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

  wui <- st_read(dsn = file.path(wui_out, "wui_conus.gpkg")) %>%
  mutate(ClArea_m2 = as.numeric(st_area(geom)),
         ClArea_km2 = ClArea_m2/1000000)

wui_hex <- st_read(dsn = file.path(wui_out, "wui_state_eco_hex.gpkg")) %>%
  st_make_valid()

fpa_fire <- st_read(dsn = file.path(fpa_out, "fpa_conus.gpkg")) %>%
  mutate(MTBS_DISCOVERY_YEAR = "NA",
         MTBS_DISCOVERY_DOY = "NA",
         MTBS_DISCOVERY_MONTH = "NA",
         MTBS_DISCOVERY_DAY = "NA") %>%
  dplyr::select(-FIRE_YEAR, -DISCOVERY_DATE)

fpa_wui <- st_read(dsn = file.path(fpa_out, "fpa_wui_conus.gpkg"))

mtbs_fire <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg")) %>%
  mutate(RADIUS = "NA") %>%
  st_par(., st_transform, n_cores = ncores, crs = "+init=epsg:2163")

mtbs_wui <- st_read(dsn = file.path(mtbs_out, "mtbs_wui.gpkg"))

fpa_bae_wui <- st_read(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))

urban_only <- st_read(file.path(wui_out, "urban_only.gpkg"))
