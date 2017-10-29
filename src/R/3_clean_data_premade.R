# Load helper functions for external script
source("src/R/helper_functions.R")
ncores = detectCores()/2

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         StArea_km2 = area_m2/1000000,
         group = 1) %>%
  st_simplify(., preserveTopology = TRUE)

states_shp = readOGR(dsn=us_prefix, layer="cb_2016_us_state_20m")
states_shp <- spTransform(states_shp,
                          CRS("+init=epsg:2163"))

simple_state = rgeos::gSimplify(states_shp, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_state)/object.size(states_shp))[1]
states <- SpatialPolygonsDataFrame(simple_state, states_shp@data)
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))


states <- as(usa_shp, "Spatial")
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))

# Dissolve to the USA Boundary
conus <- usa_shp %>%
  group_by(group) %>%
  summarize()

# Import the Level 3 Ecoregions
ecoreg <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         EcoArea_km2 = area_m2/1000000)

# Create Fishnets ---------------------------------------------------------
# 50k Fishnet
fishnet_50k <- st_make_grid(usa_shp, cellsize = 50000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('FishID50k' = 1:length(.))) %>%
  st_intersection(., conus) %>%
  mutate(Area_FishID50k_m2 = as.numeric(st_area(geometry)),
         Area_FishID50k_km2 = Area_FishID50k_m2/1000000)

# 25k Fishnet
fishnet_25k <- st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('FishID25k' = 1:length(.))) %>%
  st_intersection(., conus) %>%
  mutate(Area_FishID25k_m2 = as.numeric(st_area(geometry)),
         Area_FishID25k_km2 = Area_FishID25k_m2/1000000,
         id = row_number())

# 10k Fishnet
fishnet_10k <- st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('FishID_10k' = 1:length(.))) %>%
  st_intersection(., conus) %>%
  mutate(Area_FishID10k_m2 = as.numeric(st_area(geometry)),
         Area_FishID10k_km2 = Area_FishID10k_m2/1000000,
         id = row_number())

hex_grid_50k <- make_grid(as(conus, "Spatial"), type = "hexagonal", cell_width = 50000,
                          cell_area = 1250000000, clip = FALSE) %>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex50k_id = row_number()) %>%
  mutate(Area_Hex50k_m2 = as.numeric(st_area(geometry)),
         Area_HexID50k_km2 = Area_Hex50k_m2/1000000)

hex_grid_25k <- make_grid(as(conus, "Spatial"), type = "hexagonal", cell_width = 25000,
                          cell_area = 625000000, clip = FALSE) %>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex25k_id = row_number()) %>%
  mutate(Area_Hex25k_m2 = as.numeric(st_area(geometry)),
         Area_HexID25k_km2 = Area_Hex25k_m2/1000000)

# Intersects the region
state_eco_fish <- st_intersection(usa_shp, ecoreg) %>%
  dplyr::select(STUSPS, NAME, StArea_km2, US_L3CODE, US_L3NAME, EcoArea_km2,
                NA_L2NAME, NA_L1CODE, NA_L1NAME, geometry) %>%
  st_intersection(., fishnet_25k) %>%
  st_intersection(., fishnet_10k)


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
  select(-FIRE_YEAR, -DISCOVERY_DATE)

fpa_wui <- st_read(dsn = file.path(fpa_out, "fpa_wui_conus.gpkg"))

mtbs_fire <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg")) %>%
  mutate(RADIUS = "NA") %>%
  st_par(., st_transform, n_cores = ncores, crs = "+init=epsg:2163")

mtbs_wui <- st_read(dsn = file.path(mtbs_out, "mtbs_wui.gpkg"))

fpa_bae_wui <- st_read(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg"))

urban_only <- st_read(file.path(wui_out, "urban_only.gpkg"))
