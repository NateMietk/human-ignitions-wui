# Load helper functions for external script
source("src/functions/helper_functions.R")
source("src/functions/make_grid.R")

ncores = detectCores()/2

# Import USA states
usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_5m", quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico",
                       "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                       "American Samoa", "Guam"))) %>%
  mutate(state.abv = STUSPS,
         state = NAME,
         state_km2 = as.numeric(st_area(geometry))/1000000) %>%
  select(state.abv, state, state_km2)

# Import the Level 1 Ecoregions
ecoreg1 <- st_read(dsn = ecoregion_prefix, layer = "NA_CEC_Eco_Level1", quiet= TRUE) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  st_buffer(0) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%
  st_intersection(., st_union(usa_shp)) %>%
  select(NA_L1CODE, NA_L1NAME) %>%
  mutate(ecoreg1.code = NA_L1CODE,
         ecoreg1.name = NA_L1NAME,
         ecoreg1_km2 = as.numeric(st_area(geometry))/1000000) %>%
  select(ecoreg1.code, ecoreg1.name, ecoreg1_km2)

# Import the Level 2 Ecoregions
ecoreg2 <- st_read(dsn = ecoregion_prefix, layer = "NA_CEC_Eco_Level2", quiet= TRUE) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  st_buffer(0) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%
  st_intersection(., st_union(usa_shp)) %>%
  select(NA_L2CODE, NA_L2NAME) %>%
  mutate(ecoreg2.code = NA_L2CODE,
         ecoreg2.name = NA_L2NAME,
         ecoreg2_km2 = as.numeric(st_area(geometry))/1000000) %>%
  select(ecoreg2.code, ecoreg2.name, ecoreg2_km2)

# Import the Level 3 Ecoregions
ecoreg3 <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  st_buffer(0) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%
  st_intersection(., st_union(usa_shp)) %>%
  select(US_L3CODE, US_L3NAME) %>%
  mutate(ecoreg3.code = US_L3CODE,
         ecoreg3.name = US_L3NAME,
         ecoreg3_km2 = as.numeric(st_area(geometry))/1000000) %>%
  st_par(., st_intersection, n_cores = ncores, y = st_union(usa_shp)) %>%
  select(ecoreg3.code, ecoreg3.name, ecoreg3_km2)

names(usa_shp) %<>% tolower
names(ecoreg1) %<>% tolower
names(ecoreg2) %<>% tolower
names(ecoreg3) %<>% tolower

state_ecoregion <- st_par(usa_shp, st_intersection, n_cores = ncores, y = ecoreg1) %>%
  st_par(., st_intersection, n_cores = ncores, y = ecoreg2) %>%
  st_par(., st_intersection, n_cores = ncores, y = ecoreg3)


usa <-as(usa_shp, "Spatial")
states <- SpatialPolygonsDataFrame(usa, usa@data)


# Create Fishnets ---------------------------------------------------------
# 50k Fishnet
fishnet_50k <- st_make_grid(usa_shp, cellsize = 50000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fish50' = 1:length(.))) %>%
  st_intersection(., st_union(usa_shp))

# 25k Fishnet
fishnet_25k <- st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fish25' = 1:length(.))) %>%
  st_intersection(., st_union(usa_shp))

# 10k Fishnet
fishnet_10k <- st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fish10' = 1:length(.))) %>%
  st_intersection(., st_union(usa_shp))

hex_grid_50k <- make_grid(as(st_union(usa_shp), "Spatial"), type = "hexagonal", cell_width = 50000,
                          cell_area = 1250000000, clip = FALSE) %>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex50 = row_number())

hex_grid_25k <- make_grid(as(st_union(usa_shp), "Spatial"), type = "hexagonal", cell_width = 25000,
                          cell_area = 625000000, clip = FALSE) %>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex25 = row_number())

# Try to summarize distance from WUI using hexagonal
hex_grid_400k <- make_grid(as(st_union(usa_shp), "Spatial"), type = "hexagonal",
                          cell_area = 1000000000000, clip = TRUE)%>%
  st_as_sf(hex_grid_c) %>%
  mutate(hex400 = row_number())

# Intersects the region
bounds <- st_intersection(usa_shp, ecoreg3) %>%
  st_intersection(., fishnet_50k) %>%
  st_intersection(., fishnet_25k) %>%
  st_intersection(., fishnet_10k) %>%
  st_intersection(., hex_grid_400k) %>%
  st_intersection(., hex_grid_50k) %>%
  st_intersection(., hex_grid_25k)

wui <- st_read(dsn = file.path(anthro_out, "wui_conus.gpkg")) %>%
    mutate(ClArea_m2 = as.numeric(st_area(geom)),
           ClArea_km2 = ClArea_m2/1000000)

wui_hex <- st_read(dsn = file.path(anthro_out, "wui_state_eco_hex.gpkg")) %>%
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

urban_only <- st_read(file.path(anthro_out, "urban_only.gpkg"))
