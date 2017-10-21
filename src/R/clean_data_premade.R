# Load helper functions for external script
source("src/R/helper_functions.R")
ncores <- detectCores(logical = FALSE)

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         StArea_km2 = area_m2/1000000,
         group = 1) %>%
  st_simplify(., preserveTopology = TRUE)

# Dissolve to the USA Boundary
conus <- usa_shp %>%
  group_by(group) %>%
  summarize()

# Import the Level 3 Ecoregions
ecoreg <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = "+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         EcoArea_km2 = area_m2/1000000)

# 25k Fishnet
fishnet_25k <- st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('FishID25k' = 1:length(.))) %>%
  st_par(., st_intersection, n_cores = ncores, y = conus) %>%
  mutate(Area_FishID25k_m2 = as.numeric(st_area(geometry)),
         Area_FishID25k_km2 = Area_FishID25k_m2/1000000)

# 10k Fishnet
fishnet_10k <- st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('FishID10k' = 1:length(.))) %>%
  st_par(., st_intersection, n_cores = ncores, y = conus) %>%
  mutate(Area_FishID10k_m2 = as.numeric(st_area(geometry)),
         Area_FishID10k_km2 = Area_FishID10k_m2/1000000)

# Intersects the region
state_eco_fish <- st_intersection(usa_shp, ecoreg) %>%
  dplyr::select(STUSPS, NAME, StArea_km2, US_L3CODE, US_L3NAME, EcoArea_km2,
                NA_L2NAME, NA_L1CODE, NA_L1NAME, geometry) %>%
  #st_intersection(., fishnet_50k) %>%
  st_par(., st_intersection, n_cores = ncores, y = fishnet_25k) %>%
  st_par(., st_intersection, n_cores = ncores, y = fishnet_10k)

wui <- st_read(dsn = file.path(wui_out, "wui_conus.gpkg")) %>%
  mutate(ClArea_m2 = as.numeric(st_area(geom)),
         ClArea_km2 = ClArea_m2/1000000)

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

bae <- st_read(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))

fpa_bae <- st_read(file.path(fpa_out, "fpa_mtbs_bae.gpkg"))

urban_only <- st_read(file.path(wui_out, "urban_only.gpkg"))
