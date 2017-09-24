# This script is the first step in the WUI project.
# Here we import, project, intersect, organize data layers
# Key layers are the Short ignitions, Radeloff WUI product, MTBS data

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(rgdal)
library(sf)
library(lubridate)

# Load helper functions for external script
source("src/R/helper_functions.R")
source("src/R/get_data.R")

# Prepare output directories
bounds_crt <- file.path("data", "bounds")
conus_crt <- file.path(bounds_crt, "conus")
ecoreg_crt <- file.path(bounds_crt, "ecoregion")
anthro_crt <- file.path("data", "anthro")
fire_crt <- file.path("data", "fire")

us_out <- file.path(conus_crt, "cb_2016_us_state_20m")
ecoregion_out <- file.path(bounds_crt, "ecoregion", "us_eco_l3")
wui_out <- file.path("data", "anthro", "us_wui_2010")
fpa_out <- file.path("data", "fire", "fpa-fod")
mtbs_out <- file.path("data", "fire", "mtbs_fod_perimeter_data")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(bounds_crt, conus_crt, ecoreg_crt, anthro_crt, fire_crt,
                us_out, ecoregion_out, wui_out, fpa_out, mtbs_out)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

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
         Area_FishID25k_km2 = Area_FishID25k_m2/1000000)

# 10k Fishnet
fishnet_10k <- st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('FishID10k' = 1:length(.))) %>%
  st_intersection(., conus) %>%
  mutate(Area_FishID10k_m2 = as.numeric(st_area(geometry)),
         Area_FishID10k_km2 = Area_FishID10k_m2/1000000)

# Intersects the region
state_eco_fish <- st_intersection(usa_shp, ecoreg) %>%
  dplyr::select(STUSPS, NAME, StArea_km2, US_L3CODE, US_L3NAME, EcoArea_km2, NA_L2NAME, NA_L1CODE, NA_L1NAME, geometry) %>%
  st_intersection(., fishnet_50k) %>%
  st_intersection(., fishnet_25k) %>%
  st_intersection(., fishnet_10k)

#Import the Wildland-Urban Interface and process
wui <- st_read(dsn = wui_gdb,
               layer = "us_wui_2010") %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 0.001) %>%
  mutate(Class = classify_wui(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  st_transform("+init=epsg:2163") %>%
  st_make_valid()

if (!file.exists(file.path(wui_out, "wui_conus.gpkg"))) {
  st_write(wui, file.path(wui_out, "wui_conus.gpkg"),
         driver = "GPKG",
         update=TRUE)}

wui_state_eco <- st_intersection(state_eco_fish, wui) %>%
  mutate(Area_km2 = (as.numeric(st_area(geometry))/1000000))

if (!file.exists(file.path(wui_out, "wui_state_eco.gpkg"))) {
  st_write(wui_state_eco, file.path(wui_out, "wui_state_eco.gpkg"),
         driver = "GPKG",
         update=TRUE)}

# Clean the FPA database class
fpa_fire <- st_read(dsn = file.path(fpa_gdb),
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
         DISCOVERY_YEAR = FIRE_YEAR)
fpa_fire <- st_transform(fpa_fire, "+init=epsg:2163")

if (!file.exists(file.path(fpa_out, "fpa_conus.gpkg"))) {
  st_write(fpa_fire, file.path(fpa_out, "fpa_conus.gpkg"),
         driver = "GPKG",
         update=TRUE)}

fpa_wui <- fpa_fire %>%
  filter(DISCOVERY_YEAR >= 2001) %>%
  st_intersection(., wui_state_eco)

if (!file.exists(file.path(fpa_out, "fpa_wui_conus.gpkg"))) {
  st_write(fpa_fire, file.path(fpa_out, "fpa_wui_conus.gpkg"),
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
         MTBS_DISCOVERY_DAY = StartDay,
         MTBS_DISCOVERY_MONTH = StartMonth,
         MTBS_DISCOVERY_DOY = yday(as.Date(paste(Year, StartMonth, StartDay, sep = "-")))) %>%
  dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DOY, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR) %>%
  merge(., as.data.frame(fpa_fire), by = c("MTBS_ID", "MTBS_FIRE_NAME"), all = FALSE) %>%
  dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, MTBS_DISCOVERY_DOY, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH,
                MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION)

if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
  st_write(mtbs_fire, file.path(mtbs_out, "mtbs_conus.gpkg"), 
           driver = "GPKG",
           update=TRUE)}

# Create the distance variable to create the simple buffers
fpa_bae <- fpa_fire %>%
  filter(DISCOVERY_YEAR >= 2001) %>%
  st_transform("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") %>% #USA_Contiguous_Equidistant_Conic
  mutate(radius = sqrt(FIRE_SIZE_m2/pi))

# Buffer FPA points based on radius, remove MTBS present in FPA, replace with the actual MTBS polygons
fpa_bae <- fpa_bae %>%
  st_buffer(., dist = fpa_bae$radius) %>%
  st_transform("+init=epsg:2163") %>%
  filter(MTBS_ID == "<NA>") %>%
  st_union(., mtbs_fire) %>%
  st_intersection(., wui) %>%
  st_intersection(., state_eco) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         ClArea_km2 = area_m2/1000000)

# Calculate the distance of each fire point to WUI boundary.
wui_only <- wui_state_eco %>%
  st_transform("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") %>% #USA_Contiguous_Equidistant_Conic
  filter(Class == "WUI") %>%
  group_by(group) %>%
  summarize()

fpa_fire_dist <- fpa_fire %>%
  st_transform("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") #USA_Contiguous_Equidistant_Conic

dist_tbl <- mapply(st_distance, st_geometry(fpa_fire_dist), st_geometry(wui_only)) %>%
  as.tibble() %>%
  mutate(dis_to_wui_m = value,
         dis_to_wui_km = (dis_to_wui_m)*0.001,
         FPA_ID = fpa_fire_dist$FPA_ID) %>%
  select(FPA_ID, dis_to_wui_m, dis_to_wui_km)

fpa_wui_dist <- fpa_fire_dist %>%
  right_join(., dist_tbl, by = "FPA_ID")
