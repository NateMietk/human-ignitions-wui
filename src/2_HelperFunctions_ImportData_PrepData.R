

# Import data from CSVs -------------------------------------------------------------
econorm <- read.csv('./data/EcoRegion_Normalization.csv', header = TRUE, stringsAsFactors = TRUE)
stnorm <- read.csv('./data/State_Normalization.csv', header=TRUE, stringsAsFactors = TRUE)

ICS <- read.csv('./data/PointShp/CONUS_wui_short_fishid_ICS_209.csv', header=TRUE, stringsAsFactors = TRUE) %>%
  mutate(Class = classify_new_categories(WUICLASS10))

wuw_short <- fread('./data/PointShp/CONUS_WUI_short_2001_2013_EcoSt_wFishID.csv', header = T, sep = ',', stringsAsFactors = TRUE)
wuw_short$FIRE_SIZE <- as.numeric(wuw_short$FIRE_SIZE)

wui <- fread('./data/PolyShp/us_wui_2010_laea_EcoSt.csv', header = T, sep = ',', stringsAsFactors = TRUE) %>%
  filter(State_Area_km2 != "NA" | EcReg_Area_km2 != "NA")

bae <- fread('./data/PolyShp/CONUS_BAE_wMTBS.csv', header = T, sep = ',', stringsAsFactors = TRUE)

#For the 4_NumFire_Per_Housing R script
regionselect <- fread('./data/PointShp/CONUS_short_dis_VLDH_Wildlands_FishID.csv', header = T, sep = ',', stringsAsFactors = TRUE) %>%
  mutate(NA_L1NAME = classify_reclass_lev1(NA_L1NAME)) %>%
  select(Region, NA_L1NAME) %>%
  group_by(NA_L1NAME) %>%
  summarise(Regions = first(Region)) %>%
  ungroup()

fishdis <- fread('./data/PointShp/CONUS_short_dis_VLDH_Wildlands_FishID.csv', header = T, sep = ',', stringsAsFactors = TRUE) %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10),
         NA_L1NAME = classify_reclass_lev1(NA_L1NAME)) %>%
  group_by(FishID_10k, NA_L1NAME, IGNITION) %>%
  summarise(Ave_NEAR_DIST = median(NEAR_DIST),
            fseason_lngth = IQR(DISCOVERY_DOY),
            Avg_DOY = mean(DISCOVERY_DOY),
            f_cnt = n()) %>%
  ungroup()

fishdis_reg <- fread('./data/PointShp/CONUS_short_dis_VLDH_Wildlands_FishID.csv', header = T, sep = ',', stringsAsFactors = TRUE) %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(FishID_10k, Region, IGNITION) %>%
  summarise(Ave_NEAR_DIST = median(NEAR_DIST),
            fseason_lngth = IQR(DISCOVERY_DOY),
            Avg_DOY = mean(DISCOVERY_DOY),
            f_cnt = n()) %>%
  ungroup()

fishdisbae <- fread('./data/PolyShp/CONUS_short_dis_BAE_FishID.csv', header = T, sep = ',', stringsAsFactors = TRUE) %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10),
         NA_L1NAME = classify_reclass_lev1(NA_L1NAME)) %>%
  group_by(FishID_10k, NA_L1NAME, IGNITION) %>%
  summarise(Ave_NEAR_DIST = median(NEAR_DIST),
            fseason_lngth = IQR(DISCOVERY_DOY),
            Min_DOY = min(DISCOVERY_DOY),
            Max_DOY = max(DISCOVERY_DOY),
            f_sum = sum(AREA_km2)) %>%
  ungroup()

fishdisbae_reg <- fread('./data/PolyShp/CONUS_short_dis_BAE_FishID.csv', header = T, sep = ',', stringsAsFactors = TRUE) %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(FishID_10k, Region, IGNITION) %>%
  summarise(Ave_NEAR_DIST = median(NEAR_DIST),
            fseason_lngth = IQR(DISCOVERY_DOY),
            Min_DOY = min(DISCOVERY_DOY),
            Max_DOY = max(DISCOVERY_DOY),
            f_sum = sum(AREA_km2)) %>%
  ungroup()


# Import Shapefiles from Geodatabase -------------------------------------------------------
require(rgdal)
# The input file geodatabase
fgdb = "data/GeoDBSE/Backgoundfiles.gdb"
# # List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
ecoreg1_shp = readOGR(dsn=fgdb,layer="Ecoregion_L1")

simple_ecoreg1 = rgeos::gSimplify(ecoreg1_shp, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_ecoreg1)/object.size(ecoreg1_shp))[1]
ecoreg1 <- SpatialPolygonsDataFrame(simple_ecoreg1, ecoreg1_shp@data)
ecoreg1$id <- row.names(ecoreg1)
er1_df <- fortify(ecoreg1, region = 'id')
er1_df <- left_join(er1_df, ecoreg1@data, by = 'id')
names(er1_df) <- tolower(names(er1_df))

# Read the feature class
conus_shp = readOGR(dsn=fgdb,layer="CONUS")

simple_conus = rgeos::gSimplify(conus_shp, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_conus)/object.size(conus_shp))[1]
conus <- SpatialPolygonsDataFrame(simple_conus, conus_shp@data)
conus$id <- row.names(conus)
cn_df <- fortify(conus, region = 'id')
cn_df <- left_join(cn_df, conus@data, by = 'id')
names(cn_df) <- tolower(names(cn_df))

# Read the feature class
states_shp = readOGR(dsn=fgdb,layer="State")

simple_state = rgeos::gSimplify(states_shp, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_state)/object.size(states_shp))[1]
states <- SpatialPolygonsDataFrame(simple_state, states_shp@data)
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))

# Read the feature class
fish_shp = readOGR(dsn=fgdb,layer="FishNet_50km_pt")

simple_fish = rgeos::gSimplify(fish_shp, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_fish)/object.size(fish_shp))[1]
fish <- SpatialPointsDataFrame(simple_fish, fish_shp@data)
fish$id <- row.names(fish)
fs_df <- data.frame(fish)

# Read the feature class
fish_shp25 = readOGR(dsn=fgdb,layer="FishNet_25km_pt")

simple_fish25 = rgeos::gSimplify(fish_shp25, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_fish25)/object.size(fish_shp25))[1]
fish25 <- SpatialPointsDataFrame(simple_fish25, fish_shp25@data)
fish25$id <- row.names(fish25)
fs25_df <- data.frame(fish25)

# Read the feature class
fish_shp10 = readOGR(dsn=fgdb,layer="FishNet_10km_pt")

simple_fish10 = rgeos::gSimplify(fish_shp10, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_fish10)/object.size(fish_shp10))[1]
fish10 <- SpatialPointsDataFrame(simple_fish10, fish_shp10@data)
fish10$id <- row.names(fish10)
fs10_df <- data.frame(fish10)

# Read the feature class
ecoregions = readOGR(dsn=fgdb,layer="Ecoregion")

simple_ecoregions = rgeos::gSimplify(ecoregions, tol = 1000, topologyPreserve = TRUE)
(object.size(simple_ecoregions)/object.size(ecoregions))[1]
ecoregions <- SpatialPolygonsDataFrame(simple_ecoregions, ecoregions@data)
ecoregions$id <- row.names(ecoregions)
er_df <- fortify(ecoregions, region = 'id')
er_df <- left_join(er_df, ecoregions@data, by = 'id')
names(er_df) <- tolower(names(er_df))

# WUI/Ecoregions/State areas ----------------------------------------------

area_grp_st <- wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, STUSPS, State_Area_km2) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup()  %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea)

area_grp_eco <- wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, US_L3NAME, EcReg_Area_km2) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea)

area_grp_fish50 <- wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, FishID_50k) %>%
  summarise(ClassArea_Fish = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea_Fish) %>%
  mutate(VLD = ifelse(is.na(VLD), 0, VLD),
         Wildlands = ifelse(is.na(Wildlands), 0, Wildlands),
         WUI = ifelse(is.na(WUI), 0, WUI))

area_grp_fish25 <- wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, FishID_25k) %>%
  summarise(ClassArea_Fish = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  spread(Class, ClassArea_Fish) %>%
  mutate(VLD = ifelse(is.na(VLD), 0, VLD),
         Wildlands = ifelse(is.na(Wildlands), 0, Wildlands),
         WUI = ifelse(is.na(WUI), 0, WUI))

# Prep key aggregate dataframes -------------------------------------------
wuw_eco_poly <- wuw_short %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         FireSize = classify_fire_size_cl(fire_size_km2),
         Class = classify_new_categories(WUICLASS10),
         Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

wuw_eco_bae <- bae %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10),
         Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_fish25, by = "FishID_25k")

wuw_eco_wui <- wui %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

wuw_eco_ICS <- wui_209 %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 
