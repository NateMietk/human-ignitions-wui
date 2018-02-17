
usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  dplyr::select(STUSPS)
names(usa_shp) %<>% tolower

states <- as(usa_shp, "Spatial")
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))

# Import the Level 3 Ecoregions
ecoreg <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  dplyr::select(US_L3CODE, US_L3NAME, NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME) %>%
  st_intersection(., usa_shp) %>%
  mutate(region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                     "TROPICAL WET FORESTS",
                                                     "NORTHERN FORESTS"), "East",
                                    if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                             "SOUTHERN SEMIARID HIGHLANDS",
                                                             "TEMPERATE SIERRAS",
                                                             "MEDITERRANEAN CALIFORNIA",
                                                             "NORTHWESTERN FORESTED MOUNTAINS",
                                                             "MARINE WEST COAST FOREST"), "West", "Central"))),
         regions = as.factor(if_else(region == "East" & stusps %in% c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", "TX", "OK"), "South East",
                                     if_else(region == "East" & stusps %in% c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "RI", "CT", "MI", "MD", 
                                                                              "MA", "WI", "IL", "IN", "OH", "WV", "VA", "KY", "MO", "IA", "MN"), "North East",
                                             as.character(region)))))
names(ecoreg) %<>% tolower

ecoreg_swse <- ecoreg %>%
  group_by(regions) %>%
  summarize() 

ecoregs <- as(ecoreg_swse, "Spatial")
ecoregs$id <- row.names(ecoregs)
ec_df <- fortify(ecoregs, region = 'id')
ec_df <- left_join(ec_df, ecoregs@data, by = 'id')
names(ec_df) <- tolower(names(ec_df))

p1 <- ggplot() +
  geom_polygon(data = ec_df, 
               aes(x = long,y = lat,group=group), 
               color='black', fill = "gray99", size = .1)+
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))
ggsave(file = "figs/draft/main_text/regions.eps", p1, width = 1.75, height = 1, dpi=1200) #saves g

# Create Fishnets ---------------------------------------------------------
# 50k Fishnet
fishnet_50k <- st_make_grid(usa_shp, cellsize = 50000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fishid50k' = 1:length(.))) %>%
  st_intersection(., st_union(usa_shp))

# 25k Fishnet
fishnet_25k <- st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fishid25k' = 1:length(.))) %>%
  st_intersection(., st_union(usa_shp))

fishnet_25k <- as(st_centroid(fishnet_25k), "Spatial")
fs25_df <- SpatialPointsDataFrame(fishnet_25k, fishnet_25k@data)
fs25_df$id <- row.names(fs25_df)
fs25_df <- data.frame(fs25_df)

# 10k Fishnet
fishnet_10k <- st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
  st_sf('geometry' = ., data.frame('fishid10k' = 1:length(.))) %>%
  st_intersection(., st_union(usa_shp))

# Intersects the region
bounds <- st_intersection(usa_shp, ecoreg) %>%
  st_intersection(., fishnet_50k) %>%
  st_intersection(., fishnet_25k) %>%
  st_intersection(., fishnet_10k)

#Import the Wildland-Urban Interface and process
if(!file.exists(file.path(anthro_out, "wui_bounds.gpkg"))) {
  wui <- st_read(dsn = file.path(wui_prefix, "us_wui_2010.gdb"),
             layer = "us_wui_2010") %>%
    st_simplify(., preserveTopology = TRUE, dTolerance = 0.001) %>%
    mutate(Class = classify_wui(WUICLASS10)) %>%
    filter(Class %in% c("Urban" ,"WUI", "VLD", "Wildlands")) %>%
    st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
    st_make_valid() 
  wui_bounds <- bounds %>%
    st_make_valid() %>%
    st_intersection(., wui) %>%
    st_make_valid() %>%
    mutate(wui_area_m2 = as.numeric(st_area(geometry))/1000000)
  names(wui) %<>% tolower
  
  st_write(wui_bounds, file.path(wui_out, "wui_bounds.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else { 
  wui <- st_read(dsn = file.path(anthro_out, "wui_bounds.gpkg")) %>%
    mutate(wui_area_km2 = as.numeric(st_area(geom))/1000000) }

# Clean the FPA database class
if(!file.exists(file.path(fpa_out, "fpa_conus.gpkg"))) {
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
    st_make_valid() %>%
    st_transform(st_crs(usa_shp)) %>%
    st_intersection(., st_union(usa_shp))
  names(fpa_fire) %<>% tolower
  
  st_write(fpa_fire, file.path(fpa_out, "fpa_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  fpa_fire <- st_read(dsn = file.path(fpa_out, "fpa_conus.gpkg")) %>%
    st_transform(st_crs(usa_shp)) %>%
    st_intersection(., st_union(usa_shp))
}

# Spatially join the fpa database to the WUI
if(!file.exists(file.path(fpa_out, "fpa_wui_conus.gpkg"))) {
  fpa_wui <- fpa_fire %>%
    st_intersection(., wui) %>%
    st_intersection(., state_eco_fish) %>%
    st_make_valid() 
  names(fpa_wui) %<>% tolower
  
  st_write(fpa_wui, file.path(fpa_out, "fpa_wui_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  fpa_wui <- st_read(dsn = file.path(fpa_out, "fpa_wui_conus.gpkg")) 
}

#Clean and prep the MTBS data to match the FPA database naming convention
if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
  mtbs_fire <- st_read(dsn = mtbs_prefix,
                     layer = "mtbs_perims_1984-2015_DD_20170815", quiet= TRUE) %>%
    st_transform(st_crs(usa_shp)) %>%
    filter(Year >= 1992) %>%
    mutate(MTBS_ID = Fire_ID,
           MTBS_FIRE_NAME = Fire_Name,
           MTBS_DISCOVERY_YEAR = Year,
           MTBS_DISCOVERY_DAY = day(ig_date),
           MTBS_DISCOVERY_MONTH = month(ig_date),
           MTBS_DISCOVERY_DOY = yday(ig_date),
           RADIUS = "NA") %>%
    dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DOY, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR) %>%
    merge(., as.data.frame(fpa_fire), by = c("MTBS_ID", "MTBS_FIRE_NAME"), all = FALSE) %>%
    dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                  MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, MTBS_DISCOVERY_DOY, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH,
                  MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS)  %>%
    st_make_valid()
  names(mtbs_fire) %<>% tolower
  
  st_write(mtbs_fire, file.path(mtbs_out, "mtbs_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  mtbs_fire <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg")) %>%
    mutate(RADIUS = "NA") %>%
    st_par(., st_transform, n_cores = ncores, crs = "+init=epsg:2163")
}

# Spatially join the MTBS to WUI
if (!file.exists(file.path(mtbs_out, "mtbs_wui.gpkg"))) {
  mtbs_wui <- mtbs_fire %>%
    st_intersection(., wui) %>%
    st_make_valid() %>%
    st_intersection(., state_eco_fish) %>%
    st_make_valid() %>%
    mutate(ClArea_m2 = as.numeric(st_area(Shape)),
           ClArea_km2 = ClArea_m2/1000000)
  names(mtbs_wui) %<>% tolower
  
  st_write(mtbs_wui, file.path(mtbs_out, "mtbs_wui.gpkg"),
           driver = "GPKG",
           update=TRUE)
} else {
  mtbs_wui <- st_read(dsn = file.path(mtbs_out, "mtbs_wui.gpkg"))
}
