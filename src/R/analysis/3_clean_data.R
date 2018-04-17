
# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("usa_shp")){
  usa_shp <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                       dir = us_prefix,
                       layer = "cb_2016_us_state_20m",
                       outname = "usa") %>%
    filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
    st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
    dplyr::select(STUSPS) %>%
    setNames(tolower(names(.)))
}

# Import the Level 3 Ecoregions
if (!file.exists(file.path(ecoregion_out, 'us_eco_l3.gpkg'))) {

  # Download the Level 3 Ecoregions
  ecoregion_shp <- file.path(ecoregion_prefix, "us_eco_l3.shp")
  if (!file.exists(ecoregion_shp)) {
    loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
    dest <- paste0(ecoregion_prefix, ".zip")
    download.file(loc, dest)
    unzip(dest, exdir = ecoregion_prefix)
    unlink(dest)
    assert_that(file.exists(ecoregion_shp))
  }

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
                                               as.character(region))))) %>%
    setNames(tolower(names(.)))

  st_write(ecoreg, file.path(ecoregion_out, 'us_eco_l3.gpkg'))
  system(paste0("aws s3 sync ",
                bounds_crt, ' ',
                s3_bounds_prefix))
} else {
  ecoreg <- sf::st_read(file.path(ecoregion_out, 'us_eco_l3.gpkg'))
}

ecoreg_swse <- ecoreg %>%
  group_by(regions) %>%
  summarize()

# Create Fishnets ---------------------------------------------------------
# Create raster mask
# 50k Fishnet
if (!exists("fishnet_50k")) {
  if (!file.exists(file.path(fishnet_path, "fishnet_50k.gpkg"))) {
    fishnet_50k <- sf::st_make_grid(usa_shp, cellsize = 50000, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('fishid50k' = 1:length(.))) %>%
      sf::st_intersection(., st_union(usa_shp))

    sf::st_write(fishnet_50k,
                 file.path(fishnet_path, "fishnet_50k.gpkg"),
                 driver = "GPKG")

    system(paste0("aws s3 sync ",
                  fishnet_path, " ",
                  s3_bounds_prefix, "/fishnet"))
  } else {
    fishnet_50k <- sf::st_read(file.path(fishnet_path, "fishnet_50k.gpkg"))
  }
}

# 25k Fishnet
if (!exists("fishnet_25k")) {
  if (!file.exists(file.path(fishnet_path, "fishnet_25k.gpkg"))) {

    fishnet_25k <- sf::st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('fishid25k' = 1:length(.))) %>%
      sf::st_intersection(., st_union(usa_shp))

    sf::st_write(fishnet_25k,
                 file.path(fishnet_path, "fishnet_25k.gpkg"),
                 driver = "GPKG")

    system(paste0("aws s3 sync ",
                  fishnet_path, " ",
                  s3_bounds_prefix, "/fishnet"))
  } else {
    fishnet_25k <- sf::st_read(file.path(fishnet_path, "fishnet_25k.gpkg"))
  }
}

# 10k Fishnet
if (!exists("fishnet_10k")) {
  if (!file.exists(file.path(fishnet_path, "fishnet_10k.gpkg"))) {

    fishnet_10k <- sf::st_make_grid(usa_shp, cellsize = 10000, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('fishid10k' = 1:length(.))) %>%
      sf::st_intersection(., st_union(usa_shp))

    sf::st_write(fishnet_10k,
                 file.path(fishnet_path, "fishnet_10k.gpkg"),
                 driver = "GPKG")

    system(paste0("aws s3 sync ",
                  fishnet_path, " ",
                  s3_bounds_prefix, "/fishnet"))
  } else {
    fishnet_10k <- sf::st_read(file.path(fishnet_path, "fishnet_10k.gpkg"))
  }
}

# Intersects the region
if (!exists("bounds")) {
  if (!file.exists(file.path(bounds_crt, "eco_fish_bounds.gpkg"))) {
    bounds <- st_intersection(usa_shp, ecoreg) %>%
      st_intersection(., fishnet_50k) %>%
      st_intersection(., fishnet_25k) %>%
      st_intersection(., fishnet_10k)

    sf::st_write(bounds,
                 file.path(bounds_crt, "eco_fish_bounds.gpkg"),
                 driver = "GPKG")

    system(paste0("aws s3 sync ",
                  bounds_crt, " ",
                  s3_bounds_prefix))
  } else {
    bounds <- sf::st_read(file.path(bounds_crt, "eco_fish_bounds.gpkg"))
  }
}

#Import the Wildland-Urban Interface and process
if (!exists('wui')) {
  if (!file.exists(file.path(anthro_out, "wui_bounds.gpkg"))) {
    # st_layers(dsn = file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb"))

    wui <- st_read(dsn = file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb"),
                   layer = "CONUS_WUI_cp12") %>%
      st_make_valid() %>%
      st_simplify(., preserveTopology = TRUE, dTolerance = 0.001) %>%
      mutate(Class90 = classify_wui(WUICLASS90),
             Class00 = classify_wui(WUICLASS00),
             Class10 = classify_wui(WUICLASS10))  %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      st_make_valid()

    st_write(wui, file.path(wui_out, "wui_bounds.gpkg"),
             driver = "GPKG",
             update=TRUE)

    system(paste0("aws s3 sync ",
                  anthro_out, " ",
                  s3_anthro_prefix))
  } else {
    wui <- st_read(dsn = file.path(wui_out, "wui_bounds.gpkg"))
  }
}

# Clean the FPA database class
if (!exists('fpa_fire')) {
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
      st_transform(st_crs(usa_shp)) %>%
      st_intersection(., st_union(usa_shp))

    st_write(fpa_fire, file.path(fpa_out, "fpa_conus.gpkg"),
             driver = "GPKG",
             update=TRUE)

    system(paste0("aws s3 sync ",
                  fire_crt, " ",
                  s3_fire_prefix))
  } else {
    fpa_fire <- st_read(dsn = file.path(fpa_out, "fpa_conus.gpkg"))
  }
}

# Spatially join the fpa database to the WUI
if (!exists('fpa_wui')) {
  if(!file.exists(file.path(fpa_out, "fpa_wui_conus.gpkg"))) {
    fpa_wui <- fpa_fire %>%
      st_intersection(., wui) %>%
      st_intersection(., bounds) %>%
      st_make_valid()

    st_write(fpa_wui, file.path(fpa_out, "fpa_wui_conus.gpkg"),
             driver = "GPKG",
             update=TRUE)

    system(paste0("aws s3 sync ",
                  fire_crt, " ",
                  s3_fire_prefix))
  } else {
    fpa_wui <- st_read(dsn = file.path(fpa_out, "fpa_wui_conus.gpkg"))
  }
}

#Clean and prep the MTBS data to match the FPA database naming convention
if (!exists('mtbs_fire')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
    mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                       layer = "dissolve_mtbs_perims_1984-2015_DD_20170501", quiet= TRUE) %>%
      filter(Year >= 1992) %>%
      st_transform(st_crs(usa_shp)) %>%
      mutate(MTBS_ID = Fire_ID,
             MTBS_FIRE_NAME = Fire_Name,
             MTBS_DISCOVERY_YEAR = Year,
             MTBS_DISCOVERY_DAY = StartDay,
             MTBS_DISCOVERY_MONTH = StartMonth,
             MTBS_ACRES = Acres,
             RADIUS = "NA") %>%
      dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR, MTBS_ACRES, RADIUS) %>%
      merge(., as.data.frame(fpa_fire), by = c("MTBS_ID", "MTBS_FIRE_NAME"), all = FALSE) %>%
      dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME, MTBS_ACRES, FIRE_SIZE, FIRE_SIZE_m2, FIRE_SIZE_ha, FIRE_SIZE_km2,
                    MTBS_DISCOVERY_YEAR, DISCOVERY_YEAR, DISCOVERY_DOY, MTBS_DISCOVERY_MONTH, DISCOVERY_MONTH,
                    MTBS_DISCOVERY_DAY, DISCOVERY_DAY, STATE, STAT_CAUSE_DESCR, IGNITION, RADIUS)  %>%
      st_make_valid()

    st_write(mtbs_fire, file.path(mtbs_out, "mtbs_conus.gpkg"),
             driver = "GPKG",
             update=TRUE)

    system(paste0("aws s3 sync ",
                  fire_crt, " ",
                  s3_fire_prefix))
  } else {
    mtbs_fire <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg"))
  }
}

# Spatially join the MTBS to WUI
if (!file.exists(file.path(mtbs_out, "mtbs_wui.gpkg"))) {
  mtbs_wui <- mtbs_fire %>%
    st_intersection(., wui) %>%
    st_make_valid() %>%
    st_intersection(., bounds) %>%
    st_make_valid() %>%
    mutate(ClArea_m2 = as.numeric(st_area(Shape)),
           ClArea_km2 = ClArea_m2/1000000)

  st_write(mtbs_wui, file.path(mtbs_out, "mtbs_wui.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)

  system(paste0("aws s3 sync ",
                fire_crt, " ",
                s3_fire_prefix))
} else {
  mtbs_wui <- st_read(dsn = file.path(mtbs_out, "mtbs_wui.gpkg"))
}
