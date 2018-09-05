
# Prep boundaries ---------------------------------------------------------
# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("usa_shp")){
  usa_shp <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                       dir = us_prefix,
                       layer = "cb_2016_us_state_20m",
                       outname = "usa") %>%
    filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
    st_transform(proj_ea) %>%  # e.g. US National Atlas Equal Area
    dplyr::select(STATEFP, STUSPS) %>%
    setNames(tolower(names(.)))
}

# Import the Level 1 Ecoregions
if (!exists('ecoregl1')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l1.gpkg'))) {
    
    # Download the Level 1 Ecoregions
    ecoregion_shp <- file.path(ecoregion_out, "NA_CEC_Eco_Level1.shp")
    if (!file.exists(ecoregion_shp)) {
      loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
      dest <- paste0(ecoregion_out, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = ecoregion_out)
      unlink(dest)
      assert_that(file.exists(ecoregion_shp))
    }
    
    ecoregl1 <- st_read(dsn = ecoregion_out, layer = "NA_CEC_Eco_Level1") %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      #st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
      st_make_valid() %>%
      st_intersection(., st_union(usa_shp)) %>%
      mutate(region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                         "TROPICAL WET FORESTS",
                                                         "NORTHERN FORESTS"), "East",
                                        if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                                 "SOUTHERN SEMI-ARID HIGHLANDS",
                                                                 "TEMPERATE SIERRAS",
                                                                 "MEDITERRANEAN CALIFORNIA",
                                                                 "NORTHWESTERN FORESTED MOUNTAINS",
                                                                 "MARINE WEST COAST FOREST"), "West", "Central")))) %>%
      setNames(tolower(names(.)))
    
    st_write(ecoregl1, file.path(ecoregion_out, 'us_eco_l1.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
    
    system(paste0("aws s3 sync ",
                  prefix, " ",
                  s3_base))
    
  } else {
    ecoregl1 <- sf::st_read(file.path(ecoregion_out, 'us_eco_l1.gpkg'))
  }
}

# Import the Level 3 Ecoregions
if (!exists('ecoreg')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l3.gpkg'))) {
    
    # Download the Level 3 Ecoregions
    ecoregion_shp <- file.path(ecoregion_out, "us_eco_l3.shp")
    if (!file.exists(ecoregion_shp)) {
      loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
      dest <- paste0(ecoregion_out, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = ecoregion_out)
      unlink(dest)
      assert_that(file.exists(ecoregion_shp))
    }
    
    ecoreg_plain <- st_read(dsn = ecoregion_out, layer = "us_eco_l3", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      dplyr::select(US_L3CODE, US_L3NAME, NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME) %>%
      st_make_valid() %>%
      st_intersection(., st_union(usa_shp)) %>%
      setNames(tolower(names(.)))
    
    ecoreg <- st_read(dsn = ecoregion_out, layer = "us_eco_l3", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      dplyr::select(US_L3CODE, US_L3NAME, NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME) %>%
      st_make_valid() %>%
      st_intersection(., usa_shp) %>%
      mutate(region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                         "TROPICAL WET FORESTS",
                                                         "NORTHERN FORESTS"), "East",
                                        if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                                 "SOUTHERN SEMI-ARID HIGHLANDS",
                                                                 "TEMPERATE SIERRAS",
                                                                 "MEDITERRANEAN CALIFORNIA",
                                                                 "NORTHWESTERN FORESTED MOUNTAINS",
                                                                 "MARINE WEST COAST FOREST"), "West", "Central"))),
             regions = as.factor(if_else(region == "East" & stusps %in% c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", "TX", "OK"), "South East",
                                         if_else(region == "East" & stusps %in% c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "RI", "CT", "MI", "MD",
                                                                                  "MA", "WI", "IL", "IN", "OH", "WV", "VA", "KY", "MO", "IA", "MN"), "North East",
                                                 as.character(region))))) %>%
      setNames(tolower(names(.)))
    
    st_write(ecoreg_plain, file.path(ecoregion_out, 'us_eco_plain.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
    st_write(ecoreg, file.path(ecoregion_out, 'us_eco_l3.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
  } else {
    ecoreg_plain <- sf::st_read( file.path(ecoregion_out, 'us_eco_plain.gpkg'))
    ecoreg <- sf::st_read(file.path(ecoregion_out, 'us_eco_l3.gpkg'))
  }
}

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
      st_intersection(., fishnet_25k) 
    
    sf::st_write(bounds,
                 file.path(bounds_crt, "eco_fish_bounds.gpkg"),
                 driver = "GPKG", delete_layer = TRUE)
    
    system(paste0("aws s3 sync ",
                  bounds_crt, " ",
                  s3_bounds_prefix))
  } else {
    bounds <- sf::st_read(file.path(bounds_crt, "eco_fish_bounds.gpkg"))
  }
}

# Prep WUI ---------------------------------------------------------
#Import the Wildland-Urban Interface and process
if (!exists('wui')) {
  if (!file.exists(file.path(wui_out, "wui_bounds.gpkg"))) {
    # st_layers(dsn = file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb"))
    
    wui <- st_read(dsn = file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb"),
                   layer = "CONUS_WUI_cp12") %>%
      st_make_valid() %>%
      st_simplify(., preserveTopology = TRUE, dTolerance = 0.001) %>%
      mutate(Class90 = classify_wui(WUICLASS90),
             Class00 = classify_wui(WUICLASS00),
             Class10 = classify_wui(WUICLASS10))  %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      st_make_valid() %>%
      st_intersection(usa_shp)
    
    st_write(wui, file.path(wui_out, "wui_bounds.gpkg"),
             driver = "GPKG",
             delete_layer = TRUE)
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
  } else {
    wui <- st_read(dsn = file.path(wui_out, "wui_bounds.gpkg"))
  }
}

if (!exists('wui_class_area')) {
  if (!file.exists(file.path(wui_out, "wui_class_area.rds"))) {
    wui_class_area <- wui %>%
      setNames(tolower(names(.))) %>%
      dplyr::select(fishid50k, fishid25k, us_l3name, stusps, class90, class00, class10) %>%
      mutate(total_class_area = as.numeric(st_area(geom))/1000000)
    
    write_rds(wui_class_area, file.path(wui_out, "wui_class_area.rds"))
  }
  wui_class_area <- read_rds(file.path(wui_out, "wui_class_area.rds"))
}

if (!exists('wui_fish25k_sum')) {
  if (!file.exists(file.path(wui_out, "wui_fish25k_sum.rds"))) {
    wui_fish25k_sum <- as.data.frame(wui_class_area) %>%
      dplyr::select(fishid25k, class90, class00, class10, total_class_area) %>%
      gather(key = tmp, 
             value = class, -fishid25k, -total_class_area) %>%
      mutate(class = as.factor(class),
             tmp = as.factor(tmp),
             year = case_when(tmp == 'class90' ~ as.integer(1990),
                              tmp == 'class00' ~ as.integer(2000),
                              tmp == 'class10' ~ as.integer(2010), TRUE ~ NA_integer_)) %>%
      dplyr::select(fishid25k, year, class, total_class_area) %>%
      group_by(fishid25k, year, class) %>%
      summarise(total_fishid25k_area = sum(total_class_area))
    
    write_rds(wui_fish25k_sum, file.path(wui_out, "wui_fish25k_sum.rds"))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
  wui_fish25k_sum <- read_rds(file.path(wui_out, "wui_fish25k_sum.rds"))
}

if (!exists('wui_fish50k_sum')) {
  if (!file.exists(file.path(wui_out, "wui_fish50k_sum.rds"))) {
    wui_fish50k_sum <- as.data.frame(wui_class_area) %>%
      dplyr::select(fishid50k, class90, class00, class10, total_class_area) %>%
      gather(key = tmp, 
             value = class, -fishid50k, -total_class_area) %>%
      mutate(class = as.factor(class),
             tmp = as.factor(tmp),
             year = case_when(tmp == 'class90' ~ as.integer(1990),
                              tmp == 'class00' ~ as.integer(2000),
                              tmp == 'class10' ~ as.integer(2010), TRUE ~ NA_integer_)) %>%
      dplyr::select(fishid50k, year, class, total_class_area) %>%
      group_by(fishid50k, year, class) %>%
      summarise(total_fishid50k_area = sum(total_class_area))
    
    write_rds(wui_fish50k_sum, file.path(wui_out, "wui_fish50k_sum.rds"))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
  wui_fish50k_sum <- read_rds(file.path(wui_out, "wui_fish50k_sum.rds"))
}

if (!exists('wui_ecol3name_sum')) {
  if (!file.exists(file.path(wui_out, "wui_ecol3name_sum.rds"))) {
    wui_ecol3name_sum <- as.data.frame(wui_class_area)  %>%
      dplyr::select(us_l3name, class90, class00, class10, total_class_area) %>%
      gather(key = tmp, 
             value = class, -us_l3name, -total_class_area) %>%
      mutate(class = as.factor(class),
             tmp = as.factor(tmp),
             year = case_when(tmp == 'class90' ~ as.integer(1990),
                              tmp == 'class00' ~ as.integer(2000),
                              tmp == 'class10' ~ as.integer(2010), TRUE ~ NA_integer_)) %>%
      dplyr::select(us_l3name, year, class, total_class_area) %>%
      group_by(us_l3name, year, class) %>%
      summarise(total_ecol3name_area = sum(total_class_area))
    
    write_rds(wui_ecol3name_sum, file.path(wui_out, "wui_ecol3name_sum.rds"))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
  wui_ecol3name_sum <- read_rds(file.path(wui_out, "wui_ecol3name_sum.rds"))
}

if (!exists('wui_stusps_sum')) {
  if (!file.exists(file.path(wui_out, "wui_stusps_sum.rds"))) {
    wui_stusps_sum <- as.data.frame(wui_class_area) %>%
      dplyr::select(stusps, class90, class00, class10, total_class_area) %>%
      gather(key = tmp, 
             value = class, -stusps, -total_class_area) %>%
      mutate(class = as.factor(class),
             tmp = as.factor(tmp),
             year = case_when(tmp == 'class90' ~ as.integer(1990),
                              tmp == 'class00' ~ as.integer(2000),
                              tmp == 'class10' ~ as.integer(2010), TRUE ~ NA_integer_)) %>%
      dplyr::select(stusps, year, class, total_class_area) %>%
      group_by(stusps, year, class) %>%
      summarise(total_stusps_area = sum(total_class_area))
    
    write_rds(wui_stusps_sum, file.path(wui_out, "wui_stusps_sum.rds"))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
  wui_stusps_sum <- read_rds(file.path(wui_out, "wui_stusps_sum.rds"))
}

if (!exists('wui_area')) {
  if (!file.exists(file.path(wui_out, "wui_areas.rds"))) {
    wuw_area <- as.data.frame(wui_class_area) %>%
      group_by(class90, class00, class10) %>%
      summarise(total_class_area = sum(total_class_area)) %>%
      gather(key = tmp, 
             value = class, -total_class_area) %>%
      mutate(class = as.factor(class),
             tmp = as.factor(tmp),
             year = case_when(tmp == 'class90' ~ as.integer(1990),
                              tmp == 'class00' ~ as.integer(2000),
                              tmp == 'class10' ~ as.integer(2010), TRUE ~ NA_integer_)) %>%
      dplyr::select(class, year, total_class_area) %>%
      group_by(class, year) %>%
      summarise(total_class_area = sum(total_class_area))
    
    write_rds(wuw_area, file.path(wui_out, 'wui_areas.rds'))
  } else {
    wuw_area <- read_rds(file.path(wui_out, 'wui_areas.rds'))
  }
}

if (!exists('wui_df')) {
  if (!file.exists(file.path(rmarkdown_files, "wui_df.rds"))) {
    
    wui_df <- wui %>%
      as.data.frame() %>%
      setNames(tolower(names(.))) %>%
      mutate(house_units_1990 = hhu1990,
             house_units_2000 = hhu2000,
             house_units_2010 = hhu2010,
             house_den_1990 = huden1990,
             house_den_2000 = huden2000,
             house_den_2010 = huden2000) %>%
      dplyr::select(-matches('(huden|hhuden|hu|hhu|shu|shuden|flag|wuiclass|veg|water|shape|geom)')) %>%
      as_tibble() %>%
      mutate( year = 0) %>%
      complete(
        nesting(blk10, statefp, stusps, pop1990, popden1990, pop2000, popden2000, pop2010, popden2010,
                class90, class00, class10, house_units_1990, house_units_2000, house_units_2010,
                house_den_1990, house_den_2000, house_den_2010),
        year = c(1990, 2000, 2010)) %>%
      mutate(year = as.integer(year),
             class = as.factor(case_when(year == 1990 ~ as.character(class90),
                                         year == 2000 ~ as.character(class00),
                                         year == 2010 ~ as.character(class10), TRUE ~ NA_character_)),
             house_units = case_when(year == 1990 ~ house_units_1990,
                                     year == 2000 ~ house_units_2000,
                                     year == 2010 ~ house_units_2010, TRUE ~ NA_integer_),
             number_of_persons = case_when(year == 1990 ~ pop1990,
                                           year == 2000 ~ pop2000,
                                           year == 2010 ~ pop2010, TRUE ~ NA_integer_),
             pop_den = case_when(year == 1990 ~ popden1990,
                                 year == 2000 ~ popden2000,
                                 year == 2010 ~ popden2010, TRUE ~ NA_real_),
             house_den = case_when(year == 1990 ~ house_den_1990,
                                   year == 2000 ~ house_den_2000,
                                   year == 2010 ~ house_den_2010, TRUE ~ NA_real_),
             class_coarse =  as.factor(ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                               ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class))))) %>%
      dplyr::select(blk10, year, statefp, stusps, class, class_coarse, house_units, number_of_persons, pop_den, house_den)
    
    write_rds(wui_df, file.path(rmarkdown_files, "wui_df.rds"))
    
    system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
    
  } else {
    wui_df <- read_rds(file.path(rmarkdown_files, "wui_df.rds"))
  }
}

coarse_wuw_area <- read_csv(file.path(wui_out, 'wui_areas.csv')) %>%
  dplyr::select(-X1) %>%
  gather(year, total_coarse_class_area, -Class) %>%
  mutate(decadal = as.factor((gsub('area_', '', year))),
         class = as.factor(Class),
         class_coarse =  as.factor(ifelse(class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                          ifelse(class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class))))) %>%
  dplyr::select(-year, -Class, -class) %>%
  group_by(decadal, class_coarse) %>%
  summarise(total_coarse_class_area = sum(total_coarse_class_area))

wuw <- wuw_area %>%
  left_join(., coarse_wuw_area, by = 'decadal')


# Prep FPA-FOD ---------------------------------------------------------
# Clean the FPA database class
if (!exists('fpa_fire')) {
  if(!file.exists(file.path(fire_pnt, "fpa_conus.gpkg"))) {
    fpa_fire <- st_read(dsn = file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb"),
                        layer = "Fires", quiet= FALSE) %>%
      filter(!(STATE %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
      dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME,
                    FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_DESCR, FIRE_SIZE, STATE) %>%
      filter(STAT_CAUSE_DESCR != 'Missing/Undefined') %>%
      mutate(IGNITION = ifelse(STAT_CAUSE_DESCR == "Lightning", "Lightning", "Human"),
             FIRE_SIZE_m2 = FIRE_SIZE*4046.86,
             FIRE_SIZE_km2 = FIRE_SIZE_m2/1000000,
             FIRE_SIZE_ha = FIRE_SIZE_m2*10000,
             DISCOVERY_DAY = day(DISCOVERY_DATE),
             DISCOVERY_MONTH = month(DISCOVERY_DATE),
             DISCOVERY_YEAR = FIRE_YEAR)  %>%
      st_transform(st_crs(usa_shp)) %>%
      st_intersection(., st_union(usa_shp))
    
    st_write(fpa_fire, file.path(fire_pnt, "fpa_conus.gpkg"),
             driver = "GPKG", delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  } else {
    fpa_fire <- st_read(dsn = file.path(fire_pnt, "fpa_conus.gpkg"))
  }
}

# Spatially join the fpa database to the WUI
if (!exists('fpa_wui')) {
  if(!file.exists(file.path(fire_pnt, "fpa_wui_conus.gpkg"))) {
    fpa_wui_step1 <- fpa_fire %>%
      st_intersection(., wui) %>%
      st_intersection(., bounds) %>%
      st_make_valid()
    fpa_wui <- fpa_wui_step1 %>%
      mutate(class = as.factor(ifelse(DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, as.character(Class90),
                                      ifelse(DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, as.character(Class00),
                                             ifelse(DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, as.character(Class10),
                                                    NA)))),
             ten_year = as.factor(ifelse(DISCOVERY_YEAR >= 1994 & DISCOVERY_YEAR <= 2004, '1994-2004',
                                         ifelse(DISCOVERY_YEAR >= 2005 & DISCOVERY_YEAR <= 2015, '2005-2015', NA))),
             bidecadal = as.factor(ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                                          ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                                                 ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                                        ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                                               ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                                      DISCOVERY_YEAR )))))),
             decadal = as.factor(ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 2000, 1990,
                                        ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2010, 2000,
                                               ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2010,
                                                      DISCOVERY_YEAR )))),
             number_of_persons = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POP1990,
                                         ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POP2000,
                                                 ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POP2010, NA))),
             pop_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POPDEN1990,
                               ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POPDEN2000,
                                       ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POPDEN2010, NA ))),
             house_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HUDEN1990,
                                 ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HUDEN2000,
                                         ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HUDEN2010, NA ))),
             house_units = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HHU1990,
                                   ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HHU2000,
                                           ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HHU2010, NA ))),
             class_coarse =  as.factor(ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                               ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))),
             seasons = as.factor(classify_seasons(DISCOVERY_DOY)),
             size = as.factor(classify_fire_size_cl(FIRE_SIZE_km2)),
             regions = as.factor(ifelse(regions == 'East', 'North East', as.character(regions)))) %>%
      setNames(tolower(names(.))) %>%
      dplyr::select(-stusps.1) %>%
      dplyr::select(-matches('(1990|2000|2010|00|90|s10|flag|wuiclass|veg|water|shape)')) %>%
      left_join(., wuw, by = c('class', 'class_coarse', 'decadal'))
    
    st_write(fpa_wui, file.path(fire_pnt, "fpa_wui_conus.gpkg"),
             driver = "GPKG", delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
    
  } else {
    fpa_wui <- st_read(dsn = file.path(fire_pnt, "fpa_wui_conus.gpkg"))
  }
}

# output dataframe for rmarkdown
if(!file.exists(file.path(rmarkdown_files, 'fpa_wui_df.rds'))) {
  as.data.frame(fpa_wui) %>%
    write_rds(file.path(rmarkdown_files, 'fpa_wui_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  fpa_wui_df <- read_rds(file.path(rmarkdown_files, 'fpa_wui_df.rds'))
}

# Prep MTBS ---------------------------------------------------------
#Clean and prep the MTBS data to match the FPA database naming convention
if (!exists('mtbs_fire')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
    mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                         layer = "dissolve_mtbs_perims_1984-2015_DD_20170501", quiet= TRUE) %>%
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
if (!exists('mtbs_wui')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_wui.gpkg"))) {
    mtbs_wui <- mtbs_fire %>%
      st_intersection(., wui) %>%
      st_make_valid() %>%
      st_intersection(., bounds) %>%
      st_make_valid() %>%
      mutate(ClArea_m2 = as.numeric(st_area(Shape)),
             ClArea_km2 = ClArea_m2/1000000,
             Class = ifelse(DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, as.character(Class90),
                            ifelse(DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, as.character(Class00),
                                   ifelse(DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, as.character(Class10),
                                          NA))))
    
    st_write(mtbs_wui, file.path(mtbs_out, "mtbs_wui.gpkg"),
             driver = "GPKG",
             delete_layer = TRUE)
    
    system(paste0("aws s3 sync ",
                  fire_crt, " ",
                  s3_fire_prefix))
  } else {
    mtbs_wui <- st_read(dsn = file.path(mtbs_out, "mtbs_wui.gpkg"))
  }
}

#Clean and prep the MTBS data
if (!exists('mtbs_pts')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
    
    # Download the MTBS fire polygons
    mtbs_shp <- file.path(mtbs_prefix, 'mtbs_fod_pts_20170501.shp')
    if (!file.exists(mtbs_shp)) {
      loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
      dest <- paste0(mtbs_prefix, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = mtbs_prefix)
      unlink(dest)
      assert_that(file.exists(mtbs_shp))
    }
    mtbs_pts <- st_read(dsn = mtbs_prefix,
                        layer = "mtbs_fod_pts_20170501", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%
      st_intersection(st_union(usa_shp)) %>%
      mutate(MTBS_ID = FIRE_ID,
             MTBS_FIRE_NAME = FIRENAME,
             MTBS_DISCOVERY_YEAR = FIRE_YEAR,
             MTBS_DISCOVERY_DAY = FIRE_DAY,
             MTBS_DISCOVERY_MONTH = FIRE_MON,
             MTBS_ACRES = R_ACRES,
             FIRE_BIDECADAL = ifelse(MTBS_DISCOVERY_YEAR > 1985 & MTBS_DISCOVERY_YEAR <= 1990, 1990,
                                     ifelse(MTBS_DISCOVERY_YEAR >= 1991 & MTBS_DISCOVERY_YEAR <= 1995, 1995,
                                            ifelse(MTBS_DISCOVERY_YEAR >= 1996 & MTBS_DISCOVERY_YEAR <= 2000, 2000,
                                                   ifelse(MTBS_DISCOVERY_YEAR >= 2001 & MTBS_DISCOVERY_YEAR <= 2005, 2005,
                                                          ifelse(MTBS_DISCOVERY_YEAR >= 2006 & MTBS_DISCOVERY_YEAR <= 2010, 2010,
                                                                 ifelse(MTBS_DISCOVERY_YEAR >= 2011 & MTBS_DISCOVERY_YEAR <= 2015, 2015,
                                                                        MTBS_DISCOVERY_YEAR))))))) %>%
      dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR, MTBS_ACRES, FIRE_BIDECADAL) %>%
      st_intersection(., ecoreg) %>%
      setNames(tolower(names(.)))
    
    st_write(mtbs_pts, file.path(mtbs_out, "mtbs_pts.gpkg"),
             driver = "GPKG")
    
    system(paste0("aws s3 sync ",
                  prefix, " ",
                  s3_base))
  } else {
    mtbs_pts <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg"))
  }
}

# Clean ICS-209 from 1999-2014 -----------------------------

if(!file.exists(file.path(ics_outtbls, 'ics209_1999_2014_incidents.rds'))) {
  ics_209 <- fread(file.path(ics_intbls, "ics209_1999_2014.csv")) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    as_tibble() %>%
    setNames(tolower(names(.))) %>%
    filter(inctyp_abbreviation == 'WF') %>%
    mutate(curr_incident_area = ifelse(curr_incident_area == 350082388, 3500, curr_incident_area))
  
  latlong_legacy <- read_csv(file.path(ics_latlong, "legacy_cleaned_ll.csv")) %>%
    setNames(tolower(names(.)))  %>%
    mutate(lat = ifelse(is.na(lat_c), 0, lat_c),
           long = ifelse(is.na(long_c), 0, long_c)) %>%
    dplyr::select(incident_unique_id, lat, long, confidence)
  
  latlong_historic <- read_csv(file.path(ics_latlong, "historical_cleaned_ll.csv")) %>%
    setNames(tolower(names(.)))  %>%
    mutate(lat = ifelse(is.na(lat_c), 0, lat_c),
           long = ifelse(is.na(long_c), 0, long_c)) %>%
    dplyr::select(incident_unique_id, lat, long, confidence)
  
  latlong_2014 <- read_csv(file.path(ics_latlong, '2014_cleaned_ll.csv') )%>%
    setNames(tolower(names(.)))  %>%
    mutate(lat = ifelse(is.na(c_lat), 0, c_lat),
           long = ifelse(is.na(c_long), 0, c_long)) %>%
    dplyr::select(incident_unique_id, lat, long, confidence)
  
  latlongs <- rbind(latlong_legacy, latlong_historic, latlong_2014) %>%
    group_by(incident_unique_id) %>%
    summarise(lat_c = max(lat),
              long_c = min(long)) %>%
    filter(lat_c != 0)
  
  ics_209_reports <- ics_209 %>%
    mutate(incident_unique_id = as.factor(incident_unique_id),
           incident_number = as.factor(incident_number),
           incident_name = as.factor(incident_name),
           lat = poo_latitude,
           long = poo_longitude,
           report_to_date = ymd(as.Date(report_to_date, origin='1970-01-01')),
           report_to_doy = ifelse(is.na(yday(report_to_date)), 0, yday(report_to_date)),
           report_to_month = ifelse(is.na(month(report_to_date)), 0, month(report_to_date)),
           report_to_day = ifelse(is.na(day(report_to_date)), 0, day(report_to_date)),
           report_to_year = ifelse(is.na(year(report_to_date)), 0, year(report_to_date)),
           
           area_measurement = if_else(area_measurement == "", 'Acres', area_measurement),
           area_ha = ifelse(area_measurement == "Square Miles", curr_incident_area*258.99903998855,
                            ifelse(area_measurement == "Acres", curr_incident_area*0.4046859376605,
                                   curr_incident_area)),
           area_km2 = area_ha*0.01,
           cause_binary = ifelse(cause == "H", "2",
                                 ifelse(cause == "O", "2",
                                        ifelse(cause =="L", "1", "0"))),
           confidence = "H") %>%
    mutate(est_final_costs = ifelse(is.na(est_im_cost_to_date), projected_final_im_cost, projected_final_im_cost),
           costs = ifelse(projected_final_im_cost == 0 & est_im_cost_to_date > 1, est_im_cost_to_date,
                          projected_final_im_cost)) %>%
    dplyr::select( -projected_final_im_cost,-poo_latitude, -poo_longitude,
                   -est_im_cost_to_date, -est_final_costs)
  
  ics_209_incidents_max <- ics_209_reports %>%
    group_by(incident_unique_id) %>%
    arrange(desc(incident_unique_id, report_to_date)) %>%
    summarise_at(
      .vars= vars(area_km2, costs),
      .funs =  max)
  
  ics_209_incidents <- ics_209_reports %>%
    group_by(incident_unique_id) %>%
    arrange(desc(incident_unique_id, report_to_date)) %>%
    summarise(incident_number = last(incident_number),
              incident_name = last(incident_name),
              lat = max(lat),
              long = min(long),
              cause = max(cause_binary),
              cause = ifelse(cause == "2", "Human",
                             ifelse(cause =="1", "Lightning", "Unk")),
              confidence = last(confidence),
              start_date = first(report_to_date),
              end_date = last(report_to_date)) %>%
    mutate(start_year = year(start_date),
           start_month = month(start_date),
           start_day = day(start_date),
           start_doy = yday(start_date),
           end_year = year(end_date),
           end_month = month(end_date),
           end_day = day(end_date),
           end_doy = yday(end_date)) %>%
    left_join(., ics_209_incidents_max, by = "incident_unique_id") %>%
    left_join(., latlongs,  by = "incident_unique_id") %>%
    mutate(lat = ifelse(is.na(lat_c), lat, lat_c),
           long = ifelse(is.na(long_c), long, long_c),
           long = ifelse(long > 0, -long, long)) %>%
    dplyr::select(-lat_c, -long_c, -confidence, -start_date, -end_date)
  
  write_rds(ics_209_incidents, file.path(ics_outtbls, 'ics209_1999_2014_incidents.rds'))
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  ics_209_incidents <- read_rds(file.path(ics_outtbls, 'ics209_1999_2014_incidents.rds'))
}

# Clip the ICS-209 data to the CONUS and remove unknown cause
if(!file.exists(file.path(ics_spatial, "ics209_conus.gpkg"))) {
  # Make the cleaned ICS-209 data spatial
  conus_209 <- st_as_sf(ics_209_incidents,
                        coords = c("long", "lat"),
                        crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
    st_transform(crs = proj_ea) %>%
    st_intersection(., st_union(usa_shp)) %>%
    st_intersection(., bounds)
  
  st_write(conus_209, file.path(ics_spatial, "ics209_conus.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  conus_209 <- st_read(file.path(ics_spatial, "ics209_conus.gpkg"))
}

# Spatially join the 209 data to the WUI
if(!file.exists(file.path(ics_spatial, "ics209_wui_conus.gpkg"))) {
  
  wui_209 <- st_intersection(conus_209, wui) %>%
    mutate(class = ifelse(start_year >= 1992 | start_year < 2000, as.character(Class90),
                          ifelse(start_year >= 2000 | start_year < 2009, as.character(Class00),
                                 ifelse(start_year >= 2010 | start_year < 2016, as.character(Class10),
                                        NA))),
           number_of_persons = ifelse( start_year >= 1992 | start_year < 2000, POP1990,
                                       ifelse( start_year >= 2000 | start_year < 2009, POP2000,
                                               ifelse( start_year >= 2010 | start_year < 2016, POP2010, NA))),
           pop_den = ifelse( start_year >= 1992 | start_year < 2000, POPDEN1990,
                             ifelse( start_year >= 2000 | start_year < 2009, POPDEN2000,
                                     ifelse( start_year >= 2010 | start_year < 2016, POPDEN2010, NA ))),
           house_den = ifelse( start_year >= 1992 | start_year < 2000, HUDEN1990,
                               ifelse( start_year >= 2000 | start_year < 2009, HUDEN2000,
                                       ifelse( start_year >= 2010 | start_year < 2016, HUDEN2010, NA ))),
           house_units = ifelse( start_year >= 1992 | start_year < 2000, HHU1990,
                                 ifelse( start_year >= 2000 | start_year < 2009, HHU2000,
                                         ifelse( start_year >= 2010 | start_year < 2016, HHU2010, NA ))),
           class_coarse =  ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                   ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class))),
           regions = ifelse(regions == 'East', 'North East', as.character(regions))) %>%
    setNames(tolower(names(.))) %>%
    dplyr::select(-stusps.1) %>%
    dplyr::select(-matches('(1990|2000|2010|00|90|s10|flag|wuiclass|veg|blk|water|shape)'))
  
  # Write out the shapefile.
  st_write(wui_209, file.path(ics_spatial, "ics209_wui_conus.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else{
  wui_209 <- st_read(file.path(ics_spatial, "ics209_wui_conus.gpkg"))
}

# output dataframe for rmarkdown
if(!file.exists(file.path(rmarkdown_files, 'wui_209_df.rds'))) {
  as.data.frame(wui_209) %>%
    write_rds(file.path(rmarkdown_files, 'wui_209_df.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
} else{
  wui_209_df <- read_rds(file.path(rmarkdown_files, "wui_209_df.rds"))
}
