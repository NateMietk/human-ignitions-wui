wui <- st_read(dsn = file.path(anthro_out, "wui_bounds.gpkg")) %>%
  mutate(wui_area_km2 = as.numeric(st_area(geom))/1000000)

proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #USA_Contiguous_Equidistant_Conic
ncores <- detectCores()/2

wui_only <- wui %>%
  filter(Class == "WUI")  %>%
  st_transform(crs = proj_ed) %>%
  mutate(region = as.factor(if_else(na_l1name %in% c("EASTERN TEMPERATE FORESTS",
                                                     "GREAT PLAINS",
                                                     "TROPICAL WET FORESTS",
                                                     "NORTHERN FORESTS"), "East",
                                    if_else(na_l1name %in% c("NORTH AMERICAN DESERTS",
                                                             "SOUTHERN SEMIARID HIGHLANDS",
                                                             "TEMPERATE SIERRAS",
                                                             "MEDITERRANEAN CALIFORNIA",
                                                             "NORTHWESTERN FORESTED MOUNTAINS",
                                                             "MARINE WEST COAST FOREST"), "West", "Central"))),
         regions = as.factor(if_else(region == "East" & stusps %in% c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", "TX", "OK"), "South East",
                                     if_else(region == "East" & stusps %in% c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "RI", "CT", "MI", "MD",
                                                                              "MA", "WI", "IL", "IN", "OH", "WV", "VA", "KY", "MO", "IA", "MN"), "North East",
                                             as.character(region)))),
         region_dist = if_else(regions == "South East", 31,
                               if_else(regions == "North East", 72,
                                       if_else(regions == "West", 6, 34))))

names(wui_only) %<>% tolower

wui_expanded <- st_buffer(wui_only, dist = wui_only$region_dist) %>%
  st_transform("+init=epsg:2163") %>%
  mutate(wui_expanded_km2 = as.numeric(st_area(geom))/1000000)
