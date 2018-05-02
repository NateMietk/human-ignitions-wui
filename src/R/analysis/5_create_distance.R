
# Calculate the distance of each fire point to Urban boundary.
if (!file.exists(file.path(wui_out, "high_den_urban_1990.gpkg"))) {
  urban_1990 <- wui %>%
    filter(Class90 == "High Urban") %>%
    st_geometry() %>%
    st_make_valid()  %>%
    st_transform(crs = proj_ed) %>%
    st_union()

  st_write(urban_1990, file.path(wui_out, "high_den_urban_1990.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
} else {
  urban_1990 <- st_read(file.path(wui_out, "high_den_urban_1990.gpkg"))
}

if (!file.exists(file.path(wui_out, "high_den_urban_2000.gpkg"))) {
  urban_2000 <- wui %>%
    filter(Class00 == "High Urban") %>%
    st_geometry() %>%
    st_make_valid()  %>%
    st_transform(crs = proj_ed) %>%
    st_union()

  st_write(urban_2000, file.path(wui_out, "high_den_urban_2000.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
} else {
  urban_2000 <- st_read(file.path(wui_out, "high_den_urban_2000.gpkg"))
}

if (!file.exists(file.path(wui_out, "high_den_urban_2010.gpkg"))) {
  urban_2010 <- wui %>%
    filter(Class10 == "High Urban") %>%
    st_geometry() %>%
    st_make_valid()  %>%
    st_transform(crs = proj_ed) %>%
    st_union()

  st_write(urban_2010, file.path(wui_out, "high_den_urban_2010.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
} else {
  urban_2010 <- st_read(file.path(wui_out, "high_den_urban_2010.gpkg"))
}

fpa_fire_ed <- fpa_wui %>%
  st_transform(proj_ed) %>%
  split(.$STATE)

if (!file.exists(file.path(wui_out, "fpa_urban_dist_1990.gpkg"))) {
  # Create distance to urban layers

  fpa_urban_dist_1990 <- fpa_fire_ed %>%
    filter(FIRE_YEAR < 2000) %>%
    dplyr::mutate(dis_to_urban_m = st_distance(st_geometry(urban_1990), st_geometry(.)),
                  dis_to_urban_km = (dis_to_urban_m)*0.001) %>%
    dplyr::select(FPA_ID, dis_to_urban_m, dis_to_urban_km)

  st_write(fpa_urban_dist_1990, file.path(wui_out, "fpa_urban_dist_1990.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))

} else {
  fpa_urban_dist_1990 <- st_read(file.path(wui_out, "fpa_urban_dist_1990.gpkg"))
}

if (!file.exists(file.path(wui_out, "fpa_urban_dist_2000.gpkg"))) {
  # Create distance to urban layers
  fpa_urban_dist_2000 <- fpa_fire_ed %>%
    filter(FIRE_YEAR >= 2000 | FIRE_YEAR < 2010) %>%
    mutate(dis_to_urban_m = st_distance(st_geometry(urban_2000), st_geometry(.)),
           dis_to_urban_km = (dis_to_urban_m)*0.001) %>%
    dplyr::select(FPA_ID, dis_to_urban_m, dis_to_urban_km)

  st_write(fpa_urban_dist_2000, file.path(wui_out, "fpa_urban_dist_2000.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))

} else {
  fpa_urban_dist_2000 <- st_read(file.path(wui_out, "fpa_urban_dist_2000.gpkg"))
}

if (!file.exists(file.path(wui_out, "fpa_urban_dist_2010.gpkg"))) {
  # Create distance to urban layers
  fpa_urban_dist_2010 <- fpa_fire_ed %>%
    filter(FIRE_YEAR >= 2010)  %>%
    mutate(dis_to_urban_m = st_distance(st_geometry(urban_2010), st_geometry(.)),
           dis_to_urban_km = (dis_to_urban_m)*0.001) %>%
    dplyr::select(FPA_ID, dis_to_urban_m, dis_to_urban_km)

  st_write(fpa_urban_dist_2010, file.path(wui_out, "fpa_urban_dist_2010.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))

} else {
  fpa_urban_dist_2010 <- st_read(file.path(wui_out, "fpa_urban_dist_2010.gpkg"))
}

urban_1990 <- urban_1990 %>%
  st_cast('POLYGON') %>%
  mutate(poly_ids = row_number())

urban_1990_centroid <- urban_1990 %>%
  st_centroid(.)

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c('fpa_fire_ed', 'urban_1990','urban_1990_centroid'))
sfSource('src/functions/helper_functions.R')

urban_1990_distance <- sfLapply(fpa_fire_ed,
                                function (input_list) {
                                  require(tidyverse)
                                  require(magrittr)
                                  require(lubridate)
                                  require(lubridate)
                                  require(sf)

                                  sub_grid <- dplyr:::bind_cols(input_list)
                                  unique_ids <- unique(sub_grid$FPA_ID)
                                  state_name <- unique(sub_grid$STATE)[1]

                                  print(paste0('Working on ', state_name))

                                  got_distance <- lapply(unique_ids,
                                                       FUN = get_distance,
                                                       polygons = urban_1990,
                                                       centroids = urban_1990_centroid)
                                  print(paste0('Finishing ', state_name))

                                  return(got_distance)
                                }
)

sfStop()
