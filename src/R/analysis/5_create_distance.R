
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
  urban_1990 <- st_read(file.path(wui_out, "high_den_urban_1990.gpkg")) %>%
    st_cast('POLYGON') %>%
    mutate(poly_ids = row_number())
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
  urban_2000 <- st_read(file.path(wui_out, "high_den_urban_2000.gpkg")) %>%
    st_cast('POLYGON') %>%
    mutate(poly_ids = row_number())
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
  urban_2010 <- st_read(file.path(wui_out, "high_den_urban_2010.gpkg")) %>%
    st_cast('POLYGON') %>%
    mutate(poly_ids = row_number())
}

fpa_fire_ed <- fpa_wui %>%
  dplyr::select(FPA_ID, DISCOVERY_YEAR, STATE) %>%
  mutate(DISCOVERY_YEAR = as.numeric(DISCOVERY_YEAR),
         decade = ifelse(DISCOVERY_YEAR < 2000, 1990,
                         ifelse(DISCOVERY_YEAR >= 2000 & DISCOVERY_YEAR < 2009, 2000, 2010))) %>%
  dplyr::select(-DISCOVERY_YEAR) %>%
  st_transform(proj_ed)

fpa_fire_ed$STATE <- droplevels(fpa_fire_ed$STATE)
fpa_fire_ed$FPA_ID <- droplevels(fpa_fire_ed$FPA_ID)

fpa_fire_ed <- fpa_fire_ed %>%
  split(.$decade)

urban_distance <- lapply(fpa_fire_ed,
                                function (input_list) {
                                  require(tidyverse)
                                  require(magrittr)
                                  require(lubridate)
                                  require(lubridate)
                                  require(sf)


                                  sub_grid <- dplyr:::bind_cols(input_list)
                                  unique_ids <- unique(sub_grid$FPA_ID)
                                  decades <- unique(sub_grid$decade)[1]

                                  get_polygons <- function(decades) {
                                    if (decades == '1990') {
                                      polygons <- urban_1990
                                    }  else if (decades == '2000') {
                                      polygons <- urban_2000
                                    } else  if (decades == '2010') {
                                      polygons <- urban_2010
                                    }
                                    polygons
                                  }

                                  polygons <- get_polygons(decades)
                                  centroids <- polygons %>%
                                    st_centroid(.)

                                  print(paste0('Working on ', decades))

                                  got_distance <- lapply(unique_ids,
                                                         FUN = get_distance,
                                                         points = sub_grid,
                                                         polygons = polygons,
                                                         centroids = centroids)
                                  print(paste0('Finishing ', decades))
                                  got_distance <- do.call(rbind, got_distance)

                                  return(got_distance)
                                }
)
