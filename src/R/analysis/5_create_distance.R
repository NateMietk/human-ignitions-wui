
# Calculate the distance of each fire point to Urban boundary.
if (!file.exists(file.path(wui_out, "high_den_urban_1990.gpkg"))) {
  urban_1990 <- wui %>%
    filter(Class90 == "High Urban") %>%
    st_geometry() %>%
    st_make_valid()  %>%
    st_transform(crs = proj_ed) %>%
    st_union()

  st_write(urban_1990,
           file.path(wui_out, "high_den_urban_1990.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
} else {
  urban_1990 <-
    st_read(file.path(wui_out, "high_den_urban_1990.gpkg")) %>%
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

  st_write(urban_2000,
           file.path(wui_out, "high_den_urban_2000.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
} else {
  urban_2000 <-
    st_read(file.path(wui_out, "high_den_urban_2000.gpkg")) %>%
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

  st_write(urban_2010,
           file.path(wui_out, "high_den_urban_2010.gpkg"),
           driver = "GPKG")
  system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
} else {
  urban_2010 <-
    st_read(file.path(wui_out, "high_den_urban_2010.gpkg")) %>%
    st_cast('POLYGON') %>%
    mutate(poly_ids = row_number())
}

fpa_fire_ed <- fpa_fire %>%
  dplyr::select(FPA_ID, DISCOVERY_YEAR, STATE) %>%
  mutate(
    DISCOVERY_YEAR = as.numeric(DISCOVERY_YEAR),
    decade = ifelse(
      DISCOVERY_YEAR < 2000,
      1990,
      ifelse(DISCOVERY_YEAR >= 2000 &
               DISCOVERY_YEAR < 2009, 2000, 2010)
    )
  ) %>%
  dplyr::select(-DISCOVERY_YEAR) %>%
  st_transform(proj_ed)

fpa_fire_ed$STATE <- droplevels(fpa_fire_ed$STATE)
fpa_fire_ed$FPA_ID <- droplevels(fpa_fire_ed$FPA_ID)
decades <- unique(fpa_fire_ed$decade)

for (i in decades) {
  if (!exists(file.path(distance_out, paste0('distance_fpa_', i, '.rds')))) {

    decade_df <- fpa_fire_ed %>%
      filter(decade == i)

    polygons <- get_polygons(i)
    urban_coords <- st_coordinates(polygons)
    fire_coords <- st_coordinates(decade_df)

    urban_df <- as_tibble(urban_coords) %>%
      mutate(vertex_ids = row_number())

    # compute KNN between fires and urban poly vertices
    nearest_neighbors <- as_tibble(bind_cols(nabor::knn(data = urban_coords[, c('X', 'Y')],
                                                        fire_coords,
                                                        k = 1))$nn.idx) %>%
      mutate(FPA_ID = as.data.frame(decade_df)$FPA_ID,
             vertex_ids = V1) %>%
      left_join(., urban_df, by = 'vertex_ids') %>%
      mutate(poly_ids = L2) %>%
      dplyr::select(FPA_ID, vertex_ids, poly_ids) %>%
      left_join(polygons, ., by = 'poly_ids') %>%
      na.omit()

    distance_to_fire <- decade_df %>%
      dplyr::select(-FPA_ID) %>%
      mutate(
        distance_to_urban = st_distance(
          st_geometry(nearest_neighbors),
          st_geometry(.), by_element = TRUE),
        FPA_ID = data.frame(decade_df)$FPA_ID)
  }

  write_rds(distance_to_fire,
            file.path(
              distance_out,
              paste0('distance_fpa_', i, '.rds')
            ))
  system(
    'aws s3 sync data/anthro/wui/distance_from_urban s3://earthlab-modeling-human-ignitions/anthro/wui/distance_from_urban'
  )
} else {
  rds_list <- list.files(distance_out,
                         full.names = TRUE,
                         pattern = '.rds')
  distance_rds <- lapply(rds_list, function(x) read_rds(x)) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    dplyr::select(FPA_ID, distance_to_urban) %>%
    left_join(fpa_wui, ., by = 'FPA_ID')

  write_rds(distance_rds,
            file.path(
              distance_out,
              paste0('distance_fpa.rds')
            ))
  system(
    'aws s3 sync data/anthro/wui/distance_from_urban s3://earthlab-modeling-human-ignitions/anthro/wui/distance_from_urban'
  )

}
