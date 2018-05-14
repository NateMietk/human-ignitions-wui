
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
    decade_df <- subset(fpa_fire_ed, fpa_fire_ed$decade == i)

    polygons <- get_polygons(i)

    centroids <- polygons %>%
      st_centroid(.)

    nearest_neighbors <- as.tibble(knn(st_coordinates(centroids),
                                       st_coordinates(decade_df),
                                       k = 4)$nn.idx) %>%
      mutate(FPA_ID = as.data.frame(decade_df)$FPA_ID) %>%
      gather(var, poly_ids,-FPA_ID) %>%
      dplyr::select(-var)

    nearest_neighbors <- nearest_neighbors %>%
      left_join(., polygons, by = 'poly_ids') %>%
      st_sf()
    distance_to_fire_full <- list()

    state <- unique(decade_df$STATE)

    for (j in state) {
      state_df <- subset(decade_df, decade_df$STATE == j)
      state_df$STATE <- droplevels(state_df$STATE)
      distance_to_fire <- list()

      unique_ids <- unique(state_df$FPA_ID)
      total_ids <- length(unique_ids)

      print(paste0('Working on ', j , ' ', i))

      cl <- makeCluster(2)
      registerDoParallel(cl)

      distance_to_fire <- foreach (h = 1:length(unique_ids), .combine = 'rbind') %dopar% {
        fpa_ids <- unique_ids[h]
        fpa_df <- subset(state_df, state_df$FPA_ID == fpa_ids)

        closest_centroids <-
          subset(nearest_neighbors,
                 nearest_neighbors$FPA_ID == fpa_ids)

        distance_to_fire <- fpa_df %>%
          dplyr::select(-FPA_ID) %>%
          mutate(
            distance_to_urban = min(
              st_distance(
                st_geometry(closest_centroids),
                st_geometry(.),
                by_element = TRUE
              )
            ),
            FPA_ID = data.frame(fpa_df)$FPA_ID
          )
      }
      stopCluster(cl)

      distance_to_fire_full[[j]] <- do.call(rbind, distance_to_fire)
      distance_to_fire_statedf <- do.call(rbind, distance_to_fire)

      write_rds(distance_to_fire_statedf,
                file.path(
                  distance_state_out,
                  paste0('distance_fpa_', i, '_', j, '.rds')
                ))
      system(
        'aws s3 sync data/anthro/wui/distance_from_urban s3://earthlab-modeling-human-ignitions/anthro/wui/distance_from_urban'
      )

    }

    distance_to_fire_full <-
      do.call(rbind, distance_to_fire_full) # Convert to data frame format

    # save the final cleaned climate extractions joined with the fpa-fod database
    summary_name <-
      file.path(distance_out, paste0('distance_fpa_', i, '.rds'))
    write_rds(distance_to_fire_full, summary_name)

    # push to S3
    system(
      'aws s3 sync data/anthro/wui/distance_from_urban s3://earthlab-modeling-human-ignitions/anthro/wui/distance_from_urban'
    )
  }
}
