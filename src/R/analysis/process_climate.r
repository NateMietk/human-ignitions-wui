
bae_ll <- bae %>%
  setNames(tolower(names(.))) %>%
  left_join(., as.data.frame(fpa_wui_df) %>% dplyr::select(-geom, -discovery_year), by = 'fpa_id') %>%
  dplyr::select(fpa_id, discovery_year, discovery_month, stusps, -geom) %>%
  st_transform('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0') %>%
  na.omit(stusps)

state_id <- unique(bae_ll$stusps)

get_climate <- function(x, grid_names, velox_grid, polygons, var_name) {
  if(!file.exists(file.path("data/climate/states", paste0(x, '_bae_', var_name, '.rds' )))) {
    require(tidyverse)
    require(velox)

    sub_polygons <- polygons %>%
      dplyr::filter(stusps == x)
    print(paste0(x, ' = ', nrow(sub_polygons)))

    climate_df <- velox_grid$extract(sp = sub_polygons, fun = function(x) mean(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(climate_df) <- c('ID_sp', grid_names)
    climate_df <- climate_df   %>%
      mutate(fpa_id = as.data.frame(sub_polygons)$fpa_id,
             discovery_year = as.data.frame(sub_polygons)$discovery_year,
             discovery_month = as.data.frame(sub_polygons)$discovery_month,
             stusps = as.data.frame(sub_polygons)$stusps) %>%
      dplyr::select(-ID_sp) %>%
      gather(key = key, value = mean , -fpa_id, -discovery_year, -discovery_month, -stusps) %>%
      separate(key,
               into = c("variable", 'year', 'tmp'),
               sep = "_") %>%
      separate(tmp,
               into = c("tmp", 'month'),
               sep = "\\.") %>%
      mutate(fpa_id = as.factor(fpa_id),
             mean = ifelse(discovery_year == year & discovery_month == month, mean, NA),
             month = as.integer(month),
             variable = as.factor(variable)) %>%
      dplyr::select(-tmp, -year, -month) %>%
      na.omit()

    write_rds(climate_df, file.path("data/climate/states", paste0(x, '_bae_', var_name, '.rds' )))
    return(climate_df)

  } else {
    climate_df <- read_rds(file.path("data/climate/states", paste0(x, '_bae_', var_name, '.rds' )))
    return(climate_df)

  }
}

# Extract mean pdsi by fire bae
if(!exists('fpa_climate')) {
  if (!file.exists(file.path(climate_dir, 'fpa_climate.rds'))) {

  if (!exists('bae_pdsi_mean.rds')) {
    if (!file.exists(file.path(climate_dir, 'bae_pdsi_mean.rds'))) {

      pdsi_velox <- velox(pdsi_mean)

      pdsi_mean_list <- lapply(state_id,
                                 FUN = get_climate,
                                 grid_names = names(pdsi_mean),
                                 velox_grid = pdsi_velox,
                                 polygons = bae_ll,
                                 var_name = 'pdsi_mean')

      pdsi_mean_df <- do.call(rbind, pdsi_mean_list)

      write_rds(pdsi_mean_df, file.path(climate_dir, 'bae_pdsi_mean.rds'))
      system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

    } else {
      pdsi_mean_df <- read_rds(file.path(climate_dir, 'bae_pdsi_mean.rds'))
    }
  }

  # Extract anomalies pdsi by fire bae
  if (!exists('bae_pdsi_anomalies.rds')) {
    if (!file.exists(file.path(climate_dir, 'bae_pdsi_anomalies.rds'))) {

      pdsi_anom_velox <- velox(pdsi_anomalies)

      pdsi_anomalies_list <- lapply(state_id,
                               FUN = get_climate,
                               grid_names = names(pdsi_anomalies),
                               velox_grid = pdsi_anom_velox,
                               polygons = bae_ll,
                               var_name = 'pdsi_anomalies')

      pdsi_anomalies_df <- do.call(rbind, pdsi_anomalies_list)

      write_rds(pdsi_anomalies_df, file.path(climate_dir, 'bae_pdsi_anomalies.rds'))
      system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

    } else {
      pdsi_anomalies_df <- read_rds(file.path(climate_dir, 'bae_pdsi_anomalies.rds'))
    }
  }

  # Extract mean temp by fire bae
  if (!exists('bae_temp_mean.rds')) {
    if (!file.exists(file.path(climate_dir, 'bae_temp_mean.rds'))) {

      temp_velox <- velox(temp_mean)

      temp_mean_list <- lapply(state_id,
                               FUN = get_climate,
                               grid_names = names(temp_mean),
                               velox_grid = temp_velox,
                               polygons = bae_ll,
                               var_name = 'temp_mean')

      temp_mean_df <- do.call(rbind, temp_mean_list)

      write_rds(temp_mean_df, file.path(climate_dir, 'bae_temp_mean.rds'))
      system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

    } else {
      temp_mean_df <- read_rds(file.path(climate_dir, 'bae_temp_mean.rds'))
    }
  }

  # Extract anomalies temp by fire bae
  if (!exists('bae_temp_anomalies.rds')) {
    if (!file.exists(file.path(climate_dir, 'bae_temp_anomalies.rds'))) {

      temp_anom_velox <- velox(temp_mean_anomalies)

      temp_anomalies_list <- lapply(state_id,
                                    FUN = get_climate,
                                    grid_names = names(temp_mean_anomalies),
                                    velox_grid = temp_anom_velox,
                                    polygons = bae_ll,
                                    var_name = 'temp_anomalies')

      temp_anomalies_df <- do.call(rbind, temp_anomalies_list)

      write_rds(temp_anomalies_df, file.path(climate_dir, 'bae_temp_anomalies.rds'))
      system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

    } else {
      temp_anomalies_df <- read_rds(file.path(climate_dir, 'bae_temp_anomalies.rds'))
    }
  }

  # Extract mean ppt by fire bae
  if (!exists('bae_ppt_mean.rds')) {
    if (!file.exists(file.path(climate_dir, 'bae_ppt_mean.rds'))) {

      ppt_velox <- velox(ppt_mean)

      ppt_mean_list <- lapply(state_id,
                               FUN = get_climate,
                               grid_names = names(ppt_mean),
                               velox_grid = ppt_velox,
                               polygons = bae_ll,
                               var_name = 'ppt_mean')

      ppt_mean_df <- do.call(rbind, ppt_mean_list)

      write_rds(ppt_mean_df, file.path(climate_dir, 'bae_ppt_mean.rds'))
      system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

    } else {
      ppt_mean_df <- read_rds(file.path(climate_dir, 'bae_ppt_mean.rds'))
    }
  }

  # Extract anomalies ppt by fire bae
  if (!exists('bae_ppt_anomalies.rds')) {
    if (!file.exists(file.path(climate_dir, 'bae_ppt_anomalies.rds'))) {

      ppt_anom_velox <- velox(ppt_anomalies)

      ppt_anomalies_list <- lapply(state_id,
                                    FUN = get_climate,
                                    grid_names = names(ppt_anomalies),
                                    velox_grid = ppt_anom_velox,
                                    polygons = bae_ll,
                                    var_name = 'ppt_anomalies')

      ppt_anomalies_df <- do.call(rbind, ppt_anomalies_list)

      write_rds(ppt_anomalies_df, file.path(climate_dir, 'bae_ppt_anomalies.rds'))
      system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

    } else {
      ppt_anomalies_df <- read_rds(file.path(climate_dir, 'bae_ppt_anomalies.rds'))
    }
  }

  fpa_climate <- as_tibble(as.data.frame(fpa_wui_df)) %>%
    left_join(., pdsi_mean_df %>% mutate(pdsi_mean = mean) %>% dplyr::select(fpa_id, pdsi_mean), by = 'fpa_id') %>%
    left_join(., pdsi_anomalies_df %>% mutate(pdsi_mean_anomalies = mean) %>% dplyr::select(fpa_id, pdsi_mean_anomalies), by = 'fpa_id') %>%
    left_join(., temp_mean_df %>% mutate(temp_mean = mean) %>% dplyr::select(fpa_id, temp_mean), by = 'fpa_id') %>%
    left_join(., temp_anomalies_df %>% mutate(temp_mean_anomalies = mean) %>% dplyr::select(fpa_id, temp_mean_anomalies), by = 'fpa_id') %>%
    left_join(., ppt_mean_df %>% mutate(ppt_mean = mean) %>% dplyr::select(fpa_id, ppt_mean), by = 'fpa_id') %>%
    left_join(., ppt_anomalies_df %>% mutate(ppt_mean_anomalies = mean) %>% dplyr::select(fpa_id, ppt_mean_anomalies), by = 'fpa_id') %>%
    dplyr::select(-geom)

  write_rds(fpa_climate, file.path(climate_dir, 'fpa_climate.rds'))
  system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))

  } else {
    fpa_climate <- read_rds(file.path(climate_dir, 'fpa_climate.rds'))
  }
}
