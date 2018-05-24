# Import the buidling units data
if (!file.exists(file.path(bu_out, 'bu_masked_1990.tif'))) {
  bu_list <- list.files(bu_out,
                        pattern = 'temp_slice',
                        full.names = TRUE)
  bu <- do.call(brick, lapply(bu_list, raster))
  bu_masked <- mask(bu, fbuy_raster)

  year_seq <- c(1990, 1995, 2000, 2005, 2010, 2015)

  bu_masked <- setZ(bu_masked, year_seq, 'years')
  names(bu_masked) <- paste0("bu_masked_", year_seq)

  writeRaster(bu_masked,
              filename = file.path(bu_out, names(bu_masked)),
              bylayer=TRUE, format="GTiff")
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  } else {

  bu_list <- list.files(bu_out,
                        pattern = glob2rx('*masked*tif'),
                        full.names = TRUE)
  bu_stack <- stack(bu_list)
  bu_velox <- velox(bu_stack)
}


# Housing units per ecoregion
if (!exists('sum_ecoregions_bu')) {
  if (!file.exists(file.path(bu_out, 'summary_ecoregion_bu.rds'))) {

    ecoregions <- ecoreg_plain %>%
      group_by(us_l3name) %>%
      summarise() %>%
      st_cast('POLYGON')

    sum_ecoregions_bu <- bu_velox$extract(sp = ecoregions, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(us_l3name = as.data.frame(ecoregions)$us_l3name,
             sum_bu_1990 = X1,
             sum_bu_1995 = X2,
             sum_bu_2000 = X3,
             sum_bu_2005 = X4,
             sum_bu_2010 = X5,
             sum_bu_2015 = X6) %>%
      dplyr::select(-starts_with('X'))
    write_rds(sum_ecoregions_bu, file.path(bu_out, 'summary_ecoregion_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    sum_ecoregions_bu <- read_rds(file.path(bu_out, 'summary_ecoregion_bu.rds'))
  }
}

# # Housing units per ecoregion
# if (!exists('sum_states_bu')) {
#   if (!file.exists(file.path(bu_out, 'summary_state_bu.rds'))) {
#
#     states <- usa_shp %>%
#       group_by(stusps) %>%
#       summarise() %>%
#       st_cast('POLYGON')
#
#     sum_states_bu <- bu_velox$extract(sp = states, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
#       as_tibble() %>%
#       mutate(stusps = as.data.frame(states)$stusps,
#              sum_bu_1990 = X1,
#              sum_bu_1995 = X2,
#              sum_bu_2000 = X3,
#              sum_bu_2005 = X4,
#              sum_bu_2010 = X5,
#              sum_bu_2015 = X6) %>%
#       dplyr::select(-starts_with('X'))
#     write_rds(sum_states_bu, file.path(bu_out, 'summary_state_bu.rds'))
#     system(paste0("aws s3 sync ", prefix, " ", s3_base))
#
#   } else {
#     sum_states_bu <- read_rds(file.path(bu_out, 'summary_state_bu.rds'))
#   }
# }


# Housing units per FPA fire perimeters
if (!exists('sum_fpa_bu')) {
  if (!file.exists(file.path(bu_out, 'summary_fpa_bu.rds'))) {

    fpa_dis <- fpa %>%
      slice(1:10) %>%
      mutate(year_bidecadal = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                           ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                                  ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                         ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                                ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                       DISCOVERY_YEAR ))))))
      group_by(IGNITION, year_bidecadal) %>%
      summarise.sf()


    sum_fpa_bu <- bu_velox$extract(sp = fpa, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa)$FPA_ID,
             sum_bu_1990 = X1,
             sum_bu_1995 = X2,
             sum_bu_2000 = X3,
             sum_bu_2005 = X4,
             sum_bu_2010 = X5,
             sum_bu_2015 = X6) %>%
      dplyr::select(-starts_with('X'))
    write_rds(sum_fpa_bu, file.path(bu_out, 'summary_fpa_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    sum_fpa_bu <- read_rds(file.path(bu_out, 'summary_fpa_bu.rds'))
  }
}


# Housing units per FPA fire perimeters
if (!exists('sum_fpa_bu')) {
  if (!file.exists(file.path(bu_out, 'summary_fpa_bu.rds'))) {

    sum_fpa_bu <- bu_velox$extract(sp = fpa, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa)$FPA_ID,
             sum_bu_1990 = X1,
             sum_bu_1995 = X2,
             sum_bu_2000 = X3,
             sum_bu_2005 = X4,
             sum_bu_2010 = X5,
             sum_bu_2015 = X6) %>%
      dplyr::select(-starts_with('X'))
    write_rds(sum_fpa_bu, file.path(bu_out, 'summary_fpa_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    sum_fpa_bu <- read_rds(file.path(bu_out, 'summary_fpa_bu.rds'))
    }
  }

# Housing units per fpa_1k buffered point
if (!exists('sum_fpa_1k_bu')) {
  if (!file.exists(file.path(bu_out, 'summary_fpa_1k_bu.rds'))) {

    sum_fpa_1k_bu <- bu_velox$extract(sp = fpa_1k, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa_1k)$FPA_ID,
             sum_bu_1990 = X1,
             sum_bu_1995 = X2,
             sum_bu_2000 = X3,
             sum_bu_2005 = X4,
             sum_bu_2010 = X5,
             sum_bu_2015 = X6) %>%
      dplyr::select(-starts_with('X'))

    write_rds(sum_fpa_1k_bu, file.path(bu_out, 'summary_fpa_1k_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  } else {
    sum_fpa_1k_bu <- read_rds(file.path(bu_out, 'summary_fpa_1k_bu.rds'))
    }
  }

# Housing units per fpa_2k buffered point
if (!exists('sum_fpa_2k_bu')) {
  if (!file.exists(file.path(bu_out, 'summary_fpa_2k_bu.rds'))) {

    sum_fpa_2k_bu <- bu_velox$extract(sp = fpa_2k, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa_2k)$FPA_ID,
             sum_bu_1990 = X1,
             sum_bu_1995 = X2,
             sum_bu_2000 = X3,
             sum_bu_2005 = X4,
             sum_bu_2010 = X5,
             sum_bu_2015 = X6) %>%
      dplyr::select(-starts_with('X'))

    write_rds(sum_fpa_2k_bu, file.path(bu_out, 'summary_fpa_2k_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  } else {
    sum_fpa_2k_bu <- read_rds(file.path(bu_out, 'summary_fpa_2k_bu.rds'))
    }
  }

# Housing units per fpa_3k buffered point
if (!exists('sum_fpa_3k_bu')) {
  if (!file.exists(file.path(bu_out, 'summary_fpa_3k_bu.rds'))) {

    sum_fpa_3k_bu <- bu_velox$extract(sp = st_cast(fpa_3k, 'MULTIPOLYGON'), fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa_3k)$FPA_ID,
             sum_bu_1990 = X1,
             sum_bu_1995 = X2,
             sum_bu_2000 = X3,
             sum_bu_2005 = X4,
             sum_bu_2010 = X5,
             sum_bu_2015 = X6) %>%
      dplyr::select(-starts_with('X'))

    write_rds(sum_fpa_3k_bu, file.path(bu_out, 'summary_fpa_3k_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  } else {
    sum_fpa_3k_bu <- read_rds(file.path(bu_out, 'summary_fpa_3k_bu.rds'))
    }
  }
