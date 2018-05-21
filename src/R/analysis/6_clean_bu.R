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

  bu_list <- list.files(file.path(bu_out,
                        pattern = glob2rx('*masked*tif'),
                        full.names = TRUE)
  bu_stack <- stack(bu_list)
  bu_velox <- velox(bu_stack)
}


# Housing units per ecoregion
if (!exists('sum_extractions_bu')) {
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
