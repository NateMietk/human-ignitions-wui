# Import the BUI data
if (!file.exists(file.path(bui_out, 'bui_masked_1990.tif'))) {

  bui_list <- list.files(bui_out,
                         pattern = '.tif',
                         full.names = TRUE)
  bui <- do.call(brick, lapply(bui_list, raster))
  bui_masked <- mask(bui, fbuy_raster)

  # Modis date creation
  year_seq <- c(1990, 1995, 2000, 2005, 2010, 2015)

  bui_masked <- setZ(bui_masked, year_seq, 'years')
  names(bui_masked) <- paste0("bui_masked_", year_seq)

  writeRaster(bui_masked,
              filename = file.path(bui_out, names(bui_masked)),
              bylayer=TRUE, format="GTiff")
  system(paste0("aws s3 sync ", prefix, " ", s3_base))

} else {

  bui_list <- list.files(bui_out,
                         pattern = glob2rx('*masked*tif'),
                         full.names = TRUE)
  bui_stack <- stack(bui_list)
  bui_velox <- velox(bui_stack)
}

# Housing units per ecoregion
if (!exists('sum_extractions_bui')) {
  if (!file.exists(file.path(bui_out, 'summary_ecoregion_bui.rds'))) {

    ecoregions <- ecoreg_plain %>%
      group_by(us_l3name) %>%
      summarise() %>%
      st_cast('POLYGON')

    sum_ecoregions_bui <- bui_velox$extract(sp = ecoregions, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(us_l3name = as.data.frame(ecoregions)$us_l3name,
             sum_bui_1990 = X1,
             sum_bui_1995 = X2,
             sum_bui_2000 = X3,
             sum_bui_2005 = X4,
             sum_bui_2010 = X5,
             sum_bui_2015 = X6) %>%
      dplyr::select(-starts_with('X'))
    write_rds(sum_ecoregions_bui, file.path(bui_out, 'summary_ecoregion_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    sum_ecoregions_bui <- read_rds(file.path(bui_out, 'summary_ecoregion_bui.rds'))
    }
  }

# Housing units per FPA buffered point
if (!exists('sum_fpa_bui')) {
  if (!file.exists(file.path(bui_out, 'summary_fpa_bui.rds'))) {

    sum_fpa_bui <- bui_velox$extract(sp = fpa, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa)$FPA_ID,
             sum_bui_1990 = X1,
             sum_bui_1995 = X2,
             sum_bui_2000 = X3,
             sum_bui_2005 = X4,
             sum_bui_2010 = X5,
             sum_bui_2015 = X6) %>%
      dplyr::select(-starts_with('X'))
    write_rds(sum_fpa_bui, file.path(bui_out, 'summary_fpa_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    sum_fpa_bui <- read_rds(file.path(bui_out, 'summary_fpa_bui.rds'))
  }
}

# Housing units per fpa_1k buffered point
if (!exists('sum_fpa_1k_bui')) {
  if (!file.exists(file.path(bui_out, 'summary_fpa_1k_bui.rds'))) {

    sum_fpa_1k_bui <- bui_velox$extract(sp = fpa_1k, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa_1k)$FPA_ID,
             sum_bui_1990 = X1,
             sum_bui_1995 = X2,
             sum_bui_2000 = X3,
             sum_bui_2005 = X4,
             sum_bui_2010 = X5,
             sum_bui_2015 = X6) %>%
      dplyr::select(-starts_with('X'))

    write_rds(sum_fpa_1k_bui, file.path(bui_out, 'summary_fpa_1k_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }  else {
    sum_fpa_1k_bui <- read_rds(file.path(bui_out, 'summary_fpa_1k_bui.rds'))
  }
}

# Housing units per fpa_2k buffered point
if (!exists('sum_fpa_2k_bui')) {
  if (!file.exists(file.path(bui_out, 'summary_fpa_2k_bui.rds'))) {

    sum_fpa_2k_bui <- bui_velox$extract(sp = fpa_2k, fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa_2k)$FPA_ID,
             sum_bui_1990 = X1,
             sum_bui_1995 = X2,
             sum_bui_2000 = X3,
             sum_bui_2005 = X4,
             sum_bui_2010 = X5,
             sum_bui_2015 = X6) %>%
      dplyr::select(-starts_with('X'))

    write_rds(sum_fpa_2k_bui, file.path(bui_out, 'summary_fpa_2k_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  } else {
    sum_fpa_2k_bui <- read_rds(file.path(bui_out, 'summary_fpa_2k_bui.rds'))
  }
}

# Housing units per fpa_3k buffered point
if (!exists('sum_fpa_3k_bui')) {
  if (!file.exists(file.path(bui_out, 'summary_fpa_3k_bui.rds'))) {

    sum_fpa_3k_bui <- bui_velox$extract(sp = st_cast(fpa_3k, 'MULTIPOLYGON'), fun = function(x) sum(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
      as_tibble() %>%
      mutate(fpa_id = as.data.frame(fpa_3k)$FPA_ID,
             sum_bui_1990 = X1,
             sum_bui_1995 = X2,
             sum_bui_2000 = X3,
             sum_bui_2005 = X4,
             sum_bui_2010 = X5,
             sum_bui_2015 = X6) %>%
      dplyr::select(-starts_with('X'))

    write_rds(sum_fpa_3k_bui, file.path(bui_out, 'summary_fpa_3k_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }  else {
    sum_fpa_3k_bui <- read_rds(file.path(bui_out, 'summary_fpa_3k_bui.rds'))
  }
}
