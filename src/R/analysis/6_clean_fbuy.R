
# Import the FBUY data
if (!exists('fbuy_extractions')) {
  if( !file.exists('data/anthro/first_year_built/FBUY/FBUY.csv')) {
    filename <- file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif')
    out_name <- gsub('.tif', '.csv', filename)

    fbuy_extractions <- raster(file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif'))
    fbuy_extractions <- extract(fbuy_extractions, mtbs_fire, na.rm = TRUE, stat = 'sum', df = TRUE)
    write_csv(fbuy_extractions, file = out_name)

    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    fbuy_raster <- raster(file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif'))

    fbuy_extractions <- read_csv('data/anthro/first_year_built/FBUY/FBUY.csv')
  }
}

fbuy_raster[fbuy_raster == 1] <- NA

fbuy_extractions_df <- fbuy_extractions %>%
  bind_cols %>%
  as_tibble %>%
  mutate(index = ID) %>%
  dplyr::select(-starts_with("ID")) %>%
  rename(ID = index) %>%
  mutate(bu_bool = ifelse(FBUY == 0, 0, 1),
         fbuy = ifelse( FBUY == 0 | FBUY == 1, NA, FBUY)) %>%
  group_by(ID) %>%
  summarise(num_bu_pixels = sum(bu_bool),
            fbuy_mean = mean(fbuy, na.rm = TRUE),
            fbuy_min = min(fbuy, na.rm = TRUE),
            fbuy_max = max(fbuy, na.rm = TRUE),
            fbuy_std = sd(fbuy, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mtbs_id = data.frame(mtbs_fire)$mtbs_id,
         bu_area = num_bu_pixels*0.025,
         fbuy_mean = ifelse(is.na(fbuy_mean) | is.nan(fbuy_mean) | fbuy_mean == Inf | fbuy_mean == -Inf,
                            0, fbuy_mean),
         fbuy_min = ifelse(is.na(fbuy_min) | is.nan(fbuy_min) | fbuy_min == Inf | fbuy_min == -Inf,
                           0, fbuy_min),
         fbuy_max = ifelse(is.na(fbuy_max) | is.nan(fbuy_max) | fbuy_max == Inf | fbuy_max == -Inf,
                           0, fbuy_max),
         fbuy_std = ifelse(is.na(fbuy_std) | is.nan(fbuy_std) | fbuy_std == Inf | fbuy_std == -Inf,
                           0, fbuy_std)) %>%
  dplyr::select(-ID)
