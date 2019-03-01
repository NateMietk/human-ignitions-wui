
# Remove the geommac polygons that are found in the mtbs database as well
if(!file.exists(file.path(accuracy_assessment_dir, 'geomac.gpkg'))) {
  geomac <- st_read(file.path(geomac_raw_dir, 'US_HIST_FIRE_PERIMTRS_DD83.gdb')) %>%
    st_transform(st_crs(usa_shp)) %>%
    st_make_valid() %>%
    filter(!(year_ %in% c('2016', '2017', '2018'))) %>%
    mutate(GEOMAC_ID = fire_num,
           GEOMAC_FIRE_NAME = capitalize(tolower(fire_name)),
           GEOMAC_FIRE_NAME = ifelse(GEOMAC_FIRE_NAME == 'Mm 192', '192',
                                     ifelse(GEOMAC_FIRE_NAME == 'Border no. 14', 'Border 14', GEOMAC_FIRE_NAME)),
           GEOMAC_DISCOVERY_YEAR = as.integer(as.character(year_)),
           GEOMAC_ACRES = acres) %>%
    filter(GEOMAC_ID != '2001-CA-MDF-621' & GEOMAC_ID != '2002-WY-CAD' & GEOMAC_ID != '2010-CA-BDF-2010' & GEOMAC_ID != '2010-CO_ARF-FYQ7') %>%
    dplyr::select(GEOMAC_ID, GEOMAC_FIRE_NAME, GEOMAC_DISCOVERY_YEAR, GEOMAC_ACRES) %>%
    group_by(GEOMAC_ID, GEOMAC_FIRE_NAME) %>%
    summarise(GEOMAC_DISCOVERY_YEAR = first(GEOMAC_DISCOVERY_YEAR),
              GEOMAC_ACRES = sum(GEOMAC_ACRES)) %>% ungroup()
  
  geommac_mtbs_repeats <- st_join(mtbs_fire, geomac) %>%  
    filter(GEOMAC_DISCOVERY_YEAR == MTBS_DISCOVERY_YEAR) %>%
    dplyr::select(GEOMAC_ID, GEOMAC_FIRE_NAME, GEOMAC_DISCOVERY_YEAR) 
  
  geomac <- geomac %>%
    anti_join(., as.data.frame(geommac_mtbs_repeats) %>% dplyr::select(-contains('geom|geometry|Shape')), 
                                                                       by = c('GEOMAC_ID', 'GEOMAC_FIRE_NAME', 'GEOMAC_DISCOVERY_YEAR'))
  
  geomac_2600m <- geomac %>%
    st_buffer(2600)
  
  st_write(geomac, file.path(accuracy_assessment_dir, 'geomac.gpkg'), delete_layer = TRUE)
  st_write(geomac_2600m, file.path(accuracy_assessment_dir, 'geomac_2600m.gpkg'), delete_layer = TRUE)
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  geomac <- st_read(file.path(accuracy_assessment_dir, 'geomac.gpkg'))
  geomac_2600m <- st_read(file.path(accuracy_assessment_dir, 'geomac_2600m.gpkg'))
  }

# Find the polygon in the geomac that contain an FPA id
if(!file.exists(file.path(accuracy_assessment_dir, 'geomac_fpa.gpkg'))) {
  
  geomac_fpa_final <- clean_geomac(geomac, shp_filename = 'geomac_fpa.gpkg', rds_filename = 'geomac_fpa_count_stats.rds')
  geomac_2600m_fpa_final <- clean_geomac(geomac_2600m, shp_filename = 'geomac_2600m_fpa.gpkg', rds_filename = 'geomac_2600m_fpa_count_stats.rds')
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
  } else {
    geomac_fpa_final <- st_read(file.path(accuracy_assessment_dir, 'geomac_fpa.gpkg'))
    geomac_2600m_fpa_final <- st_read(file.path(accuracy_assessment_dir, 'geomac_2600m_fpa.gpkg'))
  }

# Find the polygon in the mtbs that contain an FPA id
if(!file.exists(file.path(accuracy_assessment_dir, 'mtbs_fpa.gpkg'))) {

  mtbs_fpa_final <- clean_mtbs(shp_filename = 'mtbs_fpa.gpkg', rds_filename = 'mtbs_fpa_count_stats.rds')
  mtbs_2600m_fpa_final <- clean_mtbs(buffer = TRUE, cores = 1, shp_filename = 'mtbs_2600m_fpa.gpkg', rds_filename = 'mtbs_2600m_fpa_count_stats.rds')
  
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  mtbs_fpa_final <- st_read(file.path(accuracy_assessment_dir, 'mtbs_fpa.gpkg')) 
  mtbs_2600m_fpa_final <- st_read(file.path(accuracy_assessment_dir, 'mtbs_2600m_fpa.gpkg')) 
  }

# Adjoin the cleaned mtbs and geomac databases that have known FPA ids
if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac.gpkg"))) {
  fpa_mtbs_geomac <- do.call(rbind, list(geomac_fpa_final, mtbs_fpa_final)) %>%
    mutate(RADIUS = sqrt(as.numeric(st_area(.))/pi)) %>%
    st_make_valid()
  fpa_mtbs_geomac <- fpa_mtbs_geomac[!is.na(st_dimension(fpa_mtbs_geomac)),]
  st_write(fpa_mtbs_geomac, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  fpa_mtbs_geomac_2600m <- do.call(rbind, list(geomac_2600m_fpa_final, mtbs_2600m_fpa_final)) %>%
    st_make_valid()
  fpa_mtbs_geomac_2600m <- fpa_mtbs_geomac_2600m[!is.na(st_dimension(fpa_mtbs_geomac_2600m)),]
  st_write(fpa_mtbs_geomac_2600m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_2600m.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  for(i in c(250, 500, 1000)) {
    fpa_df <- fpa_mtbs_geomac %>% 
      st_parallel(., st_buffer, n_cores =  parallel::detectCores(), dist = i) %>%
      st_write(., file.path(accuracy_assessment_dir, paste0("fpa_mtbs_geomac_", i, "m.gpkg")),
             driver = "GPKG", delete_layer = TRUE)
    system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  }
  
} else {
  fpa_mtbs_geomac <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac.gpkg")) 
  fpa_mtbs_geomac_250m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_250m.gpkg")) 
  fpa_mtbs_geomac_500m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_500m.gpkg")) 
  fpa_mtbs_geomac_1000m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_1000m.gpkg")) 
  fpa_mtbs_geomac_2600m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_2600m.gpkg")) 
  }

# Create a burned area estimate of the geomac+mtbs with FPA id
if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae.gpkg"))) {
  
  fpa_mtbs_geomac_bae <- fpa_fire %>%
    dplyr::select(FPA_ID, geom) %>%
    left_join(as.data.frame(fpa_mtbs_geomac) %>%
                dplyr::select(-geom), ., by = 'FPA_ID') %>%
    st_as_sf() %>%
    sf::st_transform(proj_ed) %>%
    st_buffer(., dist = .$RADIUS) %>%
    st_transform(proj_ea) %>%
    lwgeom::st_make_valid(.)
  
  fpa_mtbs_geomac_bae <- fpa_mtbs_geomac_bae[!is.na(st_dimension(fpa_mtbs_geomac_bae)),]
  st_write(fpa_mtbs_geomac_bae, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  
  for(i in c(250, 500, 1000)) {
    fpa_df <- fpa_mtbs_geomac_bae %>% 
      st_parallel(., st_buffer, n_cores =  parallel::detectCores(), dist = i) %>%
      st_write(., file.path(accuracy_assessment_dir, paste0("fpa_mtbs_geomac_bae_", i, "m.gpkg")),
               driver = "GPKG", delete_layer = TRUE)
    system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  }
} else {
  fpa_mtbs_geomac_bae <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae.gpkg")) 
  fpa_mtbs_geomac_bae_250m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae_250m.gpkg")) 
  fpa_mtbs_geomac_bae_500m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae_500m.gpkg")) 
  fpa_mtbs_geomac_bae_1000m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_bae_1000m.gpkg")) 
}

# Find the difference in burned area from the estimated and 'true' perimeters
if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_difference.gpkg"))) {
  buffer_perimeter_difference <- st_erase(fpa_mtbs_geomac_bae, fpa_mtbs_geomac) %>%
    mutate(DIF_FIRE_SIZE_M2 = as.numeric(st_area(.)))
  
  st_write(buffer_perimeter_difference, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_difference.gpkg"),
           driver = "GPKG", delete_layer = TRUE)
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  buffer_perimeter_difference <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_difference.gpkg")) 
}

# What is the percentage of FPA points that fall within GeoMac and MTBS polygons?
# inital_count <- geomac_inital_count + mtbs_inital_count
# final_count <- geomac_final_count + mtbs_final_count
# pct_fpa <- final_count/inital_count
# 0.3835519 <- 11375/29657

# What is the percentage of buffer areas that falls outside of the MTBS+GOEMAC polygons?
as.data.frame(buffer_perimeter_difference) %>%
  group_by() %>%
  summarise(FIRE_SIZE_M2 = sum(FIRE_SIZE_M2),
            DIF_FIRE_SIZE_M2 = sum(DIF_FIRE_SIZE_M2)) %>%
  mutate(accuracy = DIF_FIRE_SIZE_M2/FIRE_SIZE_M2)

# Does the burned area percentage change based on ignition type?
as.data.frame(buffer_perimeter_difference) %>%
  group_by(IGNITION) %>%
  summarise(FIRE_SIZE_M2 = sum(FIRE_SIZE_M2),
            DIF_FIRE_SIZE_M2 = sum(DIF_FIRE_SIZE_M2)) %>%
  mutate(accuracy = DIF_FIRE_SIZE_M2/FIRE_SIZE_M2)

# How many homes are threatened in the buffered areas compared to 'true' polygons
idx_yearly <- year(seq(as.Date('1980-01-01'), as.Date('2016-01-01'), by = 'year'))

ztrax <- list.files(cumsum_ztrax_rst_dir, full.names = TRUE) %>%
  raster::stack(.) %>%
  setZ(., idx_yearly) %>%
  subset(., which(getZ(.) >= '1992' &
                    getZ(.) <= '2015'))

if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax.gpkg"))) {

  fpa_mtbs_geomac_ztrax <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac)
  fpa_mtbs_geomac_ztrax_250m <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_250m)
  fpa_mtbs_geomac_ztrax_500m <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_500m)
  fpa_mtbs_geomac_ztrax_1000m <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_1000m)
  
  write_rds(fpa_mtbs_geomac_ztrax, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax.rds"))
  write_rds(fpa_mtbs_geomac_ztrax_250m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_250m.rds"))
  write_rds(fpa_mtbs_geomac_ztrax_500m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_500m.rds"))
  write_rds(fpa_mtbs_geomac_ztrax_1000m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_1000m.rds"))
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))

} else {
  fpa_mtbs_geomac_ztrax <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax.gpkg"))
  fpa_mtbs_geomac_ztrax_250m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_250m.rds"))
  fpa_mtbs_geomac_ztrax_500m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_500m.rds"))
  fpa_mtbs_geomac_ztrax_1000m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_1000m.rds"))
  }

if (!file.exists(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae.gpkg"))) {

  fpa_mtbs_geomac_ztrax_bae <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_bae)
  fpa_mtbs_geomac_ztrax_bae_250m <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_bae_250m)
  fpa_mtbs_geomac_ztrax_bae_500m <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_bae_500m)
  fpa_mtbs_geomac_ztrax_bae_1000m <- extract_ztrax(rst_in = ztrax, shp_in = fpa_mtbs_geomac_bae_1000m)
  
  write_rds(fpa_mtbs_geomac_ztrax_bae, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae.gpkg"))
  write_rds(fpa_mtbs_geomac_ztrax_bae_250m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae_250m.rds"))
  write_rds(fpa_mtbs_geomac_ztrax_bae_500m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae_500m.rds"))
  write_rds(fpa_mtbs_geomac_ztrax_bae_1000m, file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae_1000m.rds"))
  system(paste0("aws s3 sync ", fire_crt, " ", s3_fire_prefix))
  
} else {
  fpa_mtbs_geomac_ztrax_bae <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae.gpkg"))
  fpa_mtbs_geomac_ztrax_bae_250m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae_250m.rds"))
  fpa_mtbs_geomac_ztrax_bae_500m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae_500m.rds"))
  fpa_mtbs_geomac_ztrax_bae_1000m <- st_read(file.path(accuracy_assessment_dir, "fpa_mtbs_geomac_ztrax_bae_1000m.rds"))
  }

