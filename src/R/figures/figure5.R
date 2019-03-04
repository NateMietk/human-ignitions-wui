
figs_data_dir <- file.path(figs_dir, 'ai', 'fig_data_arcgis')
project_albers <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

# Topanga Fire
topanga <- st_read(file.path(figs_data_dir, 'california', 'topanga.shp')) %>%
  st_transform(project_albers)

if(!file.exists(file.path(figs_data_dir, 'california', 'ZASMT_UTMAIN_SPATIAL_06_NO0.gpkg'))) {
  ztrax_ca <- st_read(file.path(figs_data_dir, 'california', 'ZASMT_UTMAIN_SPATIAL_06.gpkg')) %>%
    filter(YearBuilt != 0) %>%
    st_transform(project_albers)
  st_write(ztrax_ca, file.path(figs_data_dir, 'california', 'ZASMT_UTMAIN_SPATIAL_06_NO0.gpkg'), delete_layer =TRUE)
} else {
  ztrax_ca <- st_read(file.path(figs_data_dir, 'california', 'ZASMT_UTMAIN_SPATIAL_06_NO0.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'california', 'ztrax_topanga.gpkg'))) {
  ztrax_topanga <- ztrax_ca %>%
    st_crop(., topanga)
  st_write(ztrax_topanga, file.path(figs_data_dir, 'california', 'ztrax_topanga.gpkg'), delete_layer =TRUE)
} else {
  ztrax_topanga <- st_read(file.path(figs_data_dir, 'california', 'ztrax_topanga.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'california', 'wui_topanga.gpkg'))) {
  wui_topanga <- st_read(file.path(figs_data_dir, 'california', 'ca_wui_cp12')) %>%
    st_transform(project_albers) %>%
    st_crop(., topanga)
  st_write(wui_topanga, file.path(figs_data_dir, 'california', 'wui_topanga.gpkg'), delete_layer =TRUE)
} else {
  wui_topanga <- st_read(file.path(figs_data_dir, 'california', 'wui_topanga.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'california', 'ztrax_vs_silvis_ca.rds'))) {
  ztrax_topanga_homes <- ztrax_topanga %>%
    st_intersection(., topanga) %>%
    as.data.frame() %>%
    caunt()
  
  wui_topanga_homes <- wui_topanga  %>%
    st_intersection(., topanga) %>%
    group_by() %>%
    summarise(HU2010 = sum(HU2010, na.rm = TRUE))
  
  rbind(ztrax_topanga_homes, wui_topanga_homes) %>%
    write_rds(., file.path(figs_data_dir, 'california', 'ztrax_vs_silvis_ca.rds'))
} else {
  read_rds(file.path(figs_data_dir, 'california', 'ztrax_vs_silvis_ca.rds'))
}

if(!file.exists(file.path(figs_data_dir, 'california', 'wui_ztrax_topanga.gpkg'))) {
  wui_ztrax_topanga <- wui_topanga %>%
    st_join(., ztrax_topanga) %>%
    group_by(BLK10, WUICLASS10) %>%
    summarise(ztrax = n(),
              HU2010 = first(HU2010))
  st_write(wui_ztrax_topanga, file.path(figs_data_dir, 'california', 'wui_ztrax_topanga.gpkg'), delete_layer =TRUE)
} else {
  wui_ztrax_topanga <- st_read(file.path(figs_data_dir, 'california', 'wui_ztrax_topanga.gpkg'))
}

# Black Forest Fire
black_forest <- st_read(file.path(figs_data_dir, 'colorado', 'black_forest.shp')) %>%
  st_transform(project_albers)

if(!file.exists(file.path(figs_data_dir, 'colorado', 'ZASMT_UTMAIN_SPATIAL_08_NO0.gpkg'))) {
  ztrax_co <- st_read(file.path(figs_data_dir, 'colorado', 'ZASMT_UTMAIN_SPATIAL_08.gpkg')) %>%
    filter(YearBuilt != 0) %>%
    st_transform(project_albers)
  st_write(ztrax_co, file.path(figs_data_dir, 'colorado', 'ZASMT_UTMAIN_SPATIAL_08_NO0.gpkg'), delete_layer =TRUE)
} else {
  ztrax_co <- st_read(file.path(figs_data_dir, 'colorado', 'ZASMT_UTMAIN_SPATIAL_08_NO0.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'colorado', 'ztrax_black_forest.gpkg'))) {
  ztrax_black_forest <- ztrax_co %>%
    st_crop(., black_forest)
  st_write(ztrax_black_forest, file.path(figs_data_dir, 'colorado', 'ztrax_black_forest.gpkg'), delete_layer =TRUE)
} else {
  ztrax_black_forest <- st_read(file.path(figs_data_dir, 'colorado', 'ztrax_black_forest.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'colorado', 'wui_black_forest.gpkg'))) {
  wui_black_forest <- st_read(file.path(figs_data_dir, 'colorado', 'co_wui_cp12')) %>%
    st_transform(project_albers) %>%
    st_crop(., black_forest)
  st_write(wui_black_forest, file.path(figs_data_dir, 'colorado', 'wui_black_forest.gpkg'), delete_layer =TRUE)
} else {
  wui_black_forest <- st_read(file.path(figs_data_dir, 'colorado', 'wui_black_forest.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'colorado', 'ztrax_vs_silvis_co.rds'))) {
  ztrax_black_forest_homes <- ztrax_black_forest %>%
    st_intersection(., black_forest) %>%
    as.data.frame() %>%
    count()
  
  wui_black_forest_homes <- wui_black_forest  %>%
    st_intersection(., black_forest) %>%
    group_by() %>%
    summarise(HU2010 = sum(HU2010, na.rm = TRUE))
  
  rbind(ztrax_black_forest_homes, wui_black_forest_homes) %>%
    write_rds(., file.path(figs_data_dir, 'colorado', 'ztrax_vs_silvis_ca.rds'))
} else {
  read_rds(file.path(figs_data_dir, 'colorado', 'ztrax_vs_silvis_co.rds'))
}

if(!file.exists(file.path(figs_data_dir, 'colorado', 'wui_ztrax_black_forest.gpkg'))) {
  wui_ztrax_black_forest <- wui_black_forest %>%
    st_join(., ztrax_black_forest) %>%
    group_by(BLK10, WUICLASS10) %>%
    summarise(ztrax = n(),
              HU2010 = first(HU2010))
  st_write(wui_ztrax_black_forest, file.path(figs_data_dir, 'colorado', 'wui_ztrax_black_forest.gpkg'), delete_layer =TRUE)
} else {
  wui_ztrax_black_forest <- st_read(file.path(figs_data_dir, 'colorado', 'wui_ztrax_black_forest.gpkg'))
}

# Tenessee Fire
tn_fire <- st_read(file.path(figs_data_dir, 'tn_fire.shp')) %>%
  st_transform(project_albers)

if(!file.exists(file.path(figs_data_dir, 'tennessee', 'ZASMT_UTMAIN_SPATIAL_47_NO0.gpkg'))) {
  ztrax_tn <- st_read(file.path(figs_data_dir, 'tennessee', 'ZASMT_UTMAIN_SPATIAL_47.gpkg')) %>%
    filter(YearBuilt != 0) %>%
    st_transform(project_albers)
  st_write(ztrax_tn, file.path(figs_data_dir, 'tennessee', 'ZASMT_UTMAIN_SPATIAL_47_NO0.gpkg'), delete_layer =TRUE)
} else {
  ztrax_tn <- st_read(file.path(figs_data_dir, 'tennessee', 'ZASMT_UTMAIN_SPATIAL_47_NO0.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'tennessee', 'ztrax_tn_fire.gpkg'))) {
  ztrax_tn_fire <- ztrax_tn %>%
    st_crop(., tn_fire)
  st_write(ztrax_tn_fire, file.path(figs_data_dir, 'tennessee', 'ztrax_tn_fire.gpkg'), delete_layer =TRUE)
} else {
  ztrax_tn_fire <- st_read(file.path(figs_data_dir, 'tennessee', 'ztrax_tn_fire.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'tennessee', 'wui_tn_fire.gpkg'))) {
  wui_tn_fire <- st_read(file.path(figs_data_dir, 'tennessee', 'tn_wui_cp12')) %>%
    st_transform(project_albers) %>%
    st_crop(., tn_fire)
  st_write(wui_tn_fire, file.path(figs_data_dir, 'tennessee', 'wui_tn_fire.gpkg'), delete_layer =TRUE)
} else {
  wui_tn_fire <- st_read(file.path(figs_data_dir, 'tennessee', 'wui_tn_fire.gpkg'))
}

if(!file.exists(file.path(figs_data_dir, 'tennessee', 'ztrax_vs_silvis_tn.rds'))) {
  ztrax_tn_fire_homes <- ztrax_tn_fire %>%
    st_intersection(., tn_fire) %>%
    as.data.frame() %>%
    count()
  
  wui_tn_fire_homes <- wui_tn_fire  %>%
    st_intersection(., tn_fire) %>%
    group_by() %>%
    summarise(HU2010 = sum(HU2010, na.rm = TRUE))
  
  rbind(ztrax_tn_fire_homes, wui_tn_fire_homes) %>%
    write_rds(., file.path(figs_data_dir, 'tennessee', 'ztrax_vs_silvis_ca.rds'))
} else {
  read_rds(file.path(figs_data_dir, 'tennessee', 'ztrax_vs_silvis_tn.rds'))
}

if(!file.exists(file.path(figs_data_dir, 'tennessee', 'wui_ztrax_tn_fire.gpkg'))) {
  wui_ztrax_tn_fire <- wui_tn_fire %>%
    st_join(., ztrax_tn_fire, join = st_contains) %>%
    group_by(BLK10, WUICLASS10) %>%
    summarise(ztrax = n(),
              HU2010 = first(HU2010))
  st_write(wui_ztrax_tn_fire, file.path(figs_data_dir, 'tennessee', 'wui_ztrax_tn_fire.gpkg'), delete_layer =TRUE)
} else {
  wui_ztrax_tn_fire <- st_read(file.path(figs_data_dir, 'tennessee', 'wui_ztrax_tn_fire.gpkg'))
}

# Plot figures
topanga_p <- ggplot() +
  geom_sf(data = wui_topanga, aes(fill = WUICLASS10), alpha = 0.4) +
  scale_fill_manual(values = c(Low_Dens_Intermix = '#dd86ff',
                               Med_Dens_Intermix = '#dd86ff',
                               High_Dens_Intermix = '#dd86ff',
                               Low_Dens_Interface = '#ac68c7',
                               Med_Dens_Interface = '#ac68c7',
                               High_Dens_Interface = '#ac68c7',
                               Very_Low_Dens_Veg = '#f4e6bc',
                               Uninhabited_Veg = '#cabf9c',
                               Uninhabited_NoVeg = '#cfcacb',
                               Low_Dens_NoVeg = '#cfcacb',
                               Med_Dens_NoVeg = '#cfcacb',
                               Very_Low_Dens_NoVeg = '#cfcacb',
                               High_Dens_NoVeg = '#cfcacb')) +
  geom_sf(data = topanga, colour = "red", fill = 'red', alpha = 0.3) +
  geom_sf(data = ztrax_topanga_inaround, lwd = 0, size = 0.5) +
  coord_sf(xlim = c(-2064986, -2046469), ylim = c(1479002, 1492645)) +
  theme_nothing(legend = FALSE) +
  theme(
    panel.grid.major=element_line(colour="transparent"),
    panel.ontop = TRUE, 
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    text = element_blank(), 
    legend.key = element_rect(fill = "white")) 
ggsave(file.path(figs_data_dir, 'outputs', 'topanga_plot.tiff'), topanga_p)
       
black_forest_p <- ggplot() +
  geom_sf(data = wui_black_forest, aes(fill = WUICLASS10), alpha = 0.4) +
  scale_fill_manual(values = c(Low_Dens_Intermix = '#dd86ff',
                               Med_Dens_Intermix = '#dd86ff',
                               High_Dens_Intermix = '#dd86ff',
                               Low_Dens_Interface = '#ac68c7',
                               Med_Dens_Interface = '#ac68c7',
                               High_Dens_Interface = '#ac68c7',
                               Very_Low_Dens_Veg = '#f4e6bc',
                               Uninhabited_Veg = '#cabf9c',
                               Uninhabited_NoVeg = '#cfcacb')) +
  geom_sf(data = black_forest, colour = "red", fill = 'red', alpha = 0.3) +
  geom_sf(data = ztrax_black_forest_inaround, lwd = 0, size = 0.5) +
  coord_sf(xlim = c(-751000, -737900), ylim = c(1809100, 1818200)) +
  theme_nothing(legend = FALSE) +
  theme(
    panel.grid.major=element_line(colour="transparent"),
    panel.ontop = TRUE, 
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    text = element_blank(), 
    legend.key = element_rect(fill = "white")) 
ggsave(file.path(figs_data_dir, 'outputs', 'black_forest_plot.tiff'), black_forest_p)

tn_fire_p <- ggplot() +
  geom_sf(data = wui_tn_fire, aes(fill = WUICLASS10), alpha = 0.4) +
  scale_fill_manual(values = c(Low_Dens_Intermix = '#dd86ff',
                               Med_Dens_Intermix = '#dd86ff',
                               High_Dens_Intermix = '#dd86ff',
                               Low_Dens_Interface = '#ac68c7',
                               Med_Dens_Interface = '#ac68c7',
                               High_Dens_Interface = '#ac68c7',
                               Very_Low_Dens_Veg = '#f4e6bc',
                               Uninhabited_Veg = '#cabf9c',
                               Uninhabited_NoVeg = '#cfcacb',
                               Low_Dens_NoVeg = '#cfcacb',
                               Med_Dens_NoVeg = '#cfcacb',
                               Very_Low_Dens_NoVeg = '#cfcacb',
                               High_Dens_NoVeg = '#cfcacb')) +
  geom_sf(data = tn_fire, colour = "red", fill = 'red', alpha = 0.3) +
  geom_sf(data = ztrax_tn_fire_inaround, lwd = 0, size = 0.5) +
  coord_sf(xlim = c(1048754, 1052723), ylim = c(1525450, 1531750)) +
  theme_nothing(legend = FALSE) +
  theme(
    panel.grid.major=element_line(colour="transparent"),
    panel.ontop = TRUE, 
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    text = element_blank(), 
    legend.key = element_rect(fill = "white")) 
ggsave(file.path(figs_data_dir, 'outputs', 'tn_fire_plot.tiff'), tn_fire_p)
