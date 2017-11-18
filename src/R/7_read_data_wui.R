wui <- st_read(dsn = file.path(anthro_out, "wui_bounds.gpkg")) %>%
  mutate(wui_area_km2 = as.numeric(st_area(geom))/1000000)

names(wui) %<>% tolower

# Overall totals
totals_wui <- wui %>% 
  as.data.frame(.) %>%
  group_by() %>%
  summarise(tot_wvw = sum(wui_area_km2),
            tot_conterminous = 7827696) 

# Overall totals by CLASS
wvw_by_class <- wui %>% 
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(wvw = sum(wui_area_km2)) %>%
  mutate(pct_wvw = (wvw/totals_wui$tot_wvw)*100,
         pct_conus = (wvw/totals_wui$tot_conterminous)*100)
