
for(i in unique(wui$stusps)) {
  if(!file.exists(file.path(wui_out, 'wui_only', paste0('wui_only_', i, '.gpkg')))) {
    print(paste('Starting ', i))
    
    wui_filtered <- wui %>%
      dplyr::select(Class10, stusps) %>%
      filter(stusps == i) %>%
      filter(Class10 %in% c("Interface WUI", "Intermix WUI")) %>%
      mutate(class_coarse =  as.factor(ifelse(Class10 == 'Intermix WUI' | Class10 == 'Interface WUI',
                                              'WUI', as.character(Class10)))) %>%
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))  %>%
      st_set_precision(1000) %>%
      lwgeom::st_make_valid() %>%
      group_by(class_coarse) %>%
      summarise() 
    
    print(paste('Writing ', i))
    st_write(wui_filtered, file.path(wui_out, 'wui_only', paste0('wui_only_', i, '.gpkg')))
    # system(paste0("aws s3 sync ", anthro_out, " ", s3_anthro_prefix))
    
  } else {
    print(paste('Skipping ', i))
  }
}
  
wui_files <- list.files(file.path(wui_out, 'wui_only'), pattern = 'gpkg', full.names = TRUE)

wui_only_list <- lapply(wui_files, 
                        function(x) {
                          dfs <- st_read(x)
                          return(dfs)
                          })

wui_only <- do.call('rbind', wui_only_list) %>%
  st_buffer(0)
wui_only_poly <- wui_only %>%
  st_cast('POLYGON') %>%
  mutate(row_ids = row_number(),
         area_km2 = as.numeric(st_area(.))/1000000)

st_write(wui_only_poly, file.path(wui_out, 'wui_only', paste0('wui_only_conus.gpkg')))
