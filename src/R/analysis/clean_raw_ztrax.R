ztrax_gdb_to_shp <- function(i, usa_shp, zpoints) {
  require(sf)
  require(tidyverse)
  
  outname <- basename(i) %>%
    gsub('.gdb', '.gpkg', .)
  
  if(!file.exists(file.path(zpoints, outname))) {
    
    layers <- sf::st_layers(i)
    layers <- tibble::data_frame(name = layers$name)
    
    state_ztrax <- apply(unique(layers), 1, 
                         function(j) {
                           sf::st_read(i, layer = j) %>%
                             sf::st_transform(sf::st_crs(usa_shp)) %>%
                             dplyr::filter(geom_wkt != 'POINT(0 0)') %>%
                             dplyr::filter(., grepl('AG101|AG102|AG103|AG104|AG107|AG108|PP199|RI|RR', LU_stdcode))
                         })
    
    final_ztrax <- do.call(rbind, state_ztrax) 
    
    sf::st_write(final_ztrax, file.path(zpoints, outname))
    system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")
    }
  }


gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

pblapply(gdbs, 
          ztrax_gdb_to_shp,
          usa_shp = usa_shp,
          zpoints = zpoints,
          cl = cl)
stopCluster(cl)




