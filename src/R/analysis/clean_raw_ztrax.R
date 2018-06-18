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
  }
}

gdbs <- list.files(ztrax_prefix, pattern = ".gdb", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

pblapply(gdbs,
         FUN = ztrax_gdb_to_shp,
         usa_shp = usa_shp,
         zpoints = zpoints,
         cl = cl)

stopCluster(cl)

#bind all of these together in one dataframe
gpkgs <- list.files(zpoints, pattern = ".gpkg", full.names = TRUE)

cl <- makeCluster(detectCores()/16)
registerDoParallel(cl)

ztrax_residential <- foreach(x = unique(gpkgs), .combine = 'rbind') %dopar% {
  require(sf)
  require(tidyverse)
  
  filename <- strsplit(x, "\\.|/|_") %>%
    lapply(`[`, 12) %>%
    unlist
  
  if(!file.exists(file.path(ztrax_res_out, paste0(filename, '_ztrax_residential_points.rds')))) {
    
    imported <- sf::st_read(x) %>%
      st_join(., wui, join = st_intersects) %>%
      setNames(tolower(names(.))) %>%
      as.data.frame() %>%
      group_by(blk10, yearbuilt) %>%
      count() 
    
    write_rds(imported,  
              file.path(ztrax_res_out,  paste0(filename, '_ztrax_residential_points.rds')))
  }
}
stopCluster(cl)

# Aggregate and clean all ztrax residential dataframes
rdss <- list.files(ztrax_res_out, pattern = ".rds", full.names = TRUE)

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()))

cleaned_wui_decades <- pblapply(rdss,
         FUN = function(x, cleaned_ztrax_out) {
           require(tidyverse)
           
           filename <- strsplit(x, "\\.|/|_") %>%
             lapply(`[`, 9) %>%
             unlist
           
           imported <- read_rds(x) 
           
           blk_grps <- imported %>%
             group_by(blk10) %>%
             summarise() %>%
             ungroup() %>%
             droplevels() %>%
             slice(rep(1:n(), each = 4)) %>%
             mutate(decade = as.factor(rep(c(1990, 2000, 2010, 2015), times = length(unique(blk10)))),
                    decade_built = as.factor(decade))
           
           cleaned_wui_decades <- imported %>%
             filter(yearbuilt != 0) %>%
             group_by(blk10, yearbuilt) %>%
             summarise(n = sum(n)) %>%
             mutate() %>%
             mutate(built_up = cumsum(n),
                    decade = case_when(
                      yearbuilt > 1800 & yearbuilt <= 1990 ~ 1990,
                      yearbuilt > 1990 & yearbuilt <= 2000 ~ 2000,
                      yearbuilt > 2000 & yearbuilt <= 2010 ~ 2010,
                      TRUE ~ 2015),
                    decade = as.factor(decade)) %>%
             group_by(blk10, decade) %>%
             summarise(built_up = max(built_up)) %>%
             ungroup() %>%
             full_join(., blk_grps, by = c('decade', 'blk10')) %>%
             droplevels() %>%
             mutate(decade = as.factor(decade),
                    blk10 = as.factor(blk10),
                    decade_built = as.integer(decade_built)) %>%
             arrange(desc(blk10, decade_built)) %>%
             fill(everything())
           
           cleaned_wui_decades %>%
             write_rds(., file.path(cleaned_ztrax_out, paste0(filename, '_cleaned_wui_residential.rds')))
         },
         cleaned_ztrax_out = cleaned_ztrax_out,
         cl = cl)

stopCluster(cl)

#bind all of these together in one dataframe
cleaned_wui_decades <- do.call(rbind, cleaned_wui_decades) %>%
  na.omit()  %>%
  mutate(year = decade,
         residential_built_up = built_up) %>%
  dplyr::select(blk10, year, residential_built_up)

cleaned_wui_decades %>%
  write_rds(., file.path(cleaned_ztrax_out, 'all_cleaned_wui_residential.rds'))

system("aws s3 sync data/anthro s3://earthlab-natem/human-ignitions-wui/anthro")


cleaned_wui_decades %>%
  group_by(decade) %>%
  summarise(built_up = sum(built_up))
