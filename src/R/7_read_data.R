source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

# ICS 209 database from 2001-2013
wui_209 <- st_read(file.path(prefix, "fire/ics209/ics209_wui_conus.gpkg")) %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 


