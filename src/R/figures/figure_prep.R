# Fortify data

states <- as(usa_shp, "Spatial")
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))

ecoregs <- as(ecoreg_swse, "Spatial")
ecoregs$id <- row.names(ecoregs)
ec_df <- fortify(ecoregs, region = 'id')
ec_df <- left_join(ec_df, ecoregs@data, by = 'id')
names(ec_df) <- tolower(names(ec_df))

fishnet_25k_sp <- as(st_centroid(fishnet_25k), "Spatial")
fs25_df <- SpatialPointsDataFrame(fishnet_25k, fishnet_25k@data)
fs25_df$id <- row.names(fs25_df)
fs25_df <- data.frame(fs25_df)

fishnet_50k_sp <- as(st_centroid(fishnet_50k), "Spatial")
fs50_df <- SpatialPointsDataFrame(fishnet_50k_sp, fishnet_50k_sp@data)
fs50_df$id <- row.names(fs50_df)
fs50_df <- data.frame(fs50_df)

p1 <- ggplot() +
  geom_polygon(data = ec_df,
               aes(x = long,y = lat,group=group),
               color='black', fill = "gray99", size = .1)+
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))
ggsave(file = "figs/draft/main_text/regions.eps", p1, width = 1.75, height = 1, dpi=1200) #saves g
