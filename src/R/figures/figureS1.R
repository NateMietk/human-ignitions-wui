p1 <- conus_bu %>%
  na.omit() %>%
  filter(class_coarse %in% c("VLD", "Wildlands")) %>%
  filter(n_den >= 1) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("0 - 250", "250 - 1000", "1000 - 10000", "> 10000"))) %>%
  mutate(buckets = bucket(n_den, 15)) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', fill = "gray97", size = .50)+
  geom_point(aes(x = long, y = lat, colour = factor(buckets), size = ptsz_n), stroke = 0.001) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(7,"RdYlBu"))) +
  scale_size_discrete(range = c(.75, 1.35), name="Homes threatened") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class_coarse, nrow = 1)

p2 <- conus_ff %>%
  filter(class_coarse %in% c("VLD", "Wildlands")) %>%
  filter(n_den >= 1) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(class_coarse = factor(class_coarse, levels=c("WUI", "Wildlands"))) %>%
  mutate(buckets = bucket(n_den, 15)) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', fill = "gray97", size = .50)+
  geom_point(aes(x = long, y = lat, colour = factor(buckets), size = ptsz_n), stroke = 0.001) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(7,"RdYlBu"))) +
  scale_size_discrete(range = c(.75, 1.35), name="Freq") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class_coarse, nrow = 1)

p3 <- conus_burn_area %>%
  na.omit() %>%
  filter(class_coarse %in% c("VLD", "Wildlands")) %>%
  transform(pct_class_human = factor(pct_class_human, levels=c("< 1", "1 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(frsz_cl = factor(frsz_cl, levels=c("0-4", "4-100", "100-400", "400-1000", ">1000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "gray97", size = .50) +
  geom_point(aes(x = long, y = lat, colour = factor(pct_class_human), size = frsz_cl), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(7,"RdYlBu"))) +
  scale_size_discrete(range = c(.5, 1.35), name="Burned area") +
  theme_nothing(legend = TRUE) +
  facet_wrap(~class_coarse, nrow = 1) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class_coarse, nrow = 1)

p4 <- conus_burn_area %>%
  na.omit() %>%
  filter(class_coarse %in% c("VLD", "Wildlands")) %>%
  transform(pct_class_lightning = factor(pct_class_lightning, levels=c("< 1", "1 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(frsz_cl = factor(frsz_cl, levels=c("0-4", "4-100", "100-400", "400-1000", ">1000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "gray97", size = .50) +
  geom_point(aes(x = long, y = lat, colour = factor(pct_class_lightning), size = frsz_cl), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(7,"RdYlBu"))) +
  scale_size_discrete(range = c(.5, 1.35), name="Burned area") +
  theme_nothing(legend = TRUE) +
  facet_wrap(~class_coarse, nrow = 1) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class_coarse, nrow = 1)

p1l <- p1 + theme(legend.position="none")
p2l <- p2 + theme(legend.position="none")
p3l <- p3 + theme(legend.position="none")
p4l <- p4 + theme(legend.position="none")

grid.arrange(p1l, p2l, p3, p4, nrow = 4)
g <- arrangeGrob(p1l, p2l, p3l, p4l, nrow = 4) #generates g

ggsave(file = file.path(supplements_text_figs, "figureS1.tiff"), g, width = 7, height = 9, dpi = 600, scale = 3, units = "cm") #saves g

system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
