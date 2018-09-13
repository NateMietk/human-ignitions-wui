shrt_doy <- as.data.frame(fpa_wui) %>%
  filter(class %in% c("Intermix WUI", 'Interface WUI', "VLD", "Wildlands")) %>%
  group_by(discovery_doy, ignition, class, region) %>%
  summarise(count = n())

doy_east_west <- shrt_doy %>%
  filter(class %in% c("Intermix WUI", 'Interface WUI', "VLD", "Wildlands")) %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', "VLD", 'Wildlands'))) %>%
  ggplot() +
  geom_bar(aes(x =  discovery_doy, y = count,
               color = ignition, fill = ignition), stat = "identity") +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  theme_pub()  +
  xlab("Discovery day of year") + ylab("Fire frequency") +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none") +
  facet_wrap(region ~ class, ncol = 4)

ggsave(file.path(supplements_text_figs, "figureS5.tiff"), doy_east_west, width = 8, height = 8, dpi = 600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
