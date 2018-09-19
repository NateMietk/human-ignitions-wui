shrt_doy <- as.data.frame(fpa_wui) %>%
  filter((class_coarse %in% c("WUI", 'Wildlands'))) %>%
  filter(!(region %in% c("Central"))) %>%
  group_by(discovery_doy, ignition, class_coarse, region) %>%
  summarise(count = n())

doy_east_west <- shrt_doy %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'Wildlands'))) %>%
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
  facet_wrap(region ~ class_coarse, ncol = 2)

ggsave(file.path(main_text_figs, "figure3.tiff"), doy_east_west, width = 5, height = 5, dpi = 600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
