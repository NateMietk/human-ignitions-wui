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
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(region ~ class_coarse, ncol = 2)

ggsave(file.path(main_text_figs, "figure3.tiff"), doy_east_west, width = 5, height = 5, dpi = 600, scale = 3, units = "cm") #saves g

shrt_doy <- as.data.frame(fpa_wui) %>%
  filter((class %in% c("VLD"))) %>%
  filter(!(region %in% c("Central"))) %>%
  group_by(discovery_doy, ignition, class, region) %>%
  summarise(count = n())

doy_east_west <- shrt_doy %>%
  transform(class = factor(class, levels=c("VLD"))) %>%
  ggplot() +
  geom_bar(aes(x =  discovery_doy, y = count,
               color = ignition, fill = ignition), stat = "identity") +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  theme_pub()  +
  xlab("Discovery day of year") + ylab("Fire frequency") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(region ~ class, ncol = 1)

ggsave(file.path(supplements_text_figs, "figureS3.tiff"), doy_east_west, width = 3.5, height = 5, dpi = 600, scale = 3, units = "cm") #saves g
