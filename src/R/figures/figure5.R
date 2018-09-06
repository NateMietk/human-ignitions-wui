# Distance versus fire frequency
fishdis_reg <- as.data.frame(distance_rds) %>%
  filter(class != 'Other' & class != 'High Urban') %>%
  mutate(distance_to_urban = distance_to_urban * 0.001) %>%
  group_by(fishid10k, ten_year, regions, ignition) %>%
  summarise(
    median_popdensity = median(pop_den),
    median_homedensity = median(house_den),
    median_distance = median(distance_to_urban),
    fseason_lngth = IQR(discovery_doy),
    median_doy = median(discovery_doy),
    f_cnt = n()
  ) %>%
  ungroup() %>%
  mutate(inter = paste0(ignition, "_", ten_year)) %>%
  filter(!is.na(ten_year))

# Median home density versus fire Frequency -------------------------------
firefreq_p <- fishdis_reg %>%
  transform(regions = factor(regions, levels=c('West', 'Central', 'South East', 'North East'))) %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  scale_color_manual(values = c("#fc9272","#D62728", '#a6bddb','#1F77B4')) + 
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none") +
  facet_wrap( ~ regions, nrow = 2) +
  scale_y_continuous(limits = c(0,NA))

ggsave(file.path(main_text_figs, "figure5.tiff"), firefreq_p, 
       width = 7, height = 8, dpi = 600, scale = 3, units = "cm")
system(paste0("aws s3 sync figs s3://earthlab-natem/human-ignitions-wui/figs"))

