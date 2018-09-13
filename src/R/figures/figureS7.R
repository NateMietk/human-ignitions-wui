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

# Distance versus fire frequency
# fc9272 -> Human 1994-2004
# D62728 -> Human 2005-2015
# a6bddb -> Lightning 1994-2004
# 1F77B4 -> Lightning 2005-2015
regmean <- fishdis_reg %>%
  group_by(regions, inter) %>%
  summarise(fcnt_mean = mean(f_cnt))
supp_table <- regmean %>%
  spread(inter, fcnt_mean)

firefreq_p <- fishdis_reg %>%
  transform(regions = factor(regions, levels=c('West', 'Central', 'South East', 'North East'))) %>%
  ggplot(aes(x = median_distance, y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#fc9272","#D62728", '#a6bddb','#1F77B4')) + 
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  theme_pub()  +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none") +
  geom_hline(data = regmean, aes(yintercept = fcnt_mean, group = inter, color = inter), linetype = 'dashed') +
  facet_wrap( ~ regions, nrow = 2) +
  scale_y_continuous(limits = c(0,NA))

write_csv(regmean, file.path(supplements_text_figs, 'figureS7_distance_fcnt_means.csv'))

ggsave(file.path(supplements_text_figs, "figureS7_distance_ffreq.tiff"), firefreq_p, 
       width = 7, height = 8, dpi = 600, scale = 3, units = "cm")

# Distance versus fire season length
regmean <- fishdis_reg %>%
  group_by(regions, inter) %>%
  summarise(fseason_mean = mean(fseason_lngth))
supp_table <- regmean %>%
  spread(inter, fseason_mean)

firefreq_p <- fishdis_reg %>%
  transform(regions = factor(regions, levels=c('West', 'Central', 'South East', 'North East'))) %>%
  ggplot(aes(x = median_distance, y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#fc9272","#D62728", '#a6bddb','#1F77B4')) + 
  xlab("Distance from urban center (km)") + ylab("Fire season length") +
  theme_pub()  +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none") +
  geom_hline(data = regmean, aes(yintercept = fseason_mean, group = inter, color = inter), linetype = 'dashed') +
  facet_wrap( ~ regions, nrow = 2)  +
  scale_y_continuous(limits = c(0,NA))

write_csv(regmean, file.path(supplements_text_figs, 'figureS8_distance_fseason_means.csv'))

ggsave(file.path(supplements_text_figs, "figureS8_distance_fseason.tiff"), firefreq_p, 
       width = 7, height = 8, dpi = 600, scale = 3, units = "cm")

# Median home density versus fire season length -------------------------------
firefreq_p <- fishdis_reg %>%
  transform(regions = factor(regions, levels=c('West', 'Central', 'South East', 'North East'))) %>%
  ggplot(aes(x = log(median_homedensity), y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  scale_color_manual(values = c("#fc9272","#D62728", '#a6bddb','#1F77B4')) + 
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  theme_pub()  +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none") +
  facet_wrap( ~ regions, nrow = 2) +
  scale_y_continuous(limits = c(0,NA))

ggsave(file.path(supplements_text_figs, "figureS9_homedensity_fseason.tiff"), firefreq_p, 
       width = 7, height = 8, dpi = 600, scale = 3, units = "cm")

system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
