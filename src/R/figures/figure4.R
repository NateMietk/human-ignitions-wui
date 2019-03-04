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
  xlab("log(Median Home Density)") + ylab("Ignition frequency") +
  theme_pub()  +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none") +
  facet_wrap( ~ regions, nrow = 2) +
  scale_y_continuous(limits = c(0,NA))

ggsave(file.path(main_text_figs, "figure4.tiff"), firefreq_p, 
       width = 7, height = 8, dpi = 600, scale = 3, units = "cm")
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))

# Line intesections
# fc9272 -> Human 1994-2004
# D62728 -> Human 2005-2015
# a6bddb -> Lightning 1994-2004
# 1F77B4 -> Lightning 2005-2015
firefreq_p <- fishdis_reg %>%
  filter(ten_year != '1994-2004') %>%
  ggplot(aes(x = median_distance, y = f_cnt, group = inter, color = inter)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values = c("red","black")) + 
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 2) 

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarise(line_diff = min(line_diff))

xpoints_cnt_1 <- left_join(min_diffs, pred_diffs) %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(x_0515 = round(first(xpt_cnt),0)) %>%
  ungroup()

firefreq_p <- fishdis_reg %>%
  filter(ten_year == '1994-2004') %>%
  ggplot(aes(x = median_distance, y = f_cnt, group = inter, color = inter)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values = c("red","black")) + 
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 2) 

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarise(line_diff = min(line_diff))

xpoints_cnt_2 <- left_join(min_diffs, pred_diffs) %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(x_9404 = round(first(xpt_cnt),0)) %>%
  ungroup()

xpoints_cnt <- left_join(xpoints_cnt_2, xpoints_cnt_1)


# What is the number of homes at the peaks?
firefreq_p <- fishdis_reg %>%
  transform(regions = factor(regions, levels=c('West', 'Central', 'South East', 'North East'))) %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1)
filter(ten_year != '1994-2004') %>%
  ggplot(aes(x = median_distance, y = f_cnt, group = inter, color = inter)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values = c("red","black")) + 
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 2) 

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(group, y, x, PANEL) %>%
  group_by(PANEL, group) %>%
  summarise(y = max(y)) %>%
  left_join(., ggplot_build(firefreq_p)$data[[1]], by = 'y')
