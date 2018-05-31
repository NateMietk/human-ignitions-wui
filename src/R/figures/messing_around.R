
# WUI only ----------------------------------------------------------------

wui_ignition_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse == "WUI") %>%
  group_by(ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_wui_ignition_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse == "WUI") %>%
  group_by(discovery_year, ignition) %>%
  summarise(f_cnt_fpa = n(),
            bu_total = sum(bu),
            burned_area = sum(fire_size_km2)) %>%
  ungroup() %>%
  group_by(ignition) %>%
  summarise(lt_mean_cnt = mean(f_cnt_fpa),
            lt_mean_bu = mean(bu_total),
            lt_mean_burned_area = mean(burned_area)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_cleaned %>%
  filter(class_coarse == "WUI") %>%
  group_by(ignition, discovery_year) %>%
  summarise(f_cnt_fpa = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., wui_ignition_fpa_bu_yearly, by = c('ignition')) %>%
  left_join(., lt_wui_ignition_fpa_bu_yearly, by = c('ignition')) %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt_fpa,
         home_per_areaburned = bu/burned_area,
         anomalies_cnt = f_cnt_fpa/lt_mean_cnt,
         anomalies_bu = bu - lt_mean_bu,
         anomalies_burned_area = burned_area - lt_mean_burned_area,
         fcnt_5 = rollmean(x = f_cnt_fpa, 5, align = "right", fill = NA),
         bu_5 = rollmean(x = bu, 5, align = "right", fill = NA),
         burned_area_5 = rollmean(x = burned_area, 5, align = "right", fill = NA),
         anom_fcnt_5 = rollmean(x = anomalies_cnt, 5, align = "right", fill = NA),
         anom_burned_area_5 = rollmean(x = anomalies_burned_area, 5, align = "right", fill = NA),
         anom_bu_area_5 = rollmean(x = anomalies_bu, 5, align = "right", fill = NA)) %>%  
  ungroup() %>%
  filter(!is.na(discovery_year))

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_fcnt_5, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Anomalies of wildfire count") +
  # geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of wildfire burned area (in 1,000 km2)") +
  # geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_bu_area_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

grid.arrange(fpa_p, ba_p, bu_p, nrow = 1)
g <- arrangeGrob(fpa_p, ba_p, bu_p, nrow = 1)
ggsave(file = "results/figs/draft/wui_5yrmean_anom_cbb_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = fcnt_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Wildfire count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Wildfire burned area (in 1,000 km2)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = bu_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

grid.arrange(fpa_p, ba_p, bu_p, nrow = 1)
g <- arrangeGrob(fpa_p, ba_p, bu_p, nrow = 1)
ggsave(file = "results/figs/draft/wui_5yrmean_cbb_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

# WUI only -- REGIONS ----------------------------------------------------------------

wui_ignition_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse == "WUI") %>%
  group_by(regions, ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_wui_ignition_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse == "WUI") %>%
  group_by(discovery_year, regions, ignition) %>%
  summarise(f_cnt_fpa = n(),
            bu_total = sum(bu),
            burned_area = sum(fire_size_km2)) %>%
  ungroup() %>%
  group_by(regions, ignition) %>%
  summarise(lt_mean_cnt = mean(f_cnt_fpa),
            lt_mean_bu = mean(bu_total),
            lt_mean_burned_area = mean(burned_area)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_cleaned %>%
  filter(class_coarse == "WUI") %>%
  group_by(regions, ignition, discovery_year) %>%
  summarise(f_cnt_fpa = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., wui_ignition_fpa_bu_yearly, by = c('regions', 'ignition')) %>%
  left_join(., lt_wui_ignition_fpa_bu_yearly, by = c('regions', 'ignition')) %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt_fpa,
         home_per_areaburned = bu/burned_area,
         anomalies_cnt = f_cnt_fpa/lt_mean_cnt,
         anomalies_bu = bu - lt_mean_bu,
         anomalies_burned_area = burned_area - lt_mean_burned_area,
         fcnt_5 = rollmean(x = f_cnt_fpa, 5, align = "right", fill = NA),
         bu_5 = rollmean(x = bu, 5, align = "right", fill = NA),
         burned_area_5 = rollmean(x = burned_area, 5, align = "right", fill = NA),
         anom_fcnt_5 = rollmean(x = anomalies_cnt, 5, align = "right", fill = NA),
         anom_burned_area_5 = rollmean(x = anomalies_burned_area, 5, align = "right", fill = NA),
         anom_bu_area_5 = rollmean(x = anomalies_bu, 5, align = "right", fill = NA)) %>%  
  ungroup() %>%
  filter(!is.na(discovery_year))

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_fcnt_5, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Anomalies of wildfire count") +
  # geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')

ggsave(file = "results/figs/draft/wui_5yrmean_anom_cnt_region.pdf", fpa_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of wildfire burned area (in 1,000 km2)") +
  geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/wui_5yrmean_anom_ba_region.pdf", ba_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_bu_area_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))  + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/wui_5yrmean_anom_bu_region.pdf", bu_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = fcnt_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Wildfire count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/wui_5yrmean_cnt_region.pdf", fpa_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Wildfire burned area (in 1,000 km2)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/wui_5yrmean_ba_region.pdf", ba_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = bu_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/wui_5yrmean_bu_region.pdf", bu_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")
# CONUS -------------------------------------------------------------------

wui_ignition_fpa_bu_yearly <- bu_cleaned %>%
  group_by(ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_wui_ignition_fpa_bu_yearly <- bu_cleaned %>%
  group_by(discovery_year, ignition) %>%
  summarise(f_cnt_fpa = n(),
            bu_total = sum(bu),
            burned_area = sum(fire_size_km2)) %>%
  ungroup() %>%
  group_by(ignition) %>%
  summarise(lt_mean_cnt = mean(f_cnt_fpa),
            lt_mean_bu = mean(bu_total),
            lt_mean_burned_area = mean(burned_area)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_cleaned %>%
  group_by(ignition, discovery_year) %>%
  summarise(f_cnt_fpa = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., wui_ignition_fpa_bu_yearly, by = c('ignition')) %>%
  left_join(., lt_wui_ignition_fpa_bu_yearly, by = c('ignition')) %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt_fpa,
         home_per_areaburned = bu/burned_area,
         anomalies_cnt = f_cnt_fpa/lt_mean_cnt,
         anomalies_bu = bu - lt_mean_bu,
         anomalies_burned_area = burned_area - lt_mean_burned_area,
         fcnt_5 = rollmean(x = f_cnt_fpa, 5, align = "right", fill = NA),
         bu_5 = rollmean(x = bu, 5, align = "right", fill = NA),
         burned_area_5 = rollmean(x = burned_area, 5, align = "right", fill = NA),
         anom_fcnt_5 = rollmean(x = anomalies_cnt, 5, align = "right", fill = NA),
         anom_burned_area_5 = rollmean(x = anomalies_burned_area, 5, align = "right", fill = NA),
         anom_bu_area_5 = rollmean(x = anomalies_bu, 5, align = "right", fill = NA)) %>%  
  ungroup() %>%
  filter(!is.na(discovery_year))

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_fcnt_5, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Anomalies of wildfire count") +
  # geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of wildfire burned area (in 1,000 km2)") +
  # geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_bu_area_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

grid.arrange(fpa_p, ba_p, bu_p, nrow = 1)
g <- arrangeGrob(fpa_p, ba_p, bu_p, nrow = 1)
ggsave(file = "results/figs/draft/conus_5yrmean_anom_cbb_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = fcnt_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Wildfire count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Wildfire burned area (in 1,000 km2)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = bu_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

grid.arrange(fpa_p, ba_p, bu_p, nrow = 1)
g <- arrangeGrob(fpa_p, ba_p, bu_p, nrow = 1)
ggsave(file = "results/figs/draft/conus_5yrmean_cbb_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

# CONUS only -- REGIONS ----------------------------------------------------------------

ignition_fpa_bu_yearly <- bu_cleaned %>%
  group_by(regions, ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_ignition_fpa_bu_yearly <- bu_cleaned %>%
  group_by(discovery_year, regions, ignition) %>%
  summarise(f_cnt_fpa = n(),
            bu_total = sum(bu),
            burned_area = sum(fire_size_km2)) %>%
  ungroup() %>%
  group_by(regions, ignition) %>%
  summarise(lt_mean_cnt = mean(f_cnt_fpa),
            lt_mean_bu = mean(bu_total),
            lt_mean_burned_area = mean(burned_area)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_cleaned %>%
  group_by(regions, ignition, discovery_year) %>%
  summarise(f_cnt_fpa = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., ignition_fpa_bu_yearly, by = c('regions', 'ignition')) %>%
  left_join(., lt_ignition_fpa_bu_yearly, by = c('regions', 'ignition')) %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt_fpa,
         home_per_areaburned = bu/burned_area,
         anomalies_cnt = f_cnt_fpa/lt_mean_cnt,
         anomalies_bu = bu - lt_mean_bu,
         anomalies_burned_area = burned_area - lt_mean_burned_area,
         fcnt_5 = rollmean(x = f_cnt_fpa, 5, align = "right", fill = NA),
         bu_5 = rollmean(x = bu, 5, align = "right", fill = NA),
         burned_area_5 = rollmean(x = burned_area, 5, align = "right", fill = NA),
         anom_fcnt_5 = rollmean(x = anomalies_cnt, 5, align = "right", fill = NA),
         anom_burned_area_5 = rollmean(x = anomalies_burned_area, 5, align = "right", fill = NA),
         anom_bu_area_5 = rollmean(x = anomalies_bu, 5, align = "right", fill = NA)) %>%  
  ungroup() %>%
  filter(!is.na(discovery_year))

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_fcnt_5, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Anomalies of wildfire count") +
  # geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')

ggsave(file = "results/figs/draft/conus_5yrmean_anom_cnt_region.pdf", fpa_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of wildfire burned area (in 1,000 km2)") +
  geom_hline(aes(yintercept = 0), color = 'black', linetype= 'dashed') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/conus_5yrmean_anom_ba_region.pdf", ba_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = anom_bu_area_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Anomalies of threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))  + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/conus_5yrmean_anom_bu_region.pdf", bu_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

fpa_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = fcnt_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-3, 3)) +
  xlab("Year") + ylab("Wildfire count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/conus_5yrmean_cnt_region.pdf", fpa_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

ba_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = burned_area_5*0.001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Wildfire burned area (in 1,000 km2)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/conus_5yrmean_ba_region.pdf", ba_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")

bu_p <- class_fpa_bu_df %>%
  filter(discovery_year > 1995) %>%
  ggplot(aes(x =  discovery_year, y = bu_5*0.00001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    #method.args = list(family = "poisson"),
    size = 1) +
  theme_pub() +
  # scale_y_continuous(limits = c(-2.25, 2)) +
  xlab("Year") + ylab("Threatened homes (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b", '#2166ac')) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0")) + facet_wrap(~regions, scales = 'free_y')
ggsave(file = "results/figs/draft/conus_5yrmean_bu_region.pdf", bu_p,
       width = 6, height = 6, dpi=1200, scale = 4, units = "cm")