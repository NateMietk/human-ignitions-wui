# Distance calculations/plotting ****Fire Frequency-----------Regions --> Interaction between decadal and ignition -------------------------------
fishdis_reg <- as.data.frame(distance_rds) %>%
  filter(Class != 'Other' & Class != 'High Urban') %>%
  mutate(
    regions = ifelse(regions == 'East', 'North East', as.character(regions)),
    distance_to_urban = distance_to_urban * 0.001,
    decadal = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                     ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2005, 2005,
                             ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2015, 2015,
                                    DISCOVERY_YEAR )))) %>%
  group_by(fishid10k, decadal, regions, IGNITION) %>%
  summarise(
    median_popdensity = median(pop_den),
    median_homedensity = median(house_den),
    median_distance = median(distance_to_urban),
    fseason_lngth = IQR(DISCOVERY_DOY),
    median_doy = median(DISCOVERY_DOY),
    f_cnt = n()
  ) %>%
  ungroup() %>%
  mutate(inter = paste0(IGNITION, "_", decadal))

firefreq_p <- fishdis_reg %>%
  ggplot(aes(
    x = median_distance,
    y = f_cnt,
    group = inter,
    color = inter
  )) +
  # geom_point(alpha = 0.15) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1.5
  ) +
  # scale_color_manual(values =  c('#d8b365', '#01665e')) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none')


pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff_95 = abs(`#fcae91` - `#bdd7e7`),
         line_diff_05 = abs(`#fb6a4a` - `#6baed6`),
         line_diff_15 = abs(`#cb181d` - `#2171b5`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff_95 = min(line_diff_95),
            line_diff_05 = min(line_diff_05),
            line_diff_15 = min(line_diff_15))

xpoints_cnt_95 <- left_join(min_diffs, pred_diffs, by = 'line_diff_95') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_95 = round(first(xpt_cnt),0),
            xpt_lab_95 = as.factor(xpt_cnt_95)) %>%
  ungroup()

xpoints_cnt_05 <- left_join(min_diffs, pred_diffs, by = 'line_diff_05') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_05 = round(first(xpt_cnt),0),
            xpt_lab_05 = as.factor(xpt_cnt_05)) %>%
  ungroup()

xpoints_cnt_15 <- left_join(min_diffs, pred_diffs, by = 'line_diff_15') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_15 = round(first(xpt_cnt),0),
            xpt_lab_15 = as.factor(xpt_cnt_15)) %>%
  ungroup()

xpoints_cnt <- left_join(xpoints_cnt_95, xpoints_cnt_05, by = 'regions') %>%
  left_join(., xpoints_cnt_15, by = 'regions')
write_csv(xpoints_cnt, "results/figs/draft/distance_firefreq_ignitiondecade.csv")

regmean <- fishdis_reg %>%
  group_by(regions, IGNITION) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(IGNITION, fcnt_mean)

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    #fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_05), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "gray50") +
  geom_vline(aes(xintercept = xpt_cnt_15), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    #fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_05), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "gray50") +
  geom_vline(aes(xintercept = xpt_cnt_15), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    #fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("South East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt_95, regions == "South East"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    #fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("North East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt_95, regions == "North East"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave("results/figs/draft/Distance_FireFreq_IgnitionDecade_Reg.pdf", g, width = 6, height = 8, dpi=1200)

# Distance calculations/plotting ****Fire Frequency-----------Regions --> Ignition ------------------------

firefreq_p <- fishdis_reg %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = IGNITION,
    color = IGNITION
  )) +
  # geom_point(alpha = 0.15) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1.5
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  # scale_color_manual(values =  c('#f6e8c3', '#d8b365', '#8c510a', '#c7eae5',  '#5ab4ac', '#01665e')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free') +
  theme(legend.position = 'none')


pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(`#cb181d` - `#2171b5`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_cnt <- left_join(min_diffs, pred_diffs, by = 'line_diff') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt = round(first(xpt_cnt),0),
            xpt_lab = as.factor(xpt_cnt)) %>%
  ungroup()
write_csv(xpoints_cnt, "results/figs/draft/distance_firefreq_ignition.csv")

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("South East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "South East"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("North East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "North East"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave("results/figs/draft/Distance_FireFreq_Ignition_Reg.pdf", g, width = 6, height = 8, dpi=1200)

# Distance calculations/plotting ****IQR Fire season length---Regions --> Ignition---------------------------------------

firefreq_p <- fishdis_reg %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = IGNITION,
    color = IGNITION
  )) +
  # geom_point(alpha = 0.15) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1.5
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  # scale_color_manual(values =  c('#f6e8c3', '#d8b365', '#8c510a', '#c7eae5',  '#5ab4ac', '#01665e')) +
  xlab("Distance from urban center (km)") + ylab("Fire season length (IQR)") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free') +
  theme(legend.position = 'none')


pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(`#cb181d` - `#2171b5`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_cnt <- left_join(min_diffs, pred_diffs, by = 'line_diff') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt = round(first(xpt_cnt),0),
            xpt_lab = as.factor(xpt_cnt)) %>%
  ungroup()
write_csv(xpoints_cnt, "results/figs/draft/distance_fseason_ignition.csv")

# check to see where the min. diffs fall in plot
fslength_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("South East") +
  theme_pub()  +
  # geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "South East"),
  #            linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = IGNITION,
    color = IGNITION
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#cb181d", '#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("North East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, regions == "North East"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
g <- arrangeGrob(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
ggsave("results/figs/draft/distance_fseason_ignition_region.pdf", g, width = 6, height = 8, dpi=1200)

# Distance calculations/plotting ****IQR Fire season length---Regions --> Interaction between decadal and ignition---------------------------------------

firefreq_p <- fishdis_reg %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = inter,
    color = inter
  )) +
  # geom_point(alpha = 0.15) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1.5
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("Distance from urban center (km)") + ylab("Fire season length (IQR)") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free') +
  theme(legend.position = 'none')


pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff_95 = abs(`#fcae91` - `#bdd7e7`),
         line_diff_05 = abs(`#fb6a4a` - `#6baed6`),
         line_diff_15 = abs(`#cb181d` - `#2171b5`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff_95 = min(line_diff_95),
            line_diff_05 = min(line_diff_05),
            line_diff_15 = min(line_diff_15))

xpoints_cnt_95 <- left_join(min_diffs, pred_diffs, by = 'line_diff_95') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_95 = round(first(xpt_cnt),0),
            xpt_lab_95 = as.factor(xpt_cnt_95)) %>%
  ungroup()

xpoints_cnt_05 <- left_join(min_diffs, pred_diffs, by = 'line_diff_05') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_05 = round(first(xpt_cnt),0),
            xpt_lab_05 = as.factor(xpt_cnt_05)) %>%
  ungroup()

xpoints_cnt_15 <- left_join(min_diffs, pred_diffs, by = 'line_diff_15') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_15 = round(first(xpt_cnt),0),
            xpt_lab_15 = as.factor(xpt_cnt_15)) %>%
  ungroup()

xpoints_cnt <- left_join(xpoints_cnt_95, xpoints_cnt_05, by = 'regions') %>%
  left_join(., xpoints_cnt_15, by = 'regions')
write_csv(xpoints_cnt, "results/figs/draft/distance_fseason_ignitiondecade.csv")

regmean <- fishdis_reg %>%
  group_by(regions, IGNITION) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(IGNITION, fcnt_mean)

# check to see where the min. diffs fall in plot
fslength_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_05), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "gray50") +
  geom_vline(aes(xintercept = xpt_cnt_15), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("South East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt_95, regions == "South East"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(
    x = (median_distance),
    y = (fseason_lngth),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a','#cb181d', '#bdd7e7','#6baed6','#2171b5')) +
  xlab("") + ylab("") +
  ggtitle("North East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt_95, regions == "North East"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
g <- arrangeGrob(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
ggsave("results/figs/draft/distance_fseason_ignitiondecade_region.pdf", g, width = 6, height = 8, dpi=1200)



