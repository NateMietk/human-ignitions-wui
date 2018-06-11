# Distance calculations/plotting ****Fire Frequency-----------Regions --> Interaction between decadal and ignition -------------------------------
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none')


pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff_1 = abs(`#fcae91` - `#bdd7e7`),
         line_diff_2 = abs(`#fb6a4a` - `#6baed6`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarise(line_diff_1 = min(line_diff_1),
            line_diff_2 = min(line_diff_2))

xpoints_cnt_1 <- left_join(min_diffs, pred_diffs, by = 'line_diff_1') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_1 = round(first(xpt_cnt),0),
            xpt_lab_1 = as.factor(xpt_cnt_1)) %>%
  ungroup()

xpoints_cnt_2 <- left_join(min_diffs, pred_diffs, by = 'line_diff_2') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_2 = round(first(xpt_cnt),0),
            xpt_lab_2 = as.factor(xpt_cnt_2)) %>%
  ungroup()

xpoints_cnt <- left_join(xpoints_cnt_2, xpoints_cnt_1, by = c('regions', 'n')) %>%
  write_csv(., "results/figs/draft/distance_firefreq_ignitiondecade.csv")

regmean <- fishdis_reg %>%
  group_by(regions, ignition) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(ignition, fcnt_mean)

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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_1), data = subset(xpoints_cnt, regions == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt, regions == "Central"),
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = xpt_cnt_1), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "gray") +
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("South East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt_2, regions == "South East"),
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("North East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt_2, regions == "North East"),
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
    group = ignition,
    color = ignition
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
  summarise(line_diff = min(line_diff))

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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
    group = ignition,
    color = ignition
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
  # scale_color_manual(values =  c('#d8b365', '#01665e')) +
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none')

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff_1 = abs(`#fcae91` - `#bdd7e7`),
         line_diff_2 = abs(`#fb6a4a` - `#6baed6`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarise(line_diff_1 = min(line_diff_1),
            line_diff_2 = min(line_diff_2))

xpoints_cnt_1 <- left_join(min_diffs, pred_diffs, by = 'line_diff_1') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_1 = round(first(xpt_cnt),0),
            xpt_lab_1 = as.factor(xpt_cnt_1)) %>%
  ungroup()

xpoints_cnt_2 <- left_join(min_diffs, pred_diffs, by = 'line_diff_2') %>%
  mutate(regions = sort(unique(fishdis_reg$regions)),
         xpt_cnt = x) %>%
  dplyr::select(regions, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("regions")) %>%
  group_by(regions) %>%
  summarise(n = n(),
            xpt_cnt_2 = round(first(xpt_cnt),0),
            xpt_lab_2 = as.factor(xpt_cnt_2)) %>%
  ungroup()

xpoints_cnt <- left_join(xpoints_cnt_2, xpoints_cnt_1, by = c('regions', 'n')) %>%
  write_csv(., "results/figs/draft/distance_fseason_ignitiondecade.csv")

regmean <- fishdis_reg %>%
  group_by(regions, ignition) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(ignition, fcnt_mean)

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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt, regions == "Central"),
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_1), data = subset(xpoints_cnt, regions == "West"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt, regions == "West"),
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("South East") +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt_2, regions == "North East"),
             linetype = "dashed", color  = "black") +
  theme_pub()  +
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
  scale_color_manual(values =  c('#fcae91','#fb6a4a', '#bdd7e7','#6baed6')) +
  xlab("") + ylab("") +
  ggtitle("North East") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_2), data = subset(xpoints_cnt_2, regions == "North East"),
             linetype = "dashed", color  = "black") +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
g <- arrangeGrob(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
ggsave("results/figs/draft/distance_fseason_ignitiondecade_region.pdf", g, width = 6, height = 8, dpi=1200)



