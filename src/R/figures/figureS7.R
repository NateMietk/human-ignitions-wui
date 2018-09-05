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

# Distance versus fire season length WITH MEAN LINES -------------------------------
firefreq_p <- fishdis_reg %>%
  ggplot(aes(x = median_distance, y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), fullrange = TRUE, size = 1.5) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none')

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff_1 = abs(`#D62728` - `#1F77B4`),
         line_diff_2 = abs(`#fc9272` - `#a6bddb`))

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

xpoints_cnt <- left_join(xpoints_cnt_2, xpoints_cnt_1, by = c('regions', 'n'))

regmean <- fishdis_reg %>%
  group_by(regions, ten_year, ignition) %>%
  summarise(fcnt_mean = mean(fseason_lngth)) %>%
  spread(ignition, fcnt_mean)
write_csv(regmean, file.path(supplements_text_figs, 'figureS7_distance_fseason_means.csv'))

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter )) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("Central") +
  theme_pub()  +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "Central" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#D62728", size = 1) +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "Central" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#fc9272", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "Central" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#1F77B4", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "Central" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#a6bddb", size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,75), oob = rescale_none) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter )) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("West") +
  theme_pub()  +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "West" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#D62728", size = 1) +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "West" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#fc9272", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "West" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#1F77B4", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "West" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#a6bddb", size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,75), oob = rescale_none) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter )) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("South East") +
  theme_pub()  +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "South East" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#D62728", size = 1) +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "South East" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#fc9272", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "South East" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#1F77B4", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "South East" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#a6bddb", size = 1) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter )) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("North East") +
  theme_pub()  +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "North East" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#D62728", size = 1) +
  geom_hline(aes(yintercept = Human), data = subset(regmean, regions == "North East" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#fc9272", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "North East" & ten_year == '1994-2004'),
             linetype = "dashed", color  = "#1F77B4", size = 1) +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, regions == "North East" & ten_year == '2005-2015'),
             linetype = "dashed", color  = "#a6bddb", size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,75), oob = rescale_none) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave(file.path(supplements_text_figs, "figureS7_distance_fseason_wmean.tiff"), g, width = 6, height = 8, dpi=1200)

# Distance versus fire season length WITHOUT MEAN LINES ---------------------------------------
# check to see where the min. diffs fall in plot
fslength_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), fullrange = TRUE, size = 1.5) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("Central") +
  theme_pub()  +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), fullrange = TRUE, size = 1.5) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("West") +
  theme_pub()  +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), fullrange = TRUE, size = 1.5) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("South East") +
  theme_pub()  +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fslength_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(x = (median_distance), y = (fseason_lngth), group = inter, color = inter)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), fullrange = TRUE, size = 1.5) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("North East") +
  theme_pub()  +
  expand_limits(x = 0, y = 0) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
g <- arrangeGrob(fslength_west, fslength_cent, fslength_se, fslength_ne, ncol =2)
ggsave(file.path(supplements_text_figs, "figureS7_distance_fseason.tiff"), g, width = 6, height = 8, dpi=1200)

# Median home density versus fire Frequency -------------------------------
firefreq_p <- fishdis_reg %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none')

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("South East") +
  theme_pub()  +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(x = log(median_homedensity), y = f_cnt, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("North East") +
  theme_pub()  + 
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave(file.path(supplements_text_figs, "figureS7_homedensity_firecount.tiff"), g, width = 6, height = 8, dpi=1200)

# Median home density versus fire season length -------------------------------
firefreq_p <- fishdis_reg %>%
  ggplot(aes(x = log(median_homedensity), y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ regions, nrow = 1, scales = 'free_x') +
  theme(legend.position = 'none')

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(regions ==  "Central") %>%
  ggplot(aes(x = log(median_homedensity), y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(regions ==  "West") %>%
  ggplot(aes(x = log(median_homedensity), y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(regions ==  "South East") %>%
  ggplot(aes(x = log(median_homedensity), y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("South East") +
  theme_pub()  +
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(regions ==  "North East") %>%
  ggplot(aes(x = log(median_homedensity), y = fseason_lngth, group = inter, color = inter)) +
  geom_smooth(size = 1) +
  scale_color_manual(values = c("#D62728","#fc9272", '#1F77B4','#a6bddb')) +
  xlab("") + ylab("") +
  # ggtitle("North East") +
  theme_pub()  + 
  geom_vline(aes(xintercept = log(6.17)), linetype = "dashed", color  = "black") +
  geom_vline(aes(xintercept = log(741.3162)), linetype = "dashed", color  = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave(file.path(supplements_text_figs, "figureS7_homedensity_fseason.tiff"), g, width = 6, height = 8, dpi=1200)

system(paste0("aws s3 sync figs s3://earthlab-natem/human-ignitions-wui/figs"))
