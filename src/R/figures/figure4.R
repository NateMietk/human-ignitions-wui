# Distance calculations/plotting ****Fire Frequency-----------Regions-------------------------------


fishdis_reg <- as.data.frame(distance_rds) %>%
  filter(Class != 'Other' & Class != 'High Urban') %>%
  mutate(
    regions = ifelse(regions == 'East', 'North East', as.character(regions)),
    distance_to_urban = distance_to_urban * 0.001,
    decadal = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                     ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2005, 2005,
                             ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2015, 2015,
                                    DISCOVERY_YEAR )))) %>%
  group_by(fishid10k, decadal, region, IGNITION) %>%
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

colourCount =length(unique(fishdis_reg$inter))


firefreq_p <- fishdis_reg %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#f6e8c3', '#d8b365', '#8c510a', '#c7eae5',  '#5ab4ac', '#01665e')) +
  xlab("Distance from urban center (km)") + ylab("Ignition frequency") +
  expand_limits(x = 0, y = 0) +
  theme_pub()  +
  facet_wrap( ~ region, nrow = 1, scales = 'free_y') +
  theme(legend.position = 'right')


pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff_95 = abs(`#c7eae5` - `#f6e8c3`),
         line_diff_05 = abs(`#5ab4ac` - `#d8b365`),
         line_diff_15 = abs(`#01665e` - `#8c510a`))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff_95 = min(line_diff_95),
            line_diff_05 = min(line_diff_05),
            line_diff_15 = min(line_diff_15))

xpoints_cnt_95 <- left_join(min_diffs, pred_diffs, by = 'line_diff_95') %>%
  mutate(region = sort(unique(fishdis_reg$region)),
         xpt_cnt = x) %>%
  dplyr::select(region, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("region")) %>%
  group_by(region) %>%
  summarise(n = n(),
            xpt_cnt_95 = round(first(xpt_cnt),0),
            xpt_lab_95 = as.factor(xpt_cnt_95)) %>%
  ungroup()

xpoints_cnt_05 <- left_join(min_diffs, pred_diffs, by = 'line_diff_05') %>%
  mutate(region = sort(unique(fishdis_reg$region)),
         xpt_cnt = x) %>%
  dplyr::select(region, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("region")) %>%
  group_by(region) %>%
  summarise(n = n(),
            xpt_cnt_05 = round(first(xpt_cnt),0),
            xpt_lab_05 = as.factor(xpt_cnt_05)) %>%
  ungroup()

xpoints_cnt_15 <- left_join(min_diffs, pred_diffs, by = 'line_diff_15') %>%
  mutate(region = sort(unique(fishdis_reg$region)),
         xpt_cnt = x) %>%
  dplyr::select(region, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("region")) %>%
  group_by(region) %>%
  summarise(n = n(),
            xpt_cnt_15 = round(first(xpt_cnt),0),
            xpt_lab_15 = as.factor(xpt_cnt_15)) %>%
  ungroup()

xpoints_cnt <- left_join(xpoints_cnt_95, xpoints_cnt_05, by = 'region') %>%
  left_join(., xpoints_cnt_15, by = 'region')

regmean <- fishdis_reg %>%
  group_by(region, IGNITION) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(IGNITION, fcnt_mean)

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(region ==  "Central") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#f6e8c3', '#d8b365', '#8c510a', '#c7eae5',  '#5ab4ac', '#01665e')) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt, region == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_05), data = subset(xpoints_cnt, region == "Central"),
             linetype = "dashed", color  = "dark gray") +
  geom_vline(aes(xintercept = xpt_cnt_15), data = subset(xpoints_cnt, region == "Central"),
             linetype = "dashed", color  = "black") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, region == "Central"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, region == "Central"),
             linetype = "dashed", color = "#1F77B4") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(region ==  "West") %>%
  ggplot(aes(
    x = (median_distance),
    y = (f_cnt),
    group = inter,
    color = inter
  )) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values =  c('#f6e8c3', '#d8b365', '#8c510a', '#c7eae5',  '#5ab4ac', '#01665e')) +
  xlab("") + ylab("") +
  ggtitle("West") +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt_95), data = subset(xpoints_cnt, region == "West"),
             linetype = "dashed", color  = "gray") +
  geom_vline(aes(xintercept = xpt_cnt_05), data = subset(xpoints_cnt, region == "West"),
             linetype = "dashed", color  = "dark gray") +
  geom_vline(aes(xintercept = xpt_cnt_15), data = subset(xpoints_cnt, region == "West"),
             linetype = "dashed", color  = "black") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, region == "West"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, region == "West"),
             linetype = "dashed", color = "#1F77B4") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")


# Distance calculations/plotting ****Fire Frequency-----------Regions-------------------------------

fishdis_reg <- as.data.frame(distance_rds) %>%
  filter(Class != 'Other' & Class != 'High Urban') %>%
  mutate(
    pop_den = ifelse(
      DISCOVERY_YEAR >= 1992 |
        DISCOVERY_YEAR < 2000,
      POPDEN1990,
      ifelse(
        DISCOVERY_YEAR >= 2000 |
          DISCOVERY_YEAR < 2009,
        POPDEN2000,
        ifelse(
          DISCOVERY_YEAR >= 2010 |
            DISCOVERY_YEAR < 2016,
          POPDEN2010,
          NA
        )
      )
    ),
    house_den = ifelse(
      DISCOVERY_YEAR >= 1992 |
        DISCOVERY_YEAR < 2000,
      HUDEN1990,
      ifelse(
        DISCOVERY_YEAR >= 2000 |
          DISCOVERY_YEAR < 2009,
        HUDEN2000,
        ifelse(
          DISCOVERY_YEAR >= 2010 |
            DISCOVERY_YEAR < 2016,
          HUDEN2010,
          NA
        )
      )
    ),
    decade = ifelse(
      DISCOVERY_YEAR < 2000,
      1990,
      ifelse(DISCOVERY_YEAR >= 2000 &
               DISCOVERY_YEAR < 2009, 2000, 2010)
    ),
    regions = ifelse(regions == 'East', 'North East', as.character(regions)),
    distance_to_urban = distance_to_urban * 0.001
  ) %>%
  group_by(fishid10k, decade, regions, IGNITION) %>%
  summarise(
    median_popdensity = median(pop_den),
    median_homedensity = median(house_den),
    medain_distance = median(distance_to_urban),
    fseason_lngth = IQR(DISCOVERY_DOY),
    median_doy = median(DISCOVERY_DOY),
    f_cnt = n()
  ) %>%
  ungroup() %>%
  mutate(ignition_year = paste0(IGNITION, '_', decade))

firefreq_p <- fishdis_reg %>%
  transform(ignition_year = factor(
    ignition_year,
    levels = c(
      "Human_1990",
      "Human_2000",
      "Human_2010",
      "Lightning_1990",
      "Lightning_2000",
      "Lightning_2010"
    )
  )) %>%
  ggplot(aes(
    x = medain_distance,
    y = f_cnt,
    group = ignition_year,
    color = ignition_year
  )) +
  geom_smooth(
    method = 'loess',
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c(
    'red2',
    "red3",
    'red4',
    'royalblue2',
    'royalblue3',
    'royalblue4'
  )) +
  xlab("") + ylab("Ignition frequency") +
  theme_pub()  +
  facet_wrap(~ regions, nrow = 2) +
  theme(legend.position = 'none')

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_cnt <- left_join(min_diffs, pred_diffs) %>%
  mutate(Region = sort(unique(fishdis_reg$Region)),
         xpt_cnt = x) %>%
  dplyr::select(Region, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("Region")) %>%
  group_by(Region) %>%
  summarise(n = n(),
            xpt_cnt = round(first(xpt_cnt),0),
            xpt_lab = as.factor(xpt_cnt)) %>%
  ungroup()

regmean <- fishdis_reg %>%
  group_by(Region, IGNITION) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(IGNITION, fcnt_mean)

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(Region ==  "Central") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
               size = 0.75) +
  scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "#1F77B4") +
  # geom_text(data=subset(xpoints_cnt, Region == "Central"),
  #           aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_cnt, y = 10, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "Central"),
  #           aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "Central"),
  #           aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(Region ==  "West") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
               size = 0.75) +
  scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
  xlab("") + ylab("Ignition frequency") +
  ggtitle("West") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "West"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "#1F77B4") +
  # geom_text(data=subset(xpoints_cnt, Region == "West"),
  #           aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_cnt, y = 10, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "West"),
  #           aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = - 0.5 + Human, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "West"),
  #           aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(Region ==  "South East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
               size = 0.75) +
  scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
  xlab("Distance from WUI (km)") + ylab("Ignition frequency") +
  ggtitle("South East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "South East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "#1F77B4") +
  # geom_text(data=subset(xpoints_cnt, Region == "South East"),
  #           aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_cnt, y = 20, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "South East"),
  #           aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "South East"),
  #           aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = - 0.5 + Lightning, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(Region ==  "North East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
               size = 0.75, se = FALSE) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              fullrange = T, size = 0.5, linetype = "dashed") +
  scale_color_manual(values = c("#D62728","#1F77B4", "black")) +
  xlab("Distance from WUI (km)") + ylab("") +
  ggtitle("North East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  +
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "North East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "#1F77B4") +
  # geom_text(data=subset(xpoints_cnt, Region == "North East"),
  #           aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_cnt, y = 10, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "North East"),
  #           aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
  # geom_text(data = subset(regmean, Region == "North East"),
  #           aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave("Distance_FireFreq_Reg.png", g, width = 6, height = 8, dpi=1200)
ggsave("Distance_FireFreq_Reg.EPS", g, width = 6, height = 7, dpi=1200, scale = 2, units = "cm") #saves g

ffreq_xpt <- xpoints_cnt %>%
  transform(Region = factor(Region, levels=c("West", "Central", "South East", "North East"))) %>%
  #ggplot(aes(x = reorder(Region, -xpt_cnt), y = xpt_cnt)) +
  ggplot(aes(x = Region, y = xpt_cnt)) +
  geom_bar(stat="identity", color = "black") +
  xlab("") + ylab("Distance from the WUI (km)") +
  theme_pub()+
  ggtitle("(A) Fire frequency") +
  geom_hline(aes(yintercept = 2.4), linetype = "dashed")


  # Distance calculations/plotting ****IQR Fire season length---Regions---------------------------------------

  fseason_p <- fishdis_reg %>%
    ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
    geom_smooth(method = "glm", method.args = list(family = "poisson"),
                se = FALSE, fullrange = TRUE, size = 0.75) +
    scale_color_manual(values=c("red", "black")) +
    xlab("Distance from WUI (km)") + ylab("Fire season length") +
    theme_pub()  +
    facet_wrap(~Region,
               nrow = 2, labeller = label_wrap_gen(10))

  pred_diffs <- ggplot_build(fseason_p)$data[[1]] %>%
    tbl_df %>%
    dplyr::select(colour, y, x, PANEL) %>%
    spread(colour, y) %>%
    mutate(line_diff = abs(black - red))

  min_diffs <- pred_diffs %>%
    group_by(PANEL) %>%
    summarize(line_diff = min(line_diff))

  xpoints_fseason <- left_join(min_diffs, pred_diffs) %>%
    mutate(Region = sort(unique(fishdis_reg$Region)),
           xpt_season = x) %>%
    dplyr::select(Region, xpt_season)  %>%
    left_join(., fishdis_reg, by = c("Region")) %>%
    group_by(Region) %>%
    summarise(n = n(),
              xpt_season = round(first(xpt_season),0),
              xpt_lab = as.factor(xpt_season)) %>%
    ungroup()


  regmean <- fishdis_reg %>%
    group_by(Region, IGNITION) %>%
    summarise(fseason_mean = mean(fseason_lngth)) %>%
    spread(IGNITION, fseason_mean)


  # check to see where the min. diffs fall in plot
  fseason_cent <- fishdis_reg %>%
    filter(Region ==  "Central") %>%
    ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
    geom_smooth(method = "glm", method.args = list(family = "poisson"),
                 size = 0.75) +
    scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
    xlab("") + ylab("") +
    ggtitle("Central") +
    scale_x_continuous(limits = c(0, 125)) +
    theme_pub()  +
    geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "Central"),
               linetype = "dashed", color  = "gray") +
    geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "Central"),
               linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "Central"),
               linetype = "dashed", color = "#1F77B4") +
    geom_text(data=subset(xpoints_fseason, Region == "Central"),
              aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_season, y = 10, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "Central"),
              aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "Central"),
              aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(size = 8, face = "bold"),
          legend.position = "none")

  fseason_west <- fishdis_reg %>%
    filter(Region ==  "West") %>%
    ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
    geom_smooth(method = "glm", method.args = list(family = "poisson"),
                 size = 0.75) +
    scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
    xlab("") + ylab("IQR Range") +
    ggtitle("West") +
    scale_x_continuous(limits = c(0, 125)) +
    theme_pub()  +
    geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "West"),
               linetype = "dashed", color  = "gray") +
    geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "West"),
               linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "West"),
               linetype = "dashed", color = "#1F77B4") +
    geom_text(data=subset(xpoints_fseason, Region == "West"),
              aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_season, y = 10, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "West"),
              aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "West"),
              aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(size = 8, face = "bold"),
          legend.position = "none")

  fseason_se <- fishdis_reg %>%
    filter(Region ==  "South East") %>%
    ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
    geom_smooth(method = "glm", method.args = list(family = "poisson"),
                 size = 0.75) +
    # geom_smooth(method = "glm", method.args = list(family = "poisson"),
    #             fullrange = T, size = 0.5, linetype = "dashed") +
    scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
    xlab("Distance from WUI (km)") + ylab("IQR Range") +
    ggtitle("South East") +
    scale_x_continuous(limits = c(0, 125)) +
    theme_pub()  +
    geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "South East"),
               linetype = "dashed", color  = "gray") +
    geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "South East"),
               linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "South East"),
               linetype = "dashed", color = "#1F77B4") +
    geom_text(data=subset(xpoints_fseason, Region == "South East"),
              aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_season, y = 10, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "South East"),
              aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "South East"),
              aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(size = 8, face = "bold"),
          legend.position = "none")

  fseason_ne <- fishdis_reg %>%
    filter(Region ==  "North East") %>%
    ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
    geom_smooth(method = "glm", method.args = list(family = "poisson"),
                 size = 0.75) +
    scale_color_manual(values=c("#D62728","#1F77B4", "black")) +
    xlab("Distance from WUI (km)") + ylab("") +
    ggtitle("North East") +
    scale_x_continuous(limits = c(0, 125)) +
    theme_pub()  +
    geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "North East"),
               linetype = "dashed", color  = "gray") +
    geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "North East"),
               linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "North East"),
               linetype = "dashed", color = "#1F77B4") +
    geom_text(data=subset(xpoints_fseason, Region == "North East"),
              aes(label=paste(xpt_lab, "km", sep = " "), x = 20 + xpt_season, y = 10, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "North East"),
              aes(label=paste(round(Human,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Human, colour="red"), size = 4) +
    geom_text(data = subset(regmean, Region == "North East"),
              aes(label=paste(round(Lightning,2), "fires/km2", sep = " "), x = 90, y = 0.5 + Lightning, colour="red"), size = 4) +
    theme(axis.title = element_text(face = "bold"),
          strip.text = element_text(size = 8, face = "bold"),
          legend.position = "none")

  grid.arrange(fseason_west, fseason_cent, fseason_se, fseason_ne,
               ncol =2, widths = c(0.5, 0.5))
  g <- arrangeGrob(fseason_west, fseason_cent, fseason_se, fseason_ne,
                   ncol =2, widths = c(0.5, 0.5))
  ggsave("Distance_FireSeason_Reg.png", g, width = 6, height = 8, dpi=1200)
  ggsave("Distance_FireSeason_Reg.EPS", g, width = 6, height = 7, dpi=1200, scale = 2, units = "cm") #saves g
