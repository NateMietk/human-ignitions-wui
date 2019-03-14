bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))

df <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  mutate(fire_size_ha = fire_size_km2*100,
         fire_size = case_when(
           fire_size_ha >  0 & fire_size_ha < 100 ~ '0-100',
           fire_size_ha >= 100 & fire_size_ha < 200 ~ '100-200',
           fire_size_ha >= 200 & fire_size_ha < 400 ~ '200-400',
           fire_size_ha >= 400 & fire_size_ha < 1000 ~ '400-1k',
           fire_size_ha >= 1000 & fire_size_ha < 10000 ~ '1k-10k',
           fire_size_ha >= 10000 & fire_size_ha < 50000 ~ '10k-50k',
           fire_size_ha >= 50000 ~ '> 50k', TRUE ~ NA_character_ )) 

aov.models <- df %>%
  dplyr::select(class_coarse, fire_size, build_up_count_no_zero_0, ignition) %>%
  filter(build_up_count_no_zero_0 != 0) %>%
  unite(ignition_size, c('ignition', 'fire_size')) %>%
  mutate(ignition_size = as.factor(ignition_size),
         build_up_count_no_zero_0 = log(build_up_count_no_zero_0)) %>%
  split(.$class_coarse) %>%
  map(~ aov(build_up_count_no_zero_0 ~ ignition_size, data = .x)) %>%
  map( agricolae::HSD.test, trt = 'ignition_size', group=TRUE) %>%
  lapply(., '[', c(5)) %>%
  lapply(., function(x) {
    rownames_to_column(as.data.frame(x), var = "ignition_size") %>%
      dplyr::select(ignition_size, 
                    build_up_count_no_zero_0 = groups.build_up_count_no_zero_0,
                    groups = groups.groups)}) %>%
  dplyr::bind_rows(., .id = 'class_coarse') %>%
  separate(ignition_size, c('ignition', 'fire_size'), sep = '_')

p1 <- df %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  ggplot(aes(x = fire_size, y = log(build_up_count_no_zero_0), fill = ignition), group = ignition) +
  geom_boxplot(position = position_dodge(width = 0.85)) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('') + ylab('log Average home threatened per fire event') +
  geom_vline(aes(xintercept = 7.5), color = 'black') +
  theme_pub() +
  facet_wrap(~class_coarse) +
  theme(legend.position = 'none')

tt <- ggplot_build(p1)$data[[1]] %>%
  dplyr::select(fill, group, PANEL, ymax_final) %>%
  as_tibble() %>%
  mutate(fire_size = case_when(
    group == 1 | group == 2 ~ '0-100',
    group == 3 | group == 4 ~ '100-200',
    group == 5 | group == 6 ~ '200-400', 
    group == 7 | group == 8 ~ '400-1k',
    group == 9 | group == 10 ~ '1k-10k',
    group == 11 | group == 12 ~ '10k-50k',
    group == 13 | group == 14 ~ '> 50k'),
    class_coarse = case_when(
      PANEL == 1 ~ 'WUI',
      PANEL == 2 ~ 'VLD', 
      PANEL == 3 ~ 'Wildlands'),
    ignition = case_when(
      fill == "#1F77B4" ~ 'Lightning',
      fill == "#D62728" ~ 'Human')) %>%
  dplyr::select(class_coarse, fire_size, ignition, ymax_final) %>%
  left_join(aov.models, ., by = c('fire_size', 'class_coarse', 'ignition')) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands')))

# COSTS 
fire_sizes <- as_tibble(as.data.frame(wui_209)) %>%
  filter(cause != 'Unk') %>%
  group_by(incident_unique_id) %>%
  summarise(fire_size_km2 = sum(area_km2))

df2 <- as_tibble(as.data.frame(wui_209)) %>%
  filter(cause != 'Unk') %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
  left_join(., fire_sizes, by = 'incident_unique_id') %>%
  mutate(fire_size_ha = fire_size_km2*100,
         fire_size = case_when(
           fire_size_ha >  0 & fire_size_ha < 100 ~ '0-100',
           fire_size_ha >= 100 & fire_size_ha < 200 ~ '100-200',
           fire_size_ha >= 200 & fire_size_ha < 400 ~ '200-400',
           fire_size_ha >= 400 & fire_size_ha < 1000 ~ '400-1k',
           fire_size_ha >= 1000 & fire_size_ha < 10000 ~ '1k-10k',
           fire_size_ha >= 10000 & fire_size_ha < 50000 ~ '10k-50k',
           fire_size_ha >= 50000 ~ '> 50k', TRUE ~ NA_character_ )) %>%
  na.omit()

aov.models2 <- df2 %>%
  dplyr::select(class_coarse, fire_size, costs, cause) %>%
  filter(costs != 0) %>%
  unite(ignition_size, c('cause', 'fire_size')) %>%
  mutate(ignition_size = as.factor(ignition_size),
         costs = log(costs)) %>%
  split(.$class_coarse) %>%
  map(~ aov(costs ~ ignition_size, data = .x)) %>%
  map( agricolae::HSD.test, trt = 'ignition_size', group=TRUE) %>%
  lapply(., '[', c(5)) %>%
  lapply(., function(x) {
    rownames_to_column(as.data.frame(x), var = "ignition_size") %>%
      dplyr::select(ignition_size, 
                    costs = groups.costs,
                    groups = groups.groups)}) %>%
  dplyr::bind_rows(., .id = 'class_coarse') %>%
  separate(ignition_size, c('cause', 'fire_size'), sep = '_')

p2 <- df2 %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x = fire_size, y = log(costs), fill = cause), group = cause) +
  geom_boxplot(position = position_dodge(width = 0.85)) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('') + ylab('log Fire suppression costs') +
  geom_vline(aes(xintercept = 7.5), color = 'black') +
  theme_pub() +
  facet_wrap(~class_coarse) +
  theme(legend.position = 'none')

tt2 <- ggplot_build(p2)$data[[1]] %>%
  dplyr::select(fill, group, PANEL, ymax_final) %>%
  as_tibble() %>%
  mutate(fire_size = case_when(
    group == 1 | group == 2 ~ '0-100',
    group == 3 | group == 4 ~ '100-200',
    group == 5 | group == 6 ~ '200-400', 
    group == 7 | group == 8 ~ '400-1k',
    group == 9 | group == 10 ~ '1k-10k',
    group == 11 | group == 12 ~ '10k-50k',
    group == 13 | group == 14 ~ '> 50k'),
    class_coarse = case_when(
      PANEL == 1 ~ 'WUI',
      PANEL == 2 ~ 'VLD', 
      PANEL == 3 ~ 'Wildlands'),
    cause = case_when(
      fill == "#1F77B4" ~ 'Lightning',
      fill == "#D62728" ~ 'Human')) %>%
  dplyr::select(class_coarse, fire_size, cause, ymax_final) %>%
  left_join(aov.models2, ., by = c('fire_size', 'class_coarse', 'cause')) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands')))

p1_final <- p1 + geom_text(aes(x=fire_size, y=1.4 +ymax_final, label=groups), data=tt, size = 4,
                           position = position_dodge(width = 0.85))

p2_final <- p2 + geom_text(aes(x=fire_size, y=1.4 +ymax_final, label=groups), data=tt2, size = 4,
                           position = position_dodge(width = 0.85)) +
  theme(strip.text.x = element_blank())

p_final <- cowplot::plot_grid(p2_final, p1_final,
                              labels = c('',''), label_x = 0.2, nrow = 2)

save_plot(file.path(main_text_figs, "figure4.tiff"), p_final,
          base_aspect_ratio = 3, base_height = 5.5, base_width = 8)

system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
