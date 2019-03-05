fire_sizes <- as_tibble(as.data.frame(wui_209)) %>%
  filter(cause != 'Unk') %>%
  group_by(incident_unique_id) %>%
  summarise(fire_size_km2 = sum(area_km2))

df <- as_tibble(as.data.frame(wui_209)) %>%
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

aov.models <- df %>%
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

p1 <- df %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x = fire_size, y = log(costs), fill = cause), group = cause) +
  geom_boxplot(position = position_dodge(width = 0.85)) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Fire Size (ha)') + ylab('log Fire suppression costs') +
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
      # PANEL == 2 ~ 'Intermix WUI',
      PANEL == 2 ~ 'VLD', 
      PANEL == 3 ~ 'Wildlands'),
    cause = case_when(
      fill == "#1F77B4" ~ 'Lightning',
      fill == "#D62728" ~ 'Human')) %>%
  dplyr::select(class_coarse, fire_size, cause, ymax_final) %>%
  left_join(aov.models, ., by = c('fire_size', 'class_coarse', 'cause')) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands')))

p1 <- p1 + geom_text(aes(x=fire_size, y=1.1 +ymax_final, label=groups), data=tt, fontface = "bold",
               position = position_dodge(width = 0.85))

ggsave(file.path(supplements_text_figs, "figureS4.tiff"), p1, width = 7, height = 3, dpi = 600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir, ' --delete'))
