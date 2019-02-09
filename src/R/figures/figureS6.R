df <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
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
  dplyr::select(class, fire_size, build_up_count_no_zero_0, ignition) %>%
  filter(build_up_count_no_zero_0 != 0) %>%
  unite(ignition_size, c('ignition', 'fire_size')) %>%
  mutate(ignition_size = as.factor(ignition_size),
         build_up_count_no_zero_0 = log(build_up_count_no_zero_0)) %>%
  split(.$class) %>%
  map(~ aov(build_up_count_no_zero_0 ~ ignition_size, data = .x)) %>%
  map( agricolae::HSD.test, trt = 'ignition_size', group=TRUE) %>%
  lapply(., '[', c(5)) %>%
  lapply(., function(x) {
    rownames_to_column(as.data.frame(x), var = "ignition_size") %>%
      dplyr::select(ignition_size, 
                    build_up_count_no_zero_0 = groups.build_up_count_no_zero_0,
                    groups = groups.groups)}) %>%
  dplyr::bind_rows(., .id = 'class') %>%
  separate(ignition_size, c('ignition', 'fire_size'), sep = '_')

p1 <- df %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  ggplot(aes(x = fire_size, y = log(build_up_count_no_zero_0), fill = ignition), group = ignition) +
  geom_boxplot(position = position_dodge(width = 0.85)) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Fire Size (ha)') + ylab('log Average home threatened per fire event') +
  theme_pub() +
  facet_wrap(~class) +
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
    class = case_when(
      PANEL == 1 ~ 'Interface WUI',
      PANEL == 2 ~ 'Intermix WUI',
      PANEL == 3 ~ 'VLD', 
      PANEL == 4 ~ 'Wildlands'),
    ignition = case_when(
      fill == "#1F77B4" ~ 'Lightning',
      fill == "#D62728" ~ 'Human')) %>%
  dplyr::select(class, fire_size, ignition, ymax_final) %>%
  left_join(aov.models, ., by = c('fire_size', 'class', 'ignition'))

p1 + geom_text(aes(x=fire_size, y=1.1 +ymax_final, label=groups), data=tt, fontface = "bold",
               position = position_dodge(width = 0.85))

#





ggsave(file.path(supplements_text_figs, "figureS6.tiff"), p1, width = 7, height = 7, dpi = 1200, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))

df <- as_tibble(as.data.frame(bu_ics_complete_cleaned)) %>%
  filter(cause != 'Unk') %>%
  filter(built_class == 'Residential') %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Interface WUI', 'Intermix WUI', 'VLD', 'Wildlands'))) %>%
  mutate(fire_size_ha = area_km2*100,
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
  dplyr::select(class, fire_size, costs) %>%
  gather(variable, value, -fire_size, -class) %>%
  split(.$class) %>%
  map(~ aov(value ~ fire_size, data = .x)) %>%
  map( agricolae::HSD.test, trt = 'fire_size', group=TRUE) %>%
  lapply(., '[', c(5)) %>%
  lapply(., function(x) {
    rownames_to_column(as.data.frame(x), var = "fire_size") %>%
      dplyr::select(fire_size, 
                    costs = groups.value,
                    groups = groups.groups)}) %>%
  dplyr::bind_rows(., .id = "class") 
  
p1 <- df %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  ggplot(aes(x = fire_size, y = log(costs))) +
  geom_boxplot(fill = 'lightgray') +
  xlab('Fire Size (ha)') + ylab('log Fire suppression costs') +
  theme_pub() +
  facet_wrap(~class)

tt <- ggplot_build(p1)$data[[1]] %>%
  dplyr::select(group, PANEL, ymax_final) %>%
  as_tibble() %>%
  mutate(fire_size = case_when(
    group == 1 ~ '0-100',
    group == 2 ~ '100-200',
    group == 3 ~ '200-400', 
    group == 4 ~ '400-1k',
    group == 5 ~ '1k-10k',
    group == 6 ~ '10k-50k',
    group == 7 ~ '> 50k'),
    class = case_when(
      PANEL == 1 ~ 'Interface WUI',
      PANEL == 2 ~ 'Intermix WUI',
      PANEL == 3 ~ 'VLD', 
      PANEL == 4 ~ 'Wildlands')) %>%
  dplyr::select(class, fire_size, ymax_final) %>%
  left_join(aov.models, ., by = c('fire_size', 'class'))

p1 + geom_text(aes(x=fire_size, y=1.1 +ymax_final, label=groups), data=tt, fontface = "bold")


ggsave(file.path(supplements_text_figs, "figureS10.tiff"), p1, width = 7, height = 7, dpi = 1200, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))

p1 <- df %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  ggplot(aes(x = fire_size, y = log(costs), fill = cause)) +
  geom_boxplot(position = position_dodge()) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Fire Size (ha)') + ylab('log Fire suppression costs') +
  theme_pub() +
  facet_wrap(~class) +
  theme(legend.position = 'none')

ggsave(file.path(supplements_text_figs, "figureS11.tiff"), p1, width = 7, height = 7, dpi = 1200, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
