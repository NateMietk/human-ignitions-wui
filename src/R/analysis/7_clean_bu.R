bu_cleaned <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui),
            by = "fpa_id") %>%
  filter(fire_size_km2 > 0.00025)


if (!file.exists(file.path(bu_out, 'bu_wuiblks.csv'))) {
  bu_wui_cleaned <- sum_wui_bu %>%
    setNames(tolower(names(.))) %>%
    gather(variable, bu, -id_sp, -blk10) %>%
    separate(variable,
             into = c("statistic", 'tmp', "year"),
             sep = "_") %>%
    dplyr::select(-tmp) %>%
    left_join(., as.data.frame(wui %>% setNames(tolower(names(.)))),
              by = "blk10") %>%
    dplyr::select(-matches('(geom|s10|flag|wuiclass|veg|water|shape)')) %>%
    filter(!(year %in% c('1995', '2005', '2015'))) 
  
  write_csv(bu_wui_cleaned, file.path(bu_out, 'bu_wuiblks.csv'))
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  
} else {
  
  bu_wui_cleaned <- read_csv(file.path(bu_out, 'bu_wuiblks.csv'))
  
}
