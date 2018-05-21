theme_map <- function(...) {
  theme_minimal(base_size=10) +
    theme(
      text = element_text(family = "", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "transparent", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

hist_plot <- function(df, regions, year, panel, upper = 250, conus = TRUE) {

  if(conus == FALSE) {
    p_df <- df %>%
      filter(region != as.character(regions)) %>%
      mutate(bui_ha = BUI*0.0001000000884,
             mean_bui = mean(BUI*0.0001000000884))
  } else {
    p_df <- df %>%
      mutate(bui_ha = BUI*0.0001000000884,
             mean_bui = mean(BUI*0.0001000000884))
  }

  p_template <- p_df %>%
    ggplot(aes(x = log(bui_ha))) +
    geom_histogram(binwidth = 0.25) +
    theme_pub() +
    facet_wrap(~fire_bidecadal)

    mx_cnt <- ggplot_build(p_template)$data[[1]] %>%
    tbl_df %>%
    dplyr::select(y, x, count, PANEL) %>%
    group_by(PANEL) %>%
    summarize(max_count = max(count)) %>%
    ungroup()

  mx_info <- ggplot_build(p_template)$data[[1]] %>%
    tbl_df %>%
    left_join(., mx_cnt, by = 'PANEL') %>%
    filter(y == max_count)

  mx_stats <- mx_info %>%
    mutate(fire_bidecadal = sort(unique(p_df$fire_bidecadal))) %>%
    right_join(., p_df, by = "fire_bidecadal") %>%
    dplyr::select(fire_bidecadal, x, y, BUI) %>%
    group_by(fire_bidecadal) %>%
    mutate(bui_ha = BUI*0.0001000000884,
           log_bui_ha = log(bui_ha)) %>%
    summarise(mean_bui = mean(bui_ha),
              pct_95th = quantile(bui_ha, probs = 0.95),
              logmean_bui = log(mean_bui),
              logpct_95th = log(pct_95th)) %>%
    ungroup()

  p <- p_df %>%
    filter(fire_bidecadal == as.numeric(year)) %>%
    mutate(bui_ha = BUI*0.0001000000884,
           mean_bui = mean(BUI*0.0001000000884)) %>%
    ggplot(aes(x = log(bui_ha))) +
    geom_histogram(binwidth = 0.5) +
    ylab("Counts") +
    xlab('log Built-up Intentsity (ha)') +
    scale_y_continuous(limits = c(0, upper)) +
    scale_x_continuous(limits = c(-8, 8)) +
    ggtitle(year) +
    theme_pub() +
    geom_text(aes(label = 'Peak:'), x = -7, y = upper, colour = "darkred", size = 5) +
    geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == as.character(panel)),
               linetype = "dashed", color  = "darkred") +
    geom_text(data=subset(mx_info, PANEL == as.character(panel)),
              aes(label=paste(formatC(round(exp(x)*10000, 1), format="f", big.mark=",", digits=1), "sq m"),
                  x = -5, y = upper - upper*0.056), colour = "darkred", size = 4) +

    geom_text(aes(label = 'Mean:'), x = -7, y = upper - upper*0.12, colour = "blue", size = 5) +
    geom_vline(aes(xintercept = logmean_bui), data = subset(mx_stats, fire_bidecadal ==  year),
               linetype = "dashed", color  = "blue") +
    geom_text(data = subset(mx_stats, fire_bidecadal ==  year),
              aes(label = paste(formatC(round(mean_bui*10000, 1), format="f", big.mark=",", digits=1), "sq m"),
                  x = -5, y = upper - upper*0.176), colour = "blue", size = 4) +

    geom_text(aes(label = '95th:'), x = -7, y = upper - upper*0.28, colour = "darkgreen", size = 5) +
    geom_vline(aes(xintercept = logpct_95th), data = subset(mx_stats, fire_bidecadal ==  year),
               linetype = "dashed", color  = "darkgreen") +
    geom_text(data = subset(mx_stats, fire_bidecadal ==  year),
              aes(label = paste(formatC(round(pct_95th*10000, 1), format="f", big.mark=",", digits=1), "sq m"),
                  x = -5, y = upper - upper*0.336), colour = "darkgreen", size = 4) +
    theme(legend.position = "none")
  p
}

hist_fbuy_plot <- function(df, regions, panel, upper = 250) {

  p_template <- df %>%
    filter(fbuy_mean != 0) %>%
    ggplot(aes(x = fbuy_mean)) +
    geom_histogram(binwidth = 5) +
    theme_pub()

  mx_cnt <- ggplot_build(p_template)$data[[1]] %>%
    tbl_df %>%
    dplyr::select(y, x, count, PANEL) %>%
    group_by(PANEL) %>%
    summarize(max_count = max(count)) %>%
    ungroup()

  mx_info <- ggplot_build(p_template)$data[[1]] %>%
    tbl_df %>%
    left_join(., mx_cnt, by = 'PANEL') %>%
    filter(y == max_count)

  mx_stats <- df %>%
    filter(fbuy_mean != 0) %>%
    group_by(region) %>%
    summarise(mean_fbuy = mean(fbuy_mean),
              median_fbuy = median(fbuy_mean),
              pct_95th = quantile(fbuy_mean, probs = 0.95)) %>%
    ungroup()

  p <- df %>%
    filter(fbuy_mean != 0) %>%
    ggplot(aes(x = fbuy_mean)) +
    geom_histogram(binwidth = 1) +
    ylab("Counts") +
    xlab('Mean FBUY') +
    scale_y_continuous(limits = c(0, upper)) +
    scale_x_continuous(limits = c(1850, 2015)) +
    ggtitle(regions) +
    theme_pub() +
    geom_text(aes(label = 'Peak:'), x = 1875, y = upper, colour = "darkred", size = 5) +
    geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == as.character(panel)),
               linetype = "dashed", color  = "darkred") +
    geom_text(data=subset(mx_info, PANEL == as.character(panel)),
              aes(label=round(x, 0),
                  x = 1865, y = upper - upper*0.056), colour = "darkred", size = 4) +

    geom_text(aes(label = 'Mean:'), x = 1875, y = upper - upper*0.12, colour = "blue", size = 5) +
    geom_vline(aes(xintercept = mean_fbuy), data = mx_stats,
               linetype = "dashed", color  = "blue") +
    geom_text(data = mx_stats,
              aes(label = round(mean_fbuy, 0),
                  x = 1865, y = upper - upper*0.176), colour = "blue", size = 4) +

    geom_text(aes(label = '95th:'), x = 1875, y = upper - upper*0.28, colour = "darkgreen", size = 5) +
    geom_vline(aes(xintercept = pct_95th), data = mx_stats,
               linetype = "dashed", color  = "darkgreen") +
    geom_text(data = mx_stats,
              aes(label = round(pct_95th, 0),
                  x = 1865, y = upper - upper*0.336), colour = "darkgreen", size = 4) +
    theme(legend.position = "none")
  p
}
