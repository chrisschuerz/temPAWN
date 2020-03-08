
plot.tempawn <- function(tempawn) {

  var_name <- names(tempawn$sensitivity)

  if("dummy" %in% names(tempawn$sensitivity[[1]])) {
    sens_tbl <- tempawn$sensitivity %>%
      map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
      map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var, - dummy)) %>%
      mutate(val = val - dummy,
             val = ifelse(val < 0, 0, val))

    n_par <- ncol(tempawn$sensitivity[[1]]) - 2
  } else {
    sens_tbl <- tempawn$sensitivity %>%
      map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
      map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var))

    n_par <- ncol(tempawn$sensitivity[[1]]) - 1
  }

  gg_sens <- ggplot(data = sens_tbl) +
    geom_area(aes(x = date, y = val), fill = "grey30") +
    # geom_area(aes(x = date, y = dummy, group = var), fill = "white", col = "white") +
    theme_bw() +
    theme(strip.background.x = element_blank(),
          strip.text.x = element_blank()) +
    labs(x = "Date", y = "Sensitivity") +
    facet_grid(par~var)

  sim_tbl <- tempawn$simulation %>%
    map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
    bind_rows(.)

  gg_sim <- ggplot(data = sim_tbl) +
    geom_ribbon(aes(x = date, ymin = sim_min, ymax = sim_max), fill = "grey30") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(x = "Date", y = "Simulation") +
    facet_grid(.~var)


  gg_sim/gg_sens + plot_layout(heights = c(2,n_par))
}
