#' Plot analysis results of temoral PAWN analysis
#'
#' @param tempawn The list object that is returned by \code{tempawn()}.
#'
#' @return Returns a ggplot object with the visualization of the simulation ranges
#'   and the individual temoral parameter sensitivities.
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom dplyr %>% bind_rows filter group_by mutate select ungroup
#' @importFrom forcats fct_reorder fct_rev
#' @importFrom purrr map map2 map_df set_names transpose
#' @importFrom tibble add_column
#' @importFrom tidyr gather
#'
#' @export
#'
plot_tempawn <- function(tempawn, variable = NULL, significance = NULL, sort = TRUE, date_range = NULL) {


  if(!is.null(variable)) {
    stopifnot(all(variable %in% names(tempawn$sensitivity)))
    tempawn$sensitivity <- tempawn$sensitivity[variable]
    tempawn$simulation <- tempawn$simulation[variable]
  }

  var_name <- names(tempawn$sensitivity)

  if("dummy" %in% names(tempawn$sensitivity[[1]])) {
    sens_tbl <- tempawn$sensitivity %>%
      map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
      map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var, - dummy)) %>%
      mutate(val = val - dummy,
             val = ifelse(val < 0, 0, val))
  } else {
    sens_tbl <- tempawn$sensitivity %>%
      map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
      map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var))
  }

  if(!is.null(significance)) {
    sens_tbl <- sens_tbl %>%
      group_by(par) %>%
      filter(max(val) > significance) %>%
      ungroup()
  }

  if(sort) {
    sens_tbl <- sens_tbl %>%
      mutate(par = factor(par) %>% fct_reorder(., val, mean) %>% fct_rev())
  }

  sim_tbl <- tempawn$simulation %>%
    map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
    bind_rows(.)

  if(!is.null(date_range)) {
    stopifnot(length(date_range) == 2)
    date_range <- ymd(date_range)
    stopifnot(all(!is.na(date_range)))
    stopifnot(date_range[1] < date_range[2])
    sens_tbl <- filter(sens_tbl, date > date_range[1], date < date_range[2])
    sim_tbl  <- filter(sim_tbl,  date > date_range[1], date < date_range[2])
  }

  gg_sens <- ggplot(data = sens_tbl) +
    geom_area(aes(x = date, y = val), fill = "grey30") +
    # geom_area(aes(x = date, y = dummy, group = var), fill = "white", col = "white") +
    theme_bw() +
    theme(strip.background.x = element_blank(),
          strip.text.x = element_blank()) +
    labs(x = "Date", y = "Sensitivity")

  if(length(var_name) > 1) {
    gg_sens <- gg_sens + facet_grid(par~var)
  } else {
    gg_sens <- gg_sens + facet_grid(par~.)
  }

  gg_sim <- ggplot(data = sim_tbl) +
    geom_ribbon(aes(x = date, ymin = sim_min, ymax = sim_max), fill = "grey30") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(x = "Date", y = "Simulation") +
    facet_grid(.~var, scales = "free")

  n_par <- length(unique(sens_tbl$par)) - 2
  gg_sim/gg_sens + plot_layout(heights = c(2,n_par))
}


#' Plot analysis results of temoral PAWN analysis
#'
#' @param tempawn The list object that is returned by \code{tempawn()}.
#'
#' @return Returns a ggplot object with the visualization of the simulation ranges
#'   and the individual temoral parameter sensitivities.
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom dplyr %>% bind_rows filter group_by mutate select ungroup
#' @importFrom forcats fct_reorder fct_rev
#' @importFrom plotly ggplotly
#' @importFrom purrr map map2 map_df set_names transpose
#' @importFrom tibble add_column
#' @importFrom tidyr gather
#'
#' @export
#'
explore_tempawn <- function(tempawn, variable = NULL, significance = NULL, date_range = NULL) {

  stopifnot(length(variable) == 1)
  stopifnot(variable %in% names(tempawn$sensitivity))
  tempawn$sensitivity <- tempawn$sensitivity[variable]
  tempawn$simulation <- tempawn$simulation[variable]

  var_name <- names(tempawn$sensitivity)

  if("dummy" %in% names(tempawn$sensitivity[[1]])) {
    sens_tbl <- tempawn$sensitivity %>%
      map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
      map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var, - dummy)) %>%
      mutate(val = val - dummy,
             val = ifelse(val < 0, 0, val))
  } else {
    sens_tbl <- tempawn$sensitivity %>%
      map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
      map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var))
  }

  if(!is.null(significance)) {
    sens_tbl <- sens_tbl %>%
      group_by(par) %>%
      filter(max(val) > significance) %>%
      ungroup()
  }


  sim_tbl <- tempawn$simulation %>%
    map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>%
    bind_rows(.)

  if(!is.null(date_range)) {
    stopifnot(length(date_range) == 2)
    date_range <- ymd(date_range)
    stopifnot(all(!is.na(date_range)))
    stopifnot(date_range[1] < date_range[2])
    sens_tbl <- filter(sens_tbl, date > date_range[1], date < date_range[2])
    sim_tbl  <- filter(sim_tbl,  date > date_range[1], date < date_range[2])
  }

  sim_plt <- sim_tbl %>%
    # gather(data = ., key = "par", value = "val", -date) %>%
    mutate(plt_type = "Simulation",
           par = NA_character_,
           val = NA_real_) %>%
    select(date, par, val, plt_type, sim_min, sim_max)

  plt_tbl <- sens_tbl %>%
    select(date, par, val) %>%
    mutate(plt_type = "Sensitivity",
           sim_min = NA_real_,
           sim_max = NA_real_) %>%
    bind_rows(sim_plt) %>%
    mutate(plt_type = factor(plt_type, levels = c("Simulation", "Sensitivity"))) %>%
    mutate(par = factor(par) %>% fct_reorder(., val, mean) %>% fct_rev())

  gg_expl <- ggplot(plt_tbl) +
    geom_line(aes(x = date, y = val, col = par), lwd = 0.25)+
    geom_ribbon(aes(x = date, ymin = sim_min, ymax = sim_max), fill = "grey30") +
    theme_bw() +
    labs(x = "Date", y = "Value")+
    facet_grid(plt_type~., scales = "free")

  ggplotly(gg_expl)
}


#' Plot analysis results of temoral PAWN analysis
#'
#' @param tempawn The list object that is returned by \code{tempawn()}.
#'
#' @return Returns a ggplot object with the visualization of the simulation ranges
#'   and the individual temoral parameter sensitivities.
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom dplyr %>% bind_rows filter group_by left_join mutate select ungroup
#' @importFrom forcats fct_reorder fct_rev
#' @importFrom plotly ggplotly
#' @importFrom purrr map map2 map_df map_lgl set_names transpose
#' @importFrom tibble add_column tibble
#' @importFrom tidyr gather
#'
#' @export
#'
explore_parameter <- function(simulation, parameter, bins = 10, variable = NULL, observation = NULL) {

  stopifnot(length(parameter) == 1)
  stopifnot(parameter %in% simulation$parameter$definition$par_name)

  par <- simulation$parameter$values[,parameter] %>%
    set_names("value") %>%
    mutate(level = cut(x = value, breaks = bins))

  level_idx <- map(levels(par$level), ~ which(par$level == .x))

  if(!is.null(variable)) {
    stopifnot(all(variable %in% names(simulation$simulation)))
    sim <- simulation$simulation[variable]
  } else {
    sim <- simulation$simulation
  }

  if(!is.null(observation)) {
    stopifnot(all(names(observation) %in% names(sim)))
    stopifnot(all(map_lgl(observation, ~ is.Date(.x[[1]]))))
    stopifnot(all(map_lgl(observation, ~ is.double(.x[[2]]))))
    observation <- observation %>%
      map(., ~ set_names(.x, c("date", "obs"))) %>%
      map(., ~ left_join(select(sim[[1]], date), .x, by = "date")) %>%
      map2(., names(.), ~ mutate(.x, var = .y)) %>%
      bind_rows()
  }

  cut_sim <- function(sim, idx) {
    map(idx, ~ sim[ , c(1, .x + 1)])
  }
  get_sim_range <- function(sim) {
    # sim %>%
    #   select(-date) %>%
    #   mutate(q_025 = pmap_dbl(., ~ quantile(.x, probs = 0.025)),
    #          q_975 = pmap_dbl(., ~ quantile(.x, probs = 0.975)),
    #          date = sim$date) %>%
    #   select(date, q_025, q_975)

    sim_quantiles <- sim %>%
      select(-date) %>%
      apply(., 1, quantile, probs = c(0.025,0.975))

    tibble(date = sim$date,
           sim_lwr = sim_quantiles[1,],
           sim_upr = sim_quantiles[2,])
  }

  sim <- map(sim, ~ cut_sim(.x, level_idx)) %>%
    map(., ~ map(.x, ~ get_sim_range(.x))) %>%
    map(., ~ map2(., levels(par$level), ~ mutate(.x, level = .y))) %>%
    map(., ~ bind_rows(.x)) %>%
    map2(., names(.), ~mutate(.x, var = .y)) %>%
    bind_rows(.) %>%
    mutate(level = factor(level, levels = levels(par$level)))

  pal_rdylbu <- colorRampPalette(brewer.pal(11,"RdYlBu"))

  sim_plt <- ggplot(data = sim) +
    geom_ribbon(aes(x = date, ymin = sim_lwr, ymax = sim_upr, fill = level), alpha = 0.5)

  if(!is.null(observation)) {
    sim_plt <- sim_plt +
      geom_point(data = observation, aes(x = date, y = obs), size = 0.2)
  }

  sim_plt <- sim_plt +
    labs(x = "Date", y = "Value", fill = parameter) +
    scale_fill_manual(values = pal_rdylbu(bins)) +
    facet_wrap(.~var, ncol = 1, scales = "free_y", strip.position = "top") +
    theme_bw() +
    theme(strip.background = element_blank())

  ggplotly(sim_plt)
}



# observation <- list(flow_flut = obs$q_fluttendorf,
#                     no3n_flut = left_join(obs$n_fluttendorf, obs$q_fluttendorf, by = "date") %>%
#                                   mutate(nload = value.x*value.y*86.4) %>%
#                                   select(date, nload))
#
#
