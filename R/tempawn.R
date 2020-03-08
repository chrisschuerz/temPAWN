#' Calculate temporal PAWN indices
#'
#' tempawn calculates the PAWN sensitivity index for each time step of simulation
#' results that were generated with the run_swat*() functions from the SWATplusR
#' package. The PAWN method is implemented according to Pianosi and Wagener (2018).
#' tempawn optionally includes the computation of the temporal sensitivity of a
#' dummy parameter.
#'
#'
#' @param sim The simulation output object that results from a simulation with
#'   run_swat2012() or run_swatplus() from the SWATplusR package.
#' @param var String vector of simulated variables that should be included in the
#'   sensitivity analysis. \code{Default = NULL} means all variables are included.
#' @param stat The summary statistics that is used to compute the PAWN sensitivity
#'   index. Pianosi and Wagener (2015) suggest to implement the median or max.
#'   \code{Default = median}
#' @param bins Number of bins into which each parameter dimension is separated.
#' \code{Default = 25}
#' @param dummy \code{dummy = TRUE} includes the calculation of the dummy sensitivity.
#' This option can tremendously increase the calculation time, but provides very
#' important information. \code{Default = TRUE}
#' @param cores Parallel computing is implemented to reduce computation time. By
#' default all cores are used. If a lower number of cores should be used set cores
#' to a certain value.
#'

#' @return Returns a list of tables where each table provides the sensitivities
#'   for each time step of the simulation outputs to all the analyzed model
#'   parameters.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>% bind_cols bind_rows select
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate now
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom purrr map set_names transpose
#' @importFrom tibble add_column as_tibble tibble
#'
#' @references
#' \cite{Pianosi, F. and Wagener T.: Distribution-based sensitivity analysis
#'       from a generic input-output sample, Environmental Modelling & Software,
#'       108, 197-207, \url{https://doi.org/10.1016/j.envsoft.2018.07.019}, 2018}
#'
#'
#' \cite{Pianosi, F. and Wagener T.: A simple and efficient method for global
#'       sensitivity analysis based on cumulative distribution functions,
#'       Environmental Modelling & Software, 67, 1-11,
#'       \url{https://doi.org/10.1016/j.envsoft.2015.01.004}, 2015}
#' @export
#'
tempawn <- function(sim, var = NULL, stat = median, bins = 25, dummy = TRUE, cores = NULL) {

  cores <- min(cores,detectCores())

  var_names <- names(sim$simulation)

  if (is.null(var)) {
    var <- var_names
  } else if (!any(var %in% var_names)) {
    stop("At least one of the variables not available in 'sim$simulation'")
  }

  cat("Computation started on", cores, "cores:\n")

  t0 <- now()
  sim_stat <- list()
  for (i in 1:length(var)) {
    sim_stat[[var[i]]] <- get_sim_stat(sim$simulation[[var[i]]])
    display_progress(i, length(var), t0, "simulation statistics:")
  }
  finish_progress(t0, "simulation statistics")

  tgt <- sim$simulation %>%
    .[var] %>%
    map(., ~select(.x, -date)) %>%
    map(., ~t(.x))


  n_t <- dim(tgt[[1]])[2]
  n_sim <- nrow(sim$parameter$values)
  idx <- 1:n_sim

  inp <- sim$parameter$values

  # Generate 100 dummy parameters to assess 95% quantile for random sensitivity.
  if(dummy) {
    n_dummy <- 100
    dummy <- runif(n_dummy*n_sim) %>%
      matrix(., ncol = n_dummy) %>%
      as_tibble(., ) %>%
      set_names(., paste("dummy", 1:n_dummy, sep = "_"))

    inp <- bind_cols(inp, dummy)
  }

  inp <- map(inp, ~cut(.x, breaks = bins, labels = FALSE))

  inp_name <- names(inp)


  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  t0 <- now()
  progress <- function(n){
    display_progress(n, n_t, t0,  "sensitivity analysis:")
  }
  opts <- list(progress = progress)

  res_list <- foreach(t_i = 1:n_t,
                      .packages = c("tibble", "purrr", "dplyr", "lubridate"),
                      .options.snow = opts) %dopar% {
                        inp_tgt_i <- tgt %>%
                          map(., ~.x[,t_i]) %>%
                          map(., ~ merge_inp_tgt(inp, .x))
                        t_i <- pawn_i(inp_tgt_i, idx, stat)
                        return(t_i)
                      }

  stopCluster(cl)

  res_tbl <- res_list %>%
    transpose(.) %>%
    map(., ~bind_rows(.x)) %>%
    map(., ~compute_dummy_95(.x)) %>%
    map(., ~add_column(., date = sim$simulation[[1]]$date, .before = 1))

  finish_progress(t0, "sensitivity analysis")

  res <- list(sensitivity = res_tbl,
              simulation  = sim_stat)

  class(res) <- "tempawn"

  return(res)
}

#' Compute min/max values for the simulation of each time step
#'
#' @param sim_tbl Table with simulated time series
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap_dbl set_names
#' @importFrom tibble add_column
#' @keywords internal
#'
get_sim_stat <- function(sim_tbl) {
  sim_tbl %>%
    select(-date) %>%
    mutate(q_max = pmap_dbl(., max),
           q_min = pmap_dbl(., min)) %>%
    select(q_min, q_max) %>%
    set_names(c("sim_min", "sim_max")) %>%
    add_column(date = sim_tbl$date, .before = 1)
}


#' Compute pawn_bin for all inputs and one target
#'
#' @param inp_tgt_i List that provides the input and the target pairs for all inputs
#' @param idx index that defines the bin classes
#' @param stat The summary statistics function that is implemented to compute
#'   the PAWN index.
#'
#' @importFrom purrr map
#' @keywords internal
#'
pawn_i <- function(inp_tgt_i,  idx, stat) {
  map(inp_tgt_i, ~ pawn_ij(inp_tgt_ij = .x, idx = idx, stat = stat))
}

#' Compute pawn_bin for one input and one target
#'
#' @param inp_tgt_ij Table that provides the input and the target pairs
#' @param idx index that defines the bin classes
#' @param stat The summary statistics function that is implemented to compute
#'   the PAWN index.
#'
#' @importFrom purrr map_df
#' @keywords internal
#'
pawn_ij <- function(inp_tgt_ij, idx, stat) {
  map_df(inp_tgt_ij, ~ pawn_bin(dat = .x, idx = idx, stat = stat))
}

#' Compute the PAWN index for the defined bins for one input and one target
#'
#' @param dat Table that provides the input and the target pairs
#' @param idx index that defines the bin classes
#' @param stat The summary statistics function that is implemented to compute
#'   the PAWN index.
#'
#' @importFrom dplyr %>% group_by slice summarise ungroup
#' @keywords internal
#'
pawn_bin <- function(dat, idx, stat) {
  dat <- slice(dat, idx)
  b <- dat$target

  dat %>%
    group_by(., input) %>%
    summarise(., target = suppressWarnings(ks.test(target, b)$statistic)) %>%
    ungroup(.) %>%
    .$target %>%
    stat(.)
}

#' Merge inputs and targets
#'
#' @param inp Input
#' @param tgt Target
#'
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @keywords internal
#'
merge_inp_tgt <- function(inp, tgt) {
  map(inp, ~tibble(input = .x, target = tgt))
}

#' Compute the 95% quantile of the target variable sensitivities for the dummy
#'
#' @param res_i Results for the sensitivity analysis for the target i
#'
#' @importFrom dplyr %>% select starts_with
#' @importFrom tibble add_column
#' @keywords internal
#'
compute_dummy_95 <- function(res_i){
  if("dummy_1" %in% names(res_i)) {
    dummy_95 <- res_i %>%
      select(starts_with("dummy_")) %>%
      apply(., 1, quantile, probs = 0.95)

    res_i <- res_i %>%
      select(-starts_with("dummy_")) %>%
      add_column(., dummy = dummy_95)
  }

  return(res_i)
}


#' Display the progress if iterative processes
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#' @keywords internal
#'
display_progress <- function(n, nmax, t0, word){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")
  prgs <- paste0(round(n/nmax*100, digits = 0), "%")

  cat("\r", "Progress", word, prgs,
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}

#' Print message for completed process
#'
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now
#' @keywords internal
#'
finish_progress <- function(t0, word) {
  cat("\r", paste0(rep(" ", 80), collapse = ""))
  interval(t0,now()) %>%
    round(.) %>%
    as.period(.) %>%
    as.character(.) %>%
    cat("\r","Completed", word, "in", ., "\n")
}
