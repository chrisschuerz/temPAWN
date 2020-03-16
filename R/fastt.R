#' Calculate temporal FAST indices
#'
#' fastt calculates the FAST sensitivity index for each time step of simulation
#' results that were generated with the run_swat*() functions from the SWATplusR
#' package. The FAST method is implemented according to Pianosi and Wagener (2018).
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
fastt <- function(sim, var = NULL, cores = NULL) {
  
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
    map(., ~add_column(., date = sim$simulation[[1]]$date, .before = 1))
  
  finish_progress(t0, "sensitivity analysis")
  
  res <- list(sensitivity = res_tbl,
              simulation  = sim_stat)
  
  class(res) <- "tempawn"
  
  return(res)
}