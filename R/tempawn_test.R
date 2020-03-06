library(SWATplusR)
library(tidyverse)
library(lhs)
library(foreach)
library(lubridate)


pth <- load_demo("pro", "~/Documents", "plus")


par_bound <- tibble("cn2.hru | change = abschg" = c(-15, 10),
                    "lat_ttime.hru | change = absval" = c(0.5, 50),
                    "lat_len.hru | change = absval" = c(10, 100),
                    "k.sol | change = pctchg" = c(-50, 50),
                    "z.sol | change = pctchg" = c(-50, 50),
                    "esco.hru | change = absval" = c(0, 1),
                    "epco.hru | change = absval" = c(0, 1))

n_sample <- 1000
n_par <- ncol(par_bound)

par_pwn <- randomLHS(n = n_sample, k = n_par) %>%
  as_tibble(., .name_repair = "minimal") %>%
  map2_df(., par_bound, ~ (.x * (.y[2] - .y[1]) + .y[1])) %>%
  set_names(names(par_bound))


sim_pwn  <- run_swatplus(project_path = pth,
                       output = list(q = define_output(file = "channel",
                                                       variable = "flo_out",
                                                       unit = 1),
                                     no3 = define_output(file = "channel",
                                                         variable = "no3_out",
                                                         unit = 1)),
                       parameter = par_pwn,
                       start_date = "2000-01-01",
                       end_date = "2003-12-31",
                       years_skip = 1,
                       n_thread = 4)

saveRDS(sim_pwn, "~/Documents/sim_pwn.rds")


tempawn <- function(sim, stat, bins) {
  tgt <- sim$simulation %>% 
    map(., ~select(.x, -date)) %>% 
    map(., ~t(.x))
  
  n_t <- 365
  n_t <- dim(tgt[[1]])[2]
  n_sim <- nrow(sim$parameter$values)
  idx <- 1:n_sim
  
  n_dummy <- 100
  dummy <- runif(n_dummy*n_sim) %>% 
    matrix(., ncol = n_dummy) %>% 
    as_tibble(., ) %>% 
    set_names(., paste("dummy", 1:n_dummy, sep = "_"))
  
  inp <- sim$parameter$values %>% 
    bind_cols(., dummy) %>% 
    # add_column(., dummy = runif(nrow(.))) %>% 
    map(., ~cut(.x, breaks = bins, labels = FALSE))
  
  inp_name <- names(inp)

  n_core <- detectCores()
  cl <- makeCluster(n_core)
  registerDoSNOW(cl)
  t0 <- now()
  progress <- function(n){
    display_progress(n, n_t, t0)
  }
  opts <- list(progress = progress)
  
  res_list <- foreach(t_i = 1:n_t,
                      .packages = c("tibble", "purrr", "dplyr", "lubridate"), 
                      .options.snow = opts) %dopar% {
                      pawn_ij <- function(inp_tgt_ij, idx, stat) {
                      map_df(inp_tgt_ij, ~ pawn_bin(dat = .x, idx = idx, stat = stat))
                          }
                  inp_tgt_i <- tgt %>% 
                    map(., ~.x[,t_i]) %>% 
                    map(., ~ merge_inp_tgt(inp, .x))
                    aa_i <- pawn_i(inp_tgt_i, idx, stat)
                    return(aa_i)
                        }
  
  saveRDS(res_list, "~/Documents/res_list.rds")
  res_tbl <- res_list %>% 
    transpose(.) %>% 
    map(., ~bind_rows(.x)) %>% 
    map(., ~compute_dummy_95(.x)) %>% 
    map(., ~add_column(., date = sim$simulation[[1]]$date, .before = 1))
  
}

compute_dummy_95 <- function(res_i){
  dummy_95 <- res_i %>% 
    select(starts_with("dummy_")) %>% 
    apply(., 1, quantile, probs = 0.95)
  
  res_95 <- res_i %>% 
    select(-starts_with("dummy_")) %>% 
    add_column(., dummy = dummy_95)
}

  merge_inp_tgt <- function(inp, tgt) {
    map(inp, ~tibble(input = .x, target = tgt))
  }

pawn_i <- function(inp_tgt_i,  idx, stat) {
  map(inp_tgt_i, ~ pawn_ij(inp_tgt_ij = .x, idx = idx, stat = stat))
}
  

#' Calculate pawn index for a sample defined by the indices idx (required for boot)
#' @importFrom dplyr slice group_by summarise ungroup %>%
#' @keywords internal
pawn_ij <- function(inp_tgt_ij, idx, stat) {
  map_df(inp_tgt_ij, ~ pawn_bin(dat = .x, idx = idx, stat = stat))
}

#' Calculate pawn index for a sample defined by the indices idx (required for boot)
#' @importFrom dplyr slice group_by summarise ungroup %>%
#' @keywords internal
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
display_progress <- function(n, nmax, t0){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")
  prgs <- paste0(round(n/nmax*100, digits = 0), "%")
  
  cat("\r", "Progress:", prgs,
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}


var_name <- names(res_tbl)

gg_tbl <- res_tbl %>% 
  map2(., var_name, ~add_column(.x, var = .y, .before = 1)) %>% 
  map_df(., ~gather(data = .x, key = "par", value = "val", -date, -var, - dummy)) %>% 
  mutate(val = val - dummy,
         val = ifelse(val < 0, 0, val))

ggplot(data = gg_tbl) + 
  geom_col(aes(x = date, y = val, col = var, fill = var)) + 
  # geom_area(aes(x = date, y = dummy, group = var), fill = "white", col = "white") + 
  theme_bw() + 
  facet_grid(par~var)
