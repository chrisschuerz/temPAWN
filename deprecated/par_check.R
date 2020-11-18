library(SWATplusR)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)
library(ggplot2)

# Load parameter set for testing parameter influence
# You can also double click on the RData file to load it in the global environment
load("Your:/path/par.RData")

# Path to the TxtInOut folder of your project
pth <- "Set:/the/path/to/your/TxTInOut"

# Aqu unit is different to 1 depending on the number of aquifers. This routine
# finds the correct number of aquifers in your project.
find_aqu_unit <- function(project_path) {
  try(run_swatplus(project_path = pth,
                            output = list(snofall = define_output(file = "basin_aqu",
                                                                  variable = "seep",
                                                                  unit = 1)),
                            start_date = "2000-01-01",
                            end_date = "2000-01-02",
                            years_skip = 0,
                            keep_folder = T, quiet = T), silent = TRUE)
  aqu_file <- read_table(paste0(pth, "/.model_run/thread_1/basin_aqu_day.txt"), skip = 1)
  aqu_unit <- aqu_file$unit[2]
  return(aqu_unit)
}

aqu_unit <- find_aqu_unit(project_path = pth)


# Perform simulations and return large number of different water balance comonents
# that are returned to be analyzed

st_date <- "2008-01-01" # Start date for simulation incl skipped years
en_date <- "2013-12-31" # End date for simulation
n_yrs <- 3              # Years to skip for print

sim_par_chg <- run_swatplus(project_path = pth,
                        parameter = par,
                        output = list(snofall = define_output(file = "basin_wb",
                                                              variable = "snofall",
                                                              unit = 1),
                                      snomelt = define_output(file = "basin_wb",
                                                              variable = "snomlt",
                                                              unit = 1),
                                      surq_gen= define_output(file = "basin_wb",
                                                              variable = "surq_gen",
                                                              unit = 1),
                                      latq    = define_output(file = "basin_wb",
                                                              variable = "latq",
                                                              unit = 1),
                                      wateryld= define_output(file = "basin_wb",
                                                              variable = "wateryld",
                                                              unit = 1),
                                      perc    = define_output(file = "basin_wb",
                                                              variable = "perc",
                                                              unit = 1),
                                      et      = define_output(file = "basin_wb",
                                                              variable = "et",
                                                              unit = 1),
                                      sw      = define_output(file = "basin_wb",
                                                              variable = "sw",
                                                              unit = 1),
                                      snopack = define_output(file = "basin_wb",
                                                              variable = "snopack",
                                                              unit = 1),
                                      qtile   = define_output(file = "basin_wb",
                                                              variable = "qtile",
                                                              unit = 1),
                                      evap_ch = define_output(file = "basin_sd_cha",
                                                              variable = "evap",
                                                              unit = 1),
                                      seep_ch = define_output(file = "basin_sd_cha",
                                                              variable = "seep",
                                                              unit = 1),
                                      flo_out = define_output(file = "basin_sd_cha",
                                                              variable = "flo_out",
                                                              unit = 1),
                                      flo_aqu = define_output(file = "basin_aqu",
                                                              variable = "flo",
                                                              unit = aqu_unit),
                                      stor_aqu= define_output(file = "basin_aqu",
                                                              variable = "stor",
                                                              unit = aqu_unit),
                                      rchrg_aqu= define_output(file = "basin_aqu",
                                                              variable = "rchrg",
                                                              unit = aqu_unit),
                                      seep_aqu = define_output(file = "basin_aqu",
                                                              variable = "seep",
                                                              unit = aqu_unit)),
                        start_date = st_date,
                        end_date = en_date,
                        years_skip = n_yrs,
                        n_thread = 4)


# Function to calculate the fraction of days where a parameter change caused a
# change in the simulation
summarise_par_influence <- function(sim, idx) {
  sim$simulation %>%
    map(., ~.x[,idx:(idx+1)]) %>%
    map(., ~.x[1] - .x[2]) %>%
    map(., ~abs(.x) > 0) %>%
    map(., ~sum(.x)/length(.x)) %>%
    bind_rows()
}

# Calculation of the parameter influence table
idx <- seq(2,88, 2)
par_influence <- map(idx, ~ summarise_par_influence(sim_par_chg, .x)) %>%
  bind_rows(.)


# Visualization of the parameter influence table
gg_infl <- par_influence %>%
  add_column(parameter = factor(par_cal$par, levels = rev(par_cal$par)), .before = 1) %>%
  gather(., value = "value", key = "variable", -parameter)


ggplot(data = gg_infl, aes(x = variable, y = parameter)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_bw()

