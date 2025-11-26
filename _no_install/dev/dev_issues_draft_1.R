################################################################################
###
### DATE CREATED: 2025-08-07
###
### AUTHOR: Mark Wheldon
###
### PROJECT: TFR tfrSURFs
###
### DESCRIPTION: Investigate issues with surfs and plots for the IPC 2025
### version (draft 1).
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

stopifnot(requireNamespace("here"))
stopifnot(requireNamespace("wppLoc"))

setwd(here::here("_no_install", "dev"))

## library(tfrSURFs)
devtools::load_all()            # Assumes working dir is this file's directory.

source(here::here("inst", "slowTests", "0_setup.R"))

## Input Data
bayesTFR_output_dir <-
    setup_bayesTFR_test_data_temp_dir(here::here("data-raw", "bigData",
                                                 "bayesTFR_short_test.tar.gz"))

###-----------------------------------------------------------------------------
### * Testing

###-----------------------------------------------------------------------------
### ** Montenegro

(load(here::here("_no_install", "manuscript", "IPC_2025", "output", "rdata", "tfr_surfs_lst.rda")))

surf_monten <- tfr_surfs_lst[[as.character(wppLoc::code("Montenegro"))]]

###-----------------------------------------------------------------------------
### ** Portugal

args_list <- list(sim.dir = bayesTFR_output_dir,
                  median_only = FALSE,
                  transition_condition_type = "Phase II & >= 2.1 persistently",
                  smoothing_method = "local_linear",
                  bandwidth = 3,
                  rate_threshold = -0.01,
                  rate_prob_threshold = 0.8,
                  continuation_condition = "Regain TFR, probabilistic",
                  continuation_condition_prob_threshold = 0.8,
                  exceedance_condition = "Max TFR > 2.1, probabilistic",
                  exceedance_condition_prob_threshold = 0.8,
                  min_surf_length = 2,
                  min_inter_surf_length = 2,
                  year_lim = c(1950, 2100),
                  incl_small_countries = TRUE,
                  ncores = -2)

surf_port <-
    do.call("make_tfr_surfs",
            args = c(list(country_codes = wppLoc::code("Portugal")),
                     args_list))

###-----------------------------------------------------------------------------
### ** Jordan

plot_surfs_probs(tfr_surfs_lst[[as.character(wppLoc::code("Jordan"))]])
