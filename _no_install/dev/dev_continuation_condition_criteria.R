################################################################################
###
### DATE CREATED: 2025-06-04
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Develop continuation condition criteria 'Regain TFR' surf variant.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(here)

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


## traj <- make_tfr_estproj_traj(country = wppLoc::code("Zimbabwe"),
##                               sim.dir = bayesTFR_output_dir,
##                               out_format = "FPPlateaus")

## traj <- traj[55:70,,,drop = FALSE]



test_surfs <-
    make_tfr_surfs(country_codes = wppLoc::code("Montenegro"),
                    sim.dir = bayesTFR_output_dir,
                    median_only = FALSE,
                    transition_condition_type = "Phase II & >= 2.1 persistently",
                       rate_threshold = -0.01,
                       rate_prob_threshold = 0.8,
                       min_surf_length = 2,
                       min_inter_surf_length = 2,
                    continuation_condition = "Regain TFR, probabilistic",
                    exceedance_condition = "Max TFR > 2.1, probabilistic",
                       smoothing_method = "local_linear",
                       bandwidth = 3,
                       year_lim = c(1950, 2100),
                       ncores = NULL
                    )

test_surfs_median <-
    make_tfr_surfs(country_codes = wppLoc::code("Poland"),
                    sim.dir = bayesTFR_output_dir,
                    median_only = TRUE,
                    transition_condition_type = "Phase II & >= 2.1 persistently",
                    rate_threshold = -0.01,
                    rate_prob_threshold = 0.8,
                    min_surf_length = 2,
                    min_inter_surf_length = 2,
                    continuation_condition = "Regain TFR, probabilistic",
                    exceedance_condition = "Max TFR > 2.1, probabilistic",
                    smoothing_method = "local_linear",
                    bandwidth = 3,
                    year_lim = c(1950, 2100),
                    ncores = NULL
                    )

test_surfs_tbl <- tabulate_surf_periods(test_surfs)

test_surfs_plot <-
    plot_tfr_surfs(test_surfs, x_alt = test_surfs_median, yvar = "TFR_median")

plot_tfr_surfs(test_surfs, x_alt = test_surfs_median,
                x_alt_label = "Median only",
                yvar = "TFR_median")


test_surfs_list <-
    make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                    incl_small_countries = FALSE,
                       transition_condition_type = "Phase II & never < 2.1",
                       rate_threshold = -0.01,
                       rate_prob_threshold = 0.8,
                       min_surf_length = 1,
                       min_inter_surf_length = 2,
                       continuation_condition = "Regain TFR, probabilistic",
                       smoothing_method = "local_linear",
                       bandwidth = 3,
                       year_lim = c(1950, 2100)## ,
                       ## ncores = NULL
                    )

test_surfs_median_list <-
    make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                    incl_small_countries = FALSE,
                    median_only = TRUE,
                    transition_condition_type = "Phase II & never < 2.1",
                    rate_threshold = -0.01,
                    rate_prob_threshold = 0.8,
                    min_surf_length = 1,
                    min_inter_surf_length = 2,
                    continuation_condition = "Regain TFR, probabilistic",
                    smoothing_method = "local_linear",
                    bandwidth = 3,
                    year_lim = c(1950, 2100)## ,
                    ## ncores = NULL
                    )

test_surfs_list_plot <-
    plot_tfr_surfs(test_surfs_list, x_alt = test_surfs_median_list,
                x_alt_label = "Median only",
                yvar = "TFR_median")
