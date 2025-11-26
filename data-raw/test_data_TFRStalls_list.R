################################################################################
###
### DATE CREATED: 2025-06-16
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Create a small example run on a subset of countries.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(here)
library(withr)

## library(tfrSURFs)
devtools::load_all()            # Assumes working dir is this file's directory.

withr::local_dir(here::here("data-raw"), .local_envir = globalenv())

source(here::here("inst", "slowTests", "0_setup.R"))

## Input Data
bayesTFR_output_dir <-
    setup_bayesTFR_test_data_temp_dir(here::here("data-raw", "bigData",
                                                 "bayesTFR_short_test.tar.gz"))

###-----------------------------------------------------------------------------
### * Test Run

###-----------------------------------------------------------------------------
### ** Trajectories

## SELECTED COUNTRIES
test_data_tfrSURFs_list <-
    make_tfr_surfs(country_codes = c(12, 716, 508, 417, 764, 32, 250, 242),
                         sim.dir = bayesTFR_output_dir,
                    ncores = NULL # Need this because parallel running requires package to be fully installed
                    )


usethis::use_data(test_data_tfrSURFs_list,
                  internal = FALSE, overwrite = TRUE)


## SMALL COUNTRIES ONLY
test_data_small_c_tfrSURFs_list <-
    make_tfr_surfs(country_codes = c(184, 212, 500),
                    sim.dir = bayesTFR_output_dir,
                    incl_small_countries = TRUE,
                    ncores = NULL # Need this because parallel running requires package to be fully installed
                    )

usethis::use_data(test_data_small_c_tfrSURFs_list, internal = FALSE, overwrite = TRUE)

###-----------------------------------------------------------------------------
### ** Medians

## SELECTED COUNTRIES
test_data_tfrSURFs_median_list <-
    make_tfr_surfs(country_codes = c(12, 716, 508, 417, 764, 32, 250, 242),
                    sim.dir = bayesTFR_output_dir,
                    median_only = TRUE,
                    ncores = NULL # Need this because parallel running requires package to be fully installed
                    )

usethis::use_data(test_data_tfrSURFs_median_list,
                  internal = FALSE, overwrite = TRUE)


## SMALL COUNTRIES ONLY
test_data_small_c_tfrSURFs_median_list <-
    make_tfr_surfs(country_codes = c(184, 212, 500),
                    sim.dir = bayesTFR_output_dir,
                    incl_small_countries = TRUE,
                    median_only = TRUE,
                    ncores = NULL # Need this because parallel running requires package to be fully installed
                    )

usethis::use_data(test_data_small_c_tfrSURFs_median_list, internal = FALSE, overwrite = TRUE)
