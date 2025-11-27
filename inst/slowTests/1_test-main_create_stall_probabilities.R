################################################################################
###
### DATE CREATED: 2025-06-06
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Test the main surfs identification functions. These are in
### 'inst/slowTests' because they need to use bayesTFR output. This is too big
### to distribute with the package, so can't be used in 'tests/'.
###
###-----------------------------------------------------------------------------
###
### !!! You must run '0_setup.R' first !!!
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(tfrSURFs)

source(system.file("slowTests", "0_setup.R", package = "tfrSURFs"))

###-----------------------------------------------------------------------------
### * bayesTFR Test Run

### Create or find the bayesTFR test run.
### It if doesn't exist, it will be unpacked into a temporary directory.

## Location of bayesTFR results
## NOTE: This is the location in source form; change accordingly.
bayesTFR_short_test_file_path <- here::here("data-raw", "bigData", "bayesTFR_short_test.tar.gz")
if (!file.exists(bayesTFR_short_test_file_path))
    stop("'", bayesTFR_short_test_file_path, "' does not exist: cannot run tests without it.")

bayesTFR_test_output_dir <-
    setup_bayesTFR_test_data_temp_dir(file = bayesTFR_short_test_file_path)

###-----------------------------------------------------------------------------
### * Tests

test_that("main function works with default arguments -- ONE country", {

    pbop <- pbapply::pboptions(type = "none")
    withr::defer(pbapply::pboptions(pbop))

    expect_s3_class((
        test <- make_tfr_surfs(country_codes = 716,
                                sim.dir = bayesTFR_test_output_dir)),
        "data.frame")
})


test_that("main function works with default arguments -- ALL countries", {

    pbop <- pbapply::pboptions(type = "none")
    withr::defer(pbapply::pboptions(pbop))

    expect_error((
        test <- make_tfr_surfs(sim.dir = bayesTFR_test_output_dir)),
        NA)
})


test_that("'make_tfr_surfs' works on one country with a variety of argument configurations", {

    pbop <- pbapply::pboptions(type = "none")
    withr::defer(pbapply::pboptions(pbop))

    param_df <- make_tfr_surfs_param_df()

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        message("[", param_i, " of ", nrow(param_df), "]: ",
                paste(colnames(param_df), paste0(pars, "; "), sep = " = "))
        expect_s3_class((
            test <- make_tfr_surfs(country_code = 716,
                                    sim.dir = bayesTFR_test_output_dir,
                                    median_only = pars[["median_only"]],
                                    transition_condition_type = pars[["transition_condition_type"]],
                                    smoothing_method = pars[["smoothing_method"]],
                                    bandwidth = pars[["bandwidth"]],
                                 rate_threshold = pars[["rate_threshold"]],
                                 rate_prob_threshold = pars[["rate_prob_threshold"]],
                                 continuation_condition = pars[["exit_crition"]],
                                 continuation_condition_prob_threshold = pars[["continuation_condition_prob_threshold"]],
                                 exceedance_condition = pars[["exceedance_condition"]],
                                 exceedance_condition_prob_threshold = pars[["exceedance_condition_prob_threshold"]],
                                 min_surf_length = pars[["min_surf_length"]],
                                 min_inter_surf_length = pars[["min_inter_surf_length"]],
                                 incl_small_countries = pars[["incl_small_countries"]]
                                 )
        ), "data.frame")
    }
})


test_that("'make_tfr_surfs' works on multiple countries with a variety of argument configurations", {

    pbop <- pbapply::pboptions(type = "none")
    withr::defer(pbapply::pboptions(pbop))

    param_df <- make_tfr_surfs_param_df()

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        message("[", param_i, " of ", nrow(param_df), "]: ",
                paste(colnames(param_df), paste0(pars, "; "), sep = " = "),
                "country_codes = c(12, 508, 417, 764, 32, 250, 242)")
        test <- make_tfr_surfs(country_codes = c(12, 716, 508, 417, 764, 32, 250, 242),
                                sim.dir = bayesTFR_test_output_dir,
                                    median_only = pars[["median_only"]],
                                    transition_condition_type = pars[["transition_condition_type"]],
                                    smoothing_method = pars[["smoothing_method"]],
                                    bandwidth = pars[["bandwidth"]],
                                 rate_threshold = pars[["rate_threshold"]],
                                 rate_prob_threshold = pars[["rate_prob_threshold"]],
                                 continuation_condition = pars[["exit_crition"]],
                                 continuation_condition_prob_threshold = pars[["continuation_condition_prob_threshold"]],
                                 exceedance_condition = pars[["exceedance_condition"]],
                                 exceedance_condition_prob_threshold = pars[["exceedance_condition_prob_threshold"]],
                                 min_surf_length = pars[["min_surf_length"]],
                                 min_inter_surf_length = pars[["min_inter_surf_length"]],
                                 remove_small_countries = pars[["remove_small_countries"]],
                                 ncores = -2)
        expect_type(test, "list")
        expect_false(is.data.frame(test))

        message("[", param_i, " of ", nrow(param_df), "]: ",
                paste(colnames(param_df), paste0(pars, "; "), sep = " = "),
                "country_codes = NULL")
        test <- make_tfr_surfs(country_codes = NULL,
                                sim.dir = bayesTFR_test_output_dir,
                                    median_only = pars[["median_only"]],
                                    transition_condition_type = pars[["transition_condition_type"]],
                                    smoothing_method = pars[["smoothing_method"]],
                                    bandwidth = pars[["bandwidth"]],
                                 rate_threshold = pars[["rate_threshold"]],
                                 rate_prob_threshold = pars[["rate_prob_threshold"]],
                                 continuation_condition = pars[["exit_crition"]],
                                 continuation_condition_prob_threshold = pars[["continuation_condition_prob_threshold"]],
                                 exceedance_condition = pars[["exceedance_condition"]],
                                 exceedance_condition_prob_threshold = pars[["exceedance_condition_prob_threshold"]],
                                 min_surf_length = pars[["min_surf_length"]],
                                 min_inter_surf_length = pars[["min_inter_surf_length"]],
                                 remove_small_countries = pars[["remove_small_countries"]],
                                     ncores = -2)
        expect_type(test, "list")
        expect_false(is.data.frame(test))
    }
})
