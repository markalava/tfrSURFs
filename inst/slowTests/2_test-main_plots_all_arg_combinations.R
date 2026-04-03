################################################################################
###
### DATE CREATED: 2026-04-03
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Test the main plot functions with all combinations of
### arguments. These are in 'inst/slowTests' because they take a very long time.
### They use the small test data sets distributed with the package.
###
###-----------------------------------------------------------------------------
###
### !!! You must run '0_setup.R' first !!!
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(doParallel)
library(foreach)
library(testthat)
library(tfrSURFs)

source(system.file("slowTests", "0_setup.R", package = "tfrSURFs"))

cl <- parallel::makeCluster(parallelly::availableCores(omit = 2))
doParallel::registerDoParallel(cl)

###-----------------------------------------------------------------------------
### * Tests

###-----------------------------------------------------------------------------
### * Set Up

###-----------------------------------------------------------------------------
### ** Data Frame Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.data.frame()' works, no x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {

                ## for (param_i in seq_len(nrow(param_df))) {
                ## cat("\n\ni = ", param_i, "\n\n")

                pars <- param_df[param_i, , drop = FALSE]
                suppressMessages(suppressWarnings(
                    plot_tfr_surfs(test_data_tfrSURFs_list[["716"]],
                                   yvar = pars[["yvar"]],
                                   add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                                   add_range_regions = pars[["add_range_regions"]],
                                   add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                                   add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                   maximal_legend = pars[["maximal_legend"]],
                                   add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                   add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                                   datestamp = pars[["datestamp"]]
                                   )))
            },
            NA)
})


test_that("'plot_tfr_surfs.data.frame()' works, with x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()
    k <- "716"

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {
                pars <- param_df[param_i, , drop = FALSE]

                plot_tfr_surfs(test_data_tfrSURFs_list[["716"]],
                               yvar = pars[["yvar"]],
                               x_alt = test_data_tfrSURFs_median_list[["716"]],
                               x_alt_label = "Median only",
                               add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                               add_range_regions = pars[["add_range_regions"]],
                               add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                               add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                               maximal_legend = pars[["maximal_legend"]],
                               add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                               add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                               datestamp = pars[["datestamp"]]
                               )
            },
            NA)
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.data.frame()' works, no x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()
    k <- "716"

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {

                pars <- param_df[param_i, , drop = FALSE]
                suppressMessages(suppressWarnings(
                    plot_surfs_probs(test_data_tfrSURFs_list[["716"]],
                                     yvar = pars[["yvar"]],
                                     add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                                     add_range_regions = pars[["add_range_regions"]],
                                     add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                                     add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                     maximal_legend = pars[["maximal_legend"]],
                                     add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                     add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                                     datestamp = pars[["datestamp"]]
                                     )))
            },
            NA)
})


test_that("'plot_surfs_probs.data.frame()' works, with x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()
    k <- "716"

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {
                pars <- param_df[param_i, , drop = FALSE]

                plot_surfs_probs(test_data_tfrSURFs_list[["716"]],
                                 yvar = pars[["yvar"]],
                                 x_alt = test_data_tfrSURFs_median_list[["716"]],
                                 x_alt_label = "Median only",
                                 add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                                 add_range_regions = pars[["add_range_regions"]],
                                 add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                                 add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                 maximal_legend = pars[["maximal_legend"]],
                                 add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                 add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                                 datestamp = pars[["datestamp"]]
                                 )
            },
            NA)
})

###-----------------------------------------------------------------------------
### ** List Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.list()' works, no x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {

                pars <- param_df[param_i, , drop = FALSE]
                suppressMessages(suppressWarnings(
                    plot_tfr_surfs(test_data_tfrSURFs_list,
                                   yvar = pars[["yvar"]],
                                   add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                                   add_range_regions = pars[["add_range_regions"]],
                                   add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                                   add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                   maximal_legend = pars[["maximal_legend"]],
                                   add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                   add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                                   datestamp = pars[["datestamp"]]
                                   )))
            },
            NA)
})


test_that("'plot_tfr_surfs.list()' works, with x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()
    k <- "716"

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {
                pars <- param_df[param_i, , drop = FALSE]

                plot_tfr_surfs(test_data_tfrSURFs_list,
                               yvar = pars[["yvar"]],
                               x_alt = test_data_tfrSURFs_median_list,
                               x_alt_label = "Median only",
                               add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                               add_range_regions = pars[["add_range_regions"]],
                               add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                               add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                               maximal_legend = pars[["maximal_legend"]],
                               add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                               add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                               datestamp = pars[["datestamp"]]
                               )
            },
            NA)
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.list()' works, no x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {

                pars <- param_df[param_i, , drop = FALSE]
                suppressMessages(suppressWarnings(
                    plot_surfs_probs(test_data_tfrSURFs_list,
                                     yvar = pars[["yvar"]],
                                     add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                                     add_range_regions = pars[["add_range_regions"]],
                                     add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                                     add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                     maximal_legend = pars[["maximal_legend"]],
                                     add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                     add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                                     datestamp = pars[["datestamp"]]
                                     )))
            },
            NA)
})


test_that("'plot_surfs_probs.list()' works, with x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()

    results <-
        expect_error(
            foreach(param_i = seq_len(nrow(param_df)), .packages = c("testthat", "tfrSURFs"), .export = "param_df") %dopar% {
                pars <- param_df[param_i, , drop = FALSE]

                plot_surfs_probs(test_data_tfrSURFs_list,
                                 yvar = pars[["yvar"]],
                                 x_alt = test_data_tfrSURFs_median_list,
                                 x_alt_label = "Median only",
                                 add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                                 add_range_regions = pars[["add_range_regions"]],
                                 add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                                 add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                 maximal_legend = pars[["maximal_legend"]],
                                 add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                 add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                                 datestamp = pars[["datestamp"]]
                                 )
            },
            NA)
})

###-----------------------------------------------------------------------------
### * END

stopCluster(cl)
