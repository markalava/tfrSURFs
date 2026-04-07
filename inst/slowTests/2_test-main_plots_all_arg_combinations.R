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

ncores <- parallelly::availableCores(omit = 2)
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE, after = FALSE)

###-----------------------------------------------------------------------------
### * Tests

###-----------------------------------------------------------------------------
### * Set Up

###-----------------------------------------------------------------------------
### ** Data Frame Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.data.frame()' works.", {

    param_df <- make_plot_tfr_surfs_param_df("plot_tfr_surfs.data.frame")
    results <-
        expect_error(foreach(param_i = seq_len(nrow(param_df)),
                .export = c("param_i_error_msg", "expect_no_error_plot"),
                .packages = c("testthat", "tfrSURFs")) %dopar% {
                    pars <- param_df[param_i, , drop = FALSE]
                    expect_no_error_plot(param_i = param_i, pars = pars,
                                         plot_fn = "plot_tfr_surfs",
                                         x = test_data_tfrSURFs_list[["716"]],
                                         x_alt = test_data_tfrSURFs_median_list[["716"]])
                },
                NA)
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.data.frame()' works.", {

    param_df <- make_plot_tfr_surfs_param_df("plot_surfs_probs.data.frame")

        results <-
            expect_error(foreach(param_i = seq_len(nrow(param_df)),
                    .export = c("param_i_error_msg", "expect_no_error_plot"),
                    .packages = c("testthat", "tfrSURFs")) %dopar% {
                        pars <- param_df[param_i, , drop = FALSE]
                        expect_no_error_plot(param_i = param_i, pars = pars,
                                             plot_fn = "plot_surfs_probs",
                                             x = test_data_tfrSURFs_list[["716"]],
                                             x_alt = test_data_tfrSURFs_median_list[["716"]])
                    },
                    NA)
    })

###-----------------------------------------------------------------------------
### ** List Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.list()' works.", {

    param_df <- make_plot_tfr_surfs_param_df("plot_tfr_surfs.list")

    results <-
        expect_error(foreach(param_i = seq_len(nrow(param_df)),
                .export = c("param_i_error_msg", "expect_no_error_plot"),
                .packages = c("testthat", "tfrSURFs")) %dopar% {
                    pars <- param_df[param_i, , drop = FALSE]
                    expect_no_error_plot(param_i = param_i, pars = pars,
                                         plot_fn = "plot_tfr_surfs",
                                         x = test_data_tfrSURFs_list[c("716", "404", "508")],
                                         x_alt = test_data_tfrSURFs_median_list[c("716", "404", "508")])
                },
                NA)
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.list()' works.", {

    param_df <- make_plot_tfr_surfs_param_df("plot_surfs_probs.list")

    results <-
        expect_error(foreach(param_i = seq_len(nrow(param_df)),
                .export = c("param_i_error_msg", "expect_no_error_plot"),
                .packages = c("testthat", "tfrSURFs")) %dopar% {
                    pars <- param_df[param_i, , drop = FALSE]
                    expect_no_error_plot(param_i = param_i, pars = pars,
                                         plot_fn = "plot_surfs_probs",
                                         x = test_data_tfrSURFs_list[c("716", "404", "508")],
                                         x_alt = test_data_tfrSURFs_median_list[c("716", "404", "508")])
                },
                NA)
})
