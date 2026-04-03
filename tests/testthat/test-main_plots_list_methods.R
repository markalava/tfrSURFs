################################################################################
###
### DATE CREATED: 2026-04-03
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Test the main plotting functions with their default arguments,
### list methods.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

## Country code to use in the tests
k <- "716"

###-----------------------------------------------------------------------------
### ** List Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.list()' works, no x_alt.", {
    results <-
        expect_error(
                suppressMessages(suppressWarnings(
                    plot_tfr_surfs(test_data_tfrSURFs_list))),
            NA)
})


test_that("'plot_tfr_surfs.list()' works, with x_alt.", {
    results <-
        expect_error(
            suppressMessages(suppressWarnings(
                plot_tfr_surfs(test_data_tfrSURFs_list,
                               x_alt = test_data_tfrSURFs_median_list,
                               x_alt_label = "Median only"))),
            NA)
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.list()' works, no x_alt.", {
    results <-
        expect_error(
                suppressMessages(suppressWarnings(
                    plot_surfs_probs(test_data_tfrSURFs_list))),
            NA)
})


test_that("'plot_surfs_probs.list()' works, with x_alt.", {
    results <-
        expect_error(
            suppressMessages(suppressWarnings(
                plot_surfs_probs(test_data_tfrSURFs_list,
                                 x_alt = test_data_tfrSURFs_median_list,
                                 x_alt_label = "Median only"))),
            NA)
})
