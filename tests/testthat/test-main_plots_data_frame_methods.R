################################################################################
###
### DATE CREATED: 2026-04-03
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Test the main plotting functions with their default arguments,
### data frame methods.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

## Country code to use in the tests
k <- "716"

###-----------------------------------------------------------------------------
### * Data Frame Methods

###-----------------------------------------------------------------------------
### ** plot_tfr_surfs

test_that("'plot_tfr_surfs.data.frame()' works, no x_alt.", {
    results <-
        expect_error(
        suppressMessages(suppressWarnings(
                    plot_tfr_surfs(test_data_tfrSURFs_list[[k]]))),
            NA)
})


test_that("'plot_tfr_surfs.data.frame()' works, with x_alt.", {
    results <-
        expect_error(
            suppressMessages(suppressWarnings(
                plot_tfr_surfs(test_data_tfrSURFs_list[[k]],
                               x_alt = test_data_tfrSURFs_median_list[[k]],
                               x_alt_label = "Median only"))),
            NA)
})

###-----------------------------------------------------------------------------
### ** plot_surfs_probs

test_that("'plot_surfs_probs.data.frame()' works, no x_alt.", {
    results <-
        expect_error(
                suppressMessages(suppressWarnings(
                    plot_surfs_probs(test_data_tfrSURFs_list[[k]]))),
            NA)
})


test_that("'plot_surfs_probs.data.frame()' works, with x_alt.", {
    results <-
        expect_error(
            suppressMessages(suppressWarnings(
                plot_surfs_probs(test_data_tfrSURFs_list[[k]],
                                 x_alt = test_data_tfrSURFs_median_list[[k]],
                                 x_alt_label = "Median only"))),
            NA)
})
