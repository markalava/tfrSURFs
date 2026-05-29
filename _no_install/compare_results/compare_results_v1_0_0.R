################################################################################
###
### DATE CREATED: 2026-05-28
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Compare current results on full input dataset with results
### generated under version 1.0.0.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(testthat)
library(tfrSURFs)

archived_results_dirname <- "archived_results_v1.0.0"
source(here::here("_no_install", "compare_results", "0_setup.R"))

###-----------------------------------------------------------------------------
### * Tests

###-----------------------------------------------------------------------------
### ** Main SURF Functions

test_that("make_tfr_surfs() replicates archived results.", {

    ## Probabilistic
    expect_identical(make_tfr_surfs_control_output(sim.dir = bayesTFR_output_dir),
                     remove_comment(get(load(archived_surfs_list_rda_filepath))))

    ## Medians
    expect_identical(make_tfr_surfs_control_output(sim.dir = bayesTFR_output_dir,
                                                   median_only = TRUE),
                     remove_comment(get(load(archived_surfs_median_list_rda_filepath))))
    })

###-----------------------------------------------------------------------------
### ** Tabulation Functions

test_that("tabulate_surf_periods(..., table_type = 'concise') replicates archived results.", {

    ## Probabilistic
    load(archived_surfs_list_rda_filepath)
    expect_identical(make_surf_period_control_tbl(tfr_surfs_lst),
                     remove_comment(readRDS(archived_tab_surf_periods_concise_prob_filepath)))

    ## Medians
    load(archived_surfs_median_list_rda_filepath)
    expect_identical(make_surf_period_control_tbl(tfr_surfs_median_lst),
                     remove_comment(readRDS(archived_tab_surf_periods_concise_medians_filepath)))
})


test_that("tabulate_surf_stats(..., stat = 'avg_len') replicates archived results.", {

    ## Probabilistic
    load(archived_surfs_list_rda_filepath)
    expect_identical(make_surf_stat_control_tbl(tfr_surfs_lst, stat = "avg_len"),
                     remove_comment(readRDS(archived_tab_surf_stats_avg_len_prob_filepath)))

    ## Medians
    load(archived_surfs_median_list_rda_filepath)
    expect_identical(make_surf_stat_control_tbl(tfr_surfs_median_lst, stat = "avg_len"),
                     remove_comment(readRDS(archived_tab_surf_stats_avg_len_medians_filepath)))
})


test_that("tabulate_surf_stats(..., stat = 'count') replicates archived results.", {

    ## Probabilistic
    load(archived_surfs_list_rda_filepath)
    expect_identical(make_surf_stat_control_tbl(tfr_surfs_lst, stat = "count"),
                     remove_comment(readRDS(archived_tab_surf_stats_count_prob_filepath)))

    ## Medians
    load(archived_surfs_median_list_rda_filepath)
    expect_identical(make_surf_stat_control_tbl(tfr_surfs_median_lst, stat = "count"),
                     remove_comment(readRDS(archived_tab_surf_stats_count_medians_filepath)))
})
