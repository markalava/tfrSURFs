################################################################################
###
### DATE CREATED: 2026-05-28
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Compare current results on full input dataset with results
### generated under a previous version.
###
###-----------------------------------------------------------------------------
###
### INSTRUCTIONS: Run this file via '_run_comparison_tests.R' otherwise
### execution will stop after the first failed test.
###
################################################################################

###-----------------------------------------------------------------------------
### * Tests

## ###-----------------------------------------------------------------------------
## ### ** Main SURF Functions

## test_that("make_tfr_surfs() replicates archived results.", {

##     ## Probabilistic
##     expect_identical(make_tfr_surfs_control_output(sim.dir = bayesTFR_output_dir),
##                      remove_attr(read_control_file(archived_surfs_list_rda_filepath)))

##     ## Medians
##     expect_identical(make_tfr_surfs_control_output(sim.dir = bayesTFR_output_dir,
##                                                    median_only = TRUE),
##                      remove_attr(read_control_file(archived_surfs_median_list_rda_filepath)))
##     })

###-----------------------------------------------------------------------------
### ** Tabulation Functions

test_that("tabulate_surf_periods(..., table_type = 'concise') replicates archived results.", {

    ## Probabilistic
    expect_identical(remove_attr(make_surf_period_control_tbl(
        read_control_file(archived_surfs_list_rda_filepath))),
        remove_attr(read_control_file(archived_tab_surf_periods_concise_prob_filepath)))

    ## Medians
    expect_identical(remove_attr(make_surf_period_control_tbl(
        read_control_file(archived_surfs_median_list_rda_filepath))),
        remove_attr(read_control_file(archived_tab_surf_periods_concise_medians_filepath)))
})


test_that("tabulate_surf_stats(..., stat = 'avg_len') replicates archived results.", {

    ## Probabilistic
    expect_identical(remove_attr(make_surf_stat_control_tbl(
        read_control_file(archived_surfs_list_rda_filepath), stat = "avg_len")),
        remove_attr(read_control_file(archived_tab_surf_stats_avg_len_prob_filepath)))

    ## Medians
    expect_identical(remove_attr(make_surf_stat_control_tbl(
        read_control_file(archived_surfs_median_list_rda_filepath), stat = "avg_len")),
        remove_attr(read_control_file(archived_tab_surf_stats_avg_len_medians_filepath)))
})


test_that("tabulate_surf_stats(..., stat = 'count') replicates archived results.", {

    ## Probabilistic
    expect_identical(remove_attr(make_surf_stat_control_tbl(
        read_control_file(archived_surfs_list_rda_filepath), stat = "count")),
        remove_attr(read_control_file(archived_tab_surf_stats_count_prob_filepath)))

    ## Medians
    expect_identical(remove_attr(make_surf_stat_control_tbl(
        read_control_file(archived_surfs_median_list_rda_filepath), stat = "count")),
        remove_attr(read_control_file(archived_tab_surf_stats_count_medians_filepath)))
})
