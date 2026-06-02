################################################################################
###
### DATE CREATED: 2026-05-28
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Create control tables to use when testing main tabulation
### functions.
###
###-----------------------------------------------------------------------------
###
### INSTRUCTIONS: Only run this script manually and when necesssary. The control
### tables are not expected to change frequently.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set up

library(tfrSURFs)
stopifnot(identical(packageVersion("tfrSURFs"), package_version("1.0.0")))

archived_results_dirname <- "archived_results_v1.0.0"
source(here::here("_no_install", "compare_results", "0_setup.R"))

###-----------------------------------------------------------------------------
### * Generate Main Results

## Report argument values that will be used:
get_arg_defs("make_tfr_surfs")

###-----------------------------------------------------------------------------
### ** Probabilistic

load(archived_surfs_list_rda_filepath)
comment(tfr_surfs_lst)

###-----------------------------------------------------------------------------
### ** Medians

load(archived_surfs_median_list_rda_filepath)
comment(tfr_surfs_median_lst)

###-----------------------------------------------------------------------------
### * Make Control Tables

###-----------------------------------------------------------------------------
### ** SURF Periods

###-----------------------------------------------------------------------------
### *** Concise Format

surf_periods_tbl <- add_control_comment(make_surf_period_control_tbl(tfr_surfs_lst))
saveRDS(surf_periods_tbl, file = archived_tab_surf_periods_concise_prob_filepath)

surf_periods_tbl_medians <- add_control_comment(make_surf_period_control_tbl(tfr_surfs_median_lst))
saveRDS(surf_periods_tbl_medians, file = archived_tab_surf_periods_concise_medians_filepath)

###-----------------------------------------------------------------------------
### ** SURF Statistics

###-----------------------------------------------------------------------------
### *** Average Lengths

surf_avg_len_country_tbl <- add_control_comment(make_surf_stat_control_tbl(tfr_surfs_lst, stat = "avg_len"))
saveRDS(surf_avg_len_country_tbl, file = archived_tab_surf_stats_avg_len_prob_filepath)

surf_avg_len_country_tbl_medians <- add_control_comment(make_surf_stat_control_tbl(tfr_surfs_median_lst, stat = "avg_len"))
saveRDS(surf_avg_len_country_tbl_medians, file =  archived_tab_surf_stats_avg_len_medians_filepath)

###-----------------------------------------------------------------------------
### *** Counts

## NB: These were created with the argument `proj_split = "none"` because
## proj_split = "by_year" was not possible with `stat = "count"` in v1.0.0.

surf_count_country_tbl <- add_control_comment(make_surf_stat_control_tbl(tfr_surfs_lst, stat = "count"))
saveRDS(surf_count_country_tbl, file = archived_tab_surf_stats_count_prob_filepath)

surf_count_country_tbl_medians <- add_control_comment(make_surf_stat_control_tbl(tfr_surfs_median_lst, stat = "count"))
saveRDS(surf_count_country_tbl_medians, file =  archived_tab_surf_stats_count_medians_filepath)
