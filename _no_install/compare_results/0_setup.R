################################################################################
###
### DATE CREATED: 2026-05-28
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Functions for comparing results between revisions.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * File Paths

###-----------------------------------------------------------------------------
### ** Path to bayesTFR trajectories

bayesTFR_output_dir <-
    ## message("Specify directory to bayesTFR trajectories. These can be
    ## generated using the bayesTFR package (see below for code)
## TEMP:
    file.path(Sys.getenv("MY_LOCAL_MODEL_RUNS_DIR"),
              "bayesTFR_wpp2024/TFR1simWPP2024/TFR1unc/sim20241101")

## ## TEMP: TESTING ONLY!!
## source(here::here("inst", "slowTests", "0_setup.R"))
## bayesTFR_output_dir <-
##     setup_bayesTFR_test_data_temp_dir(here::here("data-raw", "bigData",
##                                                  "bayesTFR_short_test.tar.gz"))

###-----------------------------------------------------------------------------
### ** Archived Results

### SURF outputs

archived_surfs_list_rda_filepath <-
    here::here("_no_install", "compare_results", "archived_results", "tfr_surfs_lst.rda")
archived_surfs_median_list_rda_filepath <-
    here::here("_no_install", "compare_results", "archived_results", "tfr_surfs_median_lst.rda")

### Tabulations

archived_tab_surf_periods_concise_prob_filepath <-
    here::here("_no_install", "compare_results", "archived_results",
               "tab_surf_periods_concise_prob.rds")
archived_tab_surf_periods_concise_medians_filepath <-
    here::here("_no_install", "compare_results", "archived_results",
               "tab_surf_periods_concise_medians.rds")

archived_tab_surf_stats_avg_len_prob_filepath <-
    here::here("_no_install", "compare_results", "archived_results",
               "tab_surf_avg_len_prob.rds")
archived_tab_surf_stats_avg_len_medians_filepath <-
    here::here("_no_install", "compare_results", "archived_results",
               "tab_surf_avg_len_medians.rds")

archived_tab_surf_stats_count_prob_filepath <-
    here::here("_no_install", "compare_results", "archived_results",
               "tab_surf_count_prob.rds")
archived_tab_surf_count_medians_filepath <-
    here::here("_no_install", "compare_results", "archived_results",
               "tab_surf_count_medians.rds")

###-----------------------------------------------------------------------------
### * TFR Trajectories

### Generate probabilistic projections of TFR using 'bayesTFR' package using
### this script:
### https://bayespop.csss.washington.edu/data/bayesTFR/TFRsimWPP2024/TFR1unc/README.r
### or download from
### 'https://bayespop.csss.washington.edu/data/bayesTFR/TFR1simWPP2024.tgz' and
### set 'bayesTFR_output_dir' to the location of the results.

###-----------------------------------------------------------------------------
### * Functions

##' Control version of main SURF output list.
##'
##' @param sim.dir
##' @return A list.
##' @author Mark C Wheldon
##'
##' @name control_surf_list
##' @keywords internal
##' @noRd
make_tfr_surfs_control_output <- function(sim.dir, median_only = FALSE) {
    make_tfr_surfs(country_codes = NULL,
                   sim.dir = sim.dir,
                   median_only = median_only,
                   transition_condition_type = c("Phase II & >= 2.1 persistently"),
                   smoothing_method = c("local_linear", "annual_difference"),
                   bandwidth = 3,
                   rate_threshold = -0.01,
                   rate_prob_threshold = 0.8,
                   continuation_condition = c("Regain TFR, median, 2yr"),
                   continuation_condition_prob_threshold = 0.8,
                   exceedance_condition = c("Max TFR > 2.1, median"),
                   exceedance_condition_prob_threshold = 0.8,
                   min_surf_length = 2,
                   min_inter_surf_length = 2,
                   year_lim = c(1950, 2050),
                   incl_small_countries = FALSE,
                   ncores = getOption("cl.cores", -2))
}


##' Control versions of main output tables
##'
##' Create static versions of output tables to test updated code against.
##'
##' @param x
##' @return A data frame
##' @author Mark C Wheldon
##'
##' @name control_tables
##' @keywords internal
##' @noRd
make_surf_period_control_tbl <- function(x) {
    tabulate_surf_periods(x = x,
                          incl_small_countries = FALSE,
                          table_type = "concise", digits = 1)
}


##' @rdname control_tables
##' @keywords internal
##' @noRd
make_surf_stat_control_tbl <- function(x, stat) {
    tabulate_surf_stats(x = x, stat = stat,
                        incl_small_countries = FALSE,
                        filter_zero_rows = FALSE,
                        geographies = c("area_name", "reg_name", "name", "global"),
                        proj_split = "by_year")
}


##' Add informational comment to control outputs
##'
##' @param obj
##' @return obj
##' @author Mark C Wheldon
##'
##' @keywords internal
##' @noRd
add_control_comment <- function(obj) {
    if (requireNamespace("tfrSURFs", quietly = TRUE)) {
        pv <- packageVersion("tfrSURFs")
        pd <- packageDate("tfrSURFs", date.fields = "Built")
    } else {
        pv <- pd <- "['tfrSURFs' Not installed!]"
    }
    comment(obj) <-
        paste0("Created: ", Sys.Date(), ".\n", "Package Version: ", pv, ".\n",
               "Package Build Date: ", pd, ".\n")
    return(obj)
}

##' @rdname add_control_comment
##' @keywords internal
##' @noRd
remove_comment <- function(obj) {
    comment(obj) <- NULL
    return(obj)
}

