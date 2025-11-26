################################################################################
###
### DATE CREATED: 2025-09-09
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Probabilistic TFR SURFs
###
### DESCRIPTION: Sensitivity analysis of different settings and thresholds.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

requireNamespace("openxlsx", quietly = TRUE)

library(tfrSURFs)
## devtools::load_all(file.path(Sys.getenv("MY_REPOS_DIR"), "markalava", "tfrSURFs"))

options("tfrSURFs.sensitivity_analysis_overwrite" = FALSE)
options("tfrSURFs.verbose" = TRUE)

options("openxlsx.dateFormat" = "yyyy-mmm-dd")

###-----------------------------------------------------------------------------
### ** File Paths

###-----------------------------------------------------------------------------
### *** bayesTFR trajectories
bayesTFR_output_dir <-
    ## message("Specify directory to bayesTFR trajectories. These can be generated using the bayesTFR package (https://github.com/PPgp/bayesTFR; see below for code) or downloaded from https://bayespop.csss.washington.edu/data/bayesTFR/TFR1simWPP2024.tgz)")
    ## ## TEMP:
    file.path(Sys.getenv("MY_LOCAL_MODEL_RUNS_DIR"),
              "bayesTFR_wpp2024/TFR1simWPP2024/TFR1unc/sim20241101")


## ## TEMP: TESTING ONLY!!
## source(file.path(Sys.getenv("MY_REPOS_DIR"), "markalava", "tfrSURFs", "inst", "slowTests", "0_setup.R"))
## bayesTFR_output_dir <-
##     setup_bayesTFR_test_data_temp_dir(here::here("data-raw", "bigData",
##                                                  "bayesTFR_short_test.tar.gz"))

options("tfrSURFs.sim.dir" = bayesTFR_output_dir)

###-----------------------------------------------------------------------------
### *** Output

output_dir_name <- "sensitivity_analysis_outputs"

###-----------------------------------------------------------------------------
### * TFR Trajectories

### Generate probabilistic projections of TFR using 'bayesTFR' package, or
### download from
### 'https://bayespop.csss.washington.edu/data/bayesTFR/TFR1simWPP2024.tgz' and
### set 'bayesTFR_output_dir' to the location of the results.

###-----------------------------------------------------------------------------
### * Alternate SURF Definitions

### ...........................................................................>
### Lists that define the variants ............................................>
###

## ## TESTING ONLY -------
## make_tfr_surfs_arg_list_OLD <-  make_tfr_surfs_arg_list
## make_tfr_surfs_arg_list <- function(...) {
##     TFRSurfs:::validate_tfr_surfs_args_list(
##         modifyList(make_tfr_surfs_arg_list_OLD(...),
##                    list(make_tfr_surfs_args =
##                             list(country_codes = c(108, 716, 496, 250, 192)))))
## }
## ## --------------------

alt_surfs_list <- list(
    make_tfr_surfs_arg_list(id = "default", sim.dir = bayesTFR_output_dir),
    make_tfr_surfs_arg_list(id = "trans_cond_pII_nvr_lt_2.1",
                             desc = "Trans cond: Phase II & never < 2.1",
                             alt_args = list(transition_condition_type = "Phase II & never < 2.1")),
    make_tfr_surfs_arg_list(id = "trans_cond_pII_only",
                            desc = "Trans cond: Phase II only",
                            alt_args = list(transition_condition_type = "Phase II only")),
    make_tfr_surfs_arg_list(id = "smoothing_ad",
                            desc = "Smoothing condition = annual difference",
                            alt_args = list(smoothing_method = "annual_difference")),
    make_tfr_surfs_arg_list(id = "bandwidth_5",
                            desc = "Bandwidth = 5",
                            alt_args = list(bandwidth = 5)),
    make_tfr_surfs_arg_list(id = "rate_thold_0",
                            desc = "Rate condition threshold = 0",
                            alt_args = list(rate_threshold = 0)),
    make_tfr_surfs_arg_list(id = "rate_thold_m005",
                            desc = "Rate condition threshold = -0.05",
                            alt_args = list(transition_condition_type = "Phase II only")),
    make_tfr_surfs_arg_list(id = "rate_prob_thold_050",
                            desc = "Probability condition threshold = 50%",
                            alt_args = list(rate_prob_threshold = 0.5)),
    make_tfr_surfs_arg_list(id = "rate_prob_thold_090",
                            desc = "Probability condition threshold = 90%",
                            alt_args = list(rate_prob_threshold = 0.9)),
    make_tfr_surfs_arg_list(id = "rate_prob_thold_095",
                            desc = "Probability condition threshold = 95%",
                            alt_args = list(rate_prob_threshold = 0.95)),
    make_tfr_surfs_arg_list(id = "cont_condition_none",
                            desc = "Continuation condition = 'NONE'",
                            alt_args = list(continuation_condition = "NONE")),
    make_tfr_surfs_arg_list(id = "cont_condition_thold_080",
                             desc = "Continuation condition prob. threshold = 80%",
                             alt_args = list(continuation_condition = "Regain TFR, probabilistic",
                                             continuation_condition_prob_threshold = 0.8)),
    make_tfr_surfs_arg_list(id = "cont_condition_thold_050",
                            desc = "Continuation condition prob. threshold = 50%",
                            alt_args = list(continuation_condition = "Regain TFR, probabilistic",
                                            continuation_condition_prob_threshold = 0.5)),
    make_tfr_surfs_arg_list(id = "exc_condition_none",
                            desc = "Exceedance condition = 'NONE'",
                            alt_args = list(exceedance_condition = "NONE")),
    make_tfr_surfs_arg_list(id = "exc_condition_thold_080",
                             desc = "Exceedance condition prob. threshold = 80%",
                             alt_args = list(exceedance_condition_prob_threshold = 0.8,
                                             exceedance_condition = "Max TFR > 2.1, probabilistic")),
    make_tfr_surfs_arg_list(id = "exc_condition_thold_050",
                             desc = "Exceedance condition prob. threshold = 50%",
                             alt_args = list(exceedance_condition_prob_threshold = 0.5,
                                             exceedance_condition = "Max TFR > 2.1, probabilistic")),
    make_tfr_surfs_arg_list(id = "min_surf_len_1",
                            desc = "Min. SURF length = 1",
                            alt_args = list(min_surf_length = 1)),
    make_tfr_surfs_arg_list(id = "min_inter_surf_len_1",
                            desc = "Min. inter-SURF length = 1",
                            alt_args = list(min_inter_surf_length = 1)),
    make_tfr_surfs_arg_list(id = "min_b_len_1_c_ex_none",
                             desc = "Cont. and exc. conditions = 'NONE', min. both lengths = 1",
                             alt_args = list(continuation_condition = "NONE",
                                             exceedance_condition = "NONE",
                                             min_surf_length = 1,
                                             min_inter_surf_length = 1)),
    make_tfr_surfs_arg_list(id = "min_int_len_1_c_ex_none",
                             desc = "Cont. and exc. conditions = 'NONE', min. SURF length = 1",
                             alt_args = list(continuation_condition = "NONE",
                                             exceedance_condition = "NONE",
                                             min_surf_length = 1,
                                             min_inter_surf_length = 2)),
    make_tfr_surfs_arg_list(id = "cont_exc_none",
                             desc = "Cont. and exc. conditins = 'NONE'",
                             alt_args = list(continuation_condition = "NONE",
                                             exceedance_condition = "NONE")))

## Exclude small countries
alt_surfs_list <- lapply(alt_surfs_list, function(z) {
    z[["make_tfr_surfs_args"]] <-
        modifyList(z[["make_tfr_surfs_args"]],
                   val = list(incl_small_countries = FALSE))
    return(z)
    })


## ## TEMP: Just some countries for testing!
## ##
## test_country_c <- validate_country_codes(c(12, 716, 508), sim.dir = bayesTFR_output_dir)

## alt_surfs_list <- lapply(alt_surfs_list, function(z) {
##     z[["make_tfr_surfs_args"]] <-
##         modifyList(z[["make_tfr_surfs_args"]],
##                    val = list(country_codes = test_country_c))
##     return(z)
##     })

###
### <...........................................................................

###-----------------------------------------------------------------------------
### ** Create Outputs

tfr_surf_df_list <- make_alt_surfs(alt_surfs_list, median_only = FALSE)
tfr_surf_df_list_median <- make_alt_surfs(alt_surfs_list, median_only = TRUE)

###-----------------------------------------------------------------------------
### * Tabulate SURFs

###-----------------------------------------------------------------------------
### ** SURF Stats

###-----------------------------------------------------------------------------
### *** Counts

### Individual Tables

make_alt_surfs_stats_tables(alt_surfs_list, median_only = FALSE)
make_alt_surfs_stats_tables(alt_surfs_list, median_only = TRUE)

### Comparison Tables

make_alt_surfs_variant_comparison_table(alt_surfs_list)
make_alt_surfs_variant_comparison_table(alt_surfs_list, median_only = TRUE)

###-----------------------------------------------------------------------------
### *** Lengths

### Individual Tables

make_alt_surfs_stats_tables(alt_surfs_list, median_only = FALSE, stat = "avg_len")
make_alt_surfs_stats_tables(alt_surfs_list, median_only = TRUE, stat = "avg_len")

### Comparison Tables

make_alt_surfs_variant_comparison_table(alt_surfs_list, stat = "avg_len")
make_alt_surfs_variant_comparison_table(alt_surfs_list, stat = "avg_len",
                                        median_only = TRUE)

###-----------------------------------------------------------------------------
### ** SURF Periods

make_alt_surfs_periods_tables(alt_surfs_list, median_only = FALSE)
make_alt_surfs_periods_tables(alt_surfs_list, median_only = TRUE)

###-----------------------------------------------------------------------------
### * Plot SURFs

make_alt_surfs_plots(alt_surfs_list, file_type = "pdf")
## make_alt_surfs_plots(alt_surfs_list, file_type = "svg")

