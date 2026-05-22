################################################################################
###
### DATE CREATED: 2026-04-02
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Probabilistic TFR Stalls
###
### DESCRIPTION: Compare the impact of different conditions on SURFs for
### manuscript Discussion.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

requireNamespace("openxlsx", quietly = TRUE)

library(tfrSURFs)
## devtools::load_all(file.path(Sys.getenv("MY_REPOS_DIR"), "markalava", "tfrSURFs"))

options("tfrSURFs.show_ggplot_warning_note" = FALSE)
options("tfrSURFs.sensitivity_analysis_output_dir_name" = "sensitivity_analysis")
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
    ## bandwidth
    make_tfr_surfs_arg_list(id = "bandwidth_5",
                            desc = "Bandwidth = 5",
                            alt_args = list(bandwidth = 5)),
    make_tfr_surfs_arg_list(id = "bandwidth_7",
                            desc = "Bandwidth = 7",
                            alt_args = list(bandwidth = 7)),
    ## Rate
    make_tfr_surfs_arg_list(id = "rate_thold_0",
                            desc = "Rate condition threshold = 0",
                            alt_args = list(rate_threshold = 0)),
    make_tfr_surfs_arg_list(id = "rate_thold_m002",
                            desc = "Rate condition threshold = -0.02",
                            alt_args = list(rate_threshold = -0.02)),
    make_tfr_surfs_arg_list(id = "rate_thold_m003",
                            desc = "Rate condition threshold = -0.03",
                            alt_args = list(rate_threshold = -0.03)),
    make_tfr_surfs_arg_list(id = "rate_thold_m004",
                            desc = "Rate condition threshold = -0.04",
                            alt_args = list(rate_threshold = -0.04)),
    make_tfr_surfs_arg_list(id = "rate_thold_m005",
                            desc = "Rate condition threshold = -0.05",
                            alt_args = list(rate_threshold = -0.05)),
    ## Probability
    make_tfr_surfs_arg_list(id = "prob_thold_050",
                            desc = "Probability condition threshold = 50%",
                            alt_args = list(rate_prob_threshold = 0.5)),
    make_tfr_surfs_arg_list(id = "prob_thold_060",
                            desc = "Probability condition threshold = 60%",
                            alt_args = list(rate_prob_threshold = 0.6)),
    make_tfr_surfs_arg_list(id = "prob_thold_070",
                            desc = "Probability condition threshold = 70%",
                            alt_args = list(rate_prob_threshold = 0.7)),
    make_tfr_surfs_arg_list(id = "prob_thold_090",
                            desc = "Probability condition threshold = 90%",
                            alt_args = list(rate_prob_threshold = 0.9)),
    make_tfr_surfs_arg_list(id = "prob_thold_095",
                            desc = "Probability condition threshold = 95%",
                            alt_args = list(rate_prob_threshold = 0.95)),
    ## Minimum length
    make_tfr_surfs_arg_list(id = "min_len_1",
                            desc = "Min. SURF length = 1",
                            alt_args = list(min_surf_length = 1)),
    make_tfr_surfs_arg_list(id = "min_len_3",
                            desc = "Min. SURF length = 3",
                            alt_args = list(min_surf_length = 3)),
    make_tfr_surfs_arg_list(id = "min_inter_len_1",
                            desc = "Min. inter-SURF length = 1",
                            alt_args = list(min_inter_surf_length = 1)),
    make_tfr_surfs_arg_list(id = "min_inter_len_3",
                            desc = "Min. inter-SURF length = 3",
                            alt_args = list(min_inter_surf_length = 3)),
    make_tfr_surfs_arg_list(id = "min_len_inter_1",
                            desc = "Min. SURF length = 1 AND Min. inter-SURF length = 1",
                            alt_args = list(min_surf_length = 1,
                                            min_inter_surf_length = 1)),
    make_tfr_surfs_arg_list(id = "min_len_inter_3",
                            desc = "Min. SURF length = 3 AND Min. inter-SURF length = 3",
                            alt_args = list(min_surf_length = 3,
                                            min_inter_surf_length = 3)),
    ## Continuation condition
    make_tfr_surfs_arg_list(id = "cont_cond_none",
                            desc = "Continuation condition = 'NONE'",
                            alt_args = list(continuation_condition = "NONE")),
    ## Exceedance condition
    make_tfr_surfs_arg_list(id = "exc_cond_none",
                            desc = "Exceedance condition = 'NONE'",
                            alt_args = list(exceedance_condition = "NONE")),
    ## Continuation, exceedance, minimum lengths
    make_tfr_surfs_arg_list(id = "cont_exc_none",
                            desc = "Continuation condition = 'NONE' AND Exceedance condition = 'NONE'",
                            alt_args = list(continuation_condition = "NONE",
                                            exceedance_condition = "NONE")),
    ## COMBOs
    make_tfr_surfs_arg_list(id = "only_tran_rate_prob",
                            desc = "Only the transition, rate, and probability conditions",
                            alt_args = list(min_surf_length = 1,
                                            min_inter_surf_length = 1,
                                            continuation_condition = "NONE",
                                            exceedance_condition = "NONE"))
    )

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

make_alt_surfs_variant_comparison_table(alt_surfs_list, median_only = FALSE)
make_alt_surfs_variant_comparison_table(alt_surfs_list, median_only = TRUE)

###-----------------------------------------------------------------------------
### *** Lengths

### Individual Tables

make_alt_surfs_stats_tables(alt_surfs_list, median_only = FALSE, stat = "avg_len")
make_alt_surfs_stats_tables(alt_surfs_list, median_only = TRUE, stat = "avg_len")

### Comparison Tables

make_alt_surfs_variant_comparison_table(alt_surfs_list, stat = "avg_len",
                                        median_only = FALSE)
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

