################################################################################
###
### DATE CREATED: 2026-05-28
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Create full results to act as controls to use when testing main
### SURF identification function.
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

source(here::here("_no_install", "compare_results", "0_setup.R"))

###-----------------------------------------------------------------------------
### * Generate Main Results

## Report argument values that will be used:
get_arg_defs("make_tfr_surfs")

###-----------------------------------------------------------------------------
### ** Probabilistic

## RUN MAIN FUNCTION
tfr_surfs_lst <-
    add_control_comment(make_tfr_surfs_control_output(sim.dir = bayesTFR_output_dir))

## Save Results
save(tfr_surfs_lst, file = archived_surfs_list_rda_filepath)

###-----------------------------------------------------------------------------
### ** Medians

## RUN MAIN FUNCTION
tfr_surfs_median_lst <-
    add_control_comment(make_tfr_surfs_control_output(sim.dir = bayesTFR_output_dir,
                                                      median_only = TRUE) #<<<<<<< !!!
                        )

## Save Results
save(tfr_surfs_median_lst, file = archived_surfs_median_list_rda_filepath)

