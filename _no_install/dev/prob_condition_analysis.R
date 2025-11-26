################################################################################
###
### DATE CREATED: 2024-09-10
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Probability condition checks.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(bayesTFR)

## library(tfrSURFs)
devtools::load_all()            # Assumes working dir is this file's directory.

### Input Data
bayesTFR_output_dir <-
    file.path(Sys.getenv("MY_LOCAL_MODEL_RUNS_DIR"),
              "bayesTFR_wpp2024/TFR1simWPP2024/TFR1unc/sim20241101")


###-----------------------------------------------------------------------------
### * Checks

###-----------------------------------------------------------------------------
### ** Hungary

## The SURF from 1965--1978(ish) is longer under prob cond 95% than 80%. Why?

## ## debug(make_tfr_surfs)
## surfs_df_hun <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = 348,
##                                  continuation_condition =
##                                      "Regain TFR, probabilistic",
##                                      ## "Regain TFR, median, 2yr",
##                                  rate_prob_threshold = 0.8)

## plot_surfs_probs(surfs_df_hun,
##                   plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Gibraltar

surfs_df_gib <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = 292)

plot_surfs_probs(surfs_df_gib, datestamp = TRUE)
