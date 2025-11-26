################################################################################
###
### DATE CREATED: 2025-10-09
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Min interstall length condition checks.
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
### ** Reunion

## debug(make_tfr_surfs)
surfs_df_RE <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                 country_code = get_country_codes("Reunion"),
                                min_inter_surf_length = 2)

plot_surfs_probs(surfs_df_RE,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Thailand (Has no tfrSURFs)

## debug(make_tfr_surfs)
surfs_df_TH <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                country_code = get_country_codes("Thailand"),
                                min_inter_surf_length = 1)

plot_surfs_probs(surfs_df_TH,
                  plot_ann = "TEST annotation", datestamp = TRUE)
