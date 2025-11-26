################################################################################
###
### DATE CREATED: 2024-09-10
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Check that level condition, esp Phase II, Phase III,
### TFR < 2.1, are properly indicated. Check Malta and Bhutan, for example.
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
### ** Malta

## tfr_mcmc <- bayesTFR::get.tfr.mcmc(sim.dir = bayesTFR_output_dir)
## tfr_malta <-
##     bayesTFR::get.tfr.estimation(mcmc.list = tfr_mcmc, country = 470, probs = 0.5)

## test <- get_phase_starts(country = 470, sim.dir = bayesTFR_output_dir, plot = TRUE)

## surfs_df_malta <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = 470,
##                                       transition_condition_type = "Phase II & never < 2.1")

## plot_surfs_probs(surfs_df_malta)


## ###-----------------------------------------------------------------------------
## ### ** Bhutan

## surfs_df_bhutan <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = 64,
##                                       transition_condition_type = "Phase II & never < 2.1")

## plot_surfs_probs(surfs_df_bhutan)


## ###-----------------------------------------------------------------------------
## ### ** Uganda

## surfs_df_uganda <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = 800,
##                                       transition_condition_type = "Phase II & never < 2.1")

## plot_surfs_probs(surfs_df_uganda)


###-----------------------------------------------------------------------------
### ** Israel

surfs_df_isreal <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = 376,
                                       transition_condition_type = "Phase II & never < 2.1",
                                       min_surf_length = 2)

plot_surfs_probs(surfs_df_isreal)

