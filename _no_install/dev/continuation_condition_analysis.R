################################################################################
###
### DATE CREATED: 2025-10-09
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Continuation condition checks.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(bayesTFR)

## library(tfrSURFs)
devtools::load_all()            # Assumes working dir is this file's directory.
options(tfrSURFs.verbose = TRUE)

### Input Data
bayesTFR_output_dir <-
    file.path(Sys.getenv("MY_LOCAL_MODEL_RUNS_DIR"),
              "bayesTFR_wpp2024/TFR1simWPP2024/TFR1unc/sim20241101")


###-----------------------------------------------------------------------------
### * Checks

## ###-----------------------------------------------------------------------------
## ### ** One by one

## for (cc in get_country_codes()[-(1:54)]) {
##     if (!identical(as.numeric(cc), as.numeric(get_country_codes("Holy See")))) {
##         message("\nCountry: '", get_country_names(cc), " (", cc, ")'.")
##         test <- make_tfr_surfs(sim.dir = bayesTFR_output_dir, country_code = cc,
##                                 incl_small_countries = TRUE)
##     }
## }

## test <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
##                         country_code = c(654, 398),
##                         incl_small_countries = TRUE)


###-----------------------------------------------------------------------------
### ** Zimbabwe

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Zimbabwe"),
                             continuation_condition = "NONE")

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Kazakhstan

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                country_code = get_country_codes("Kazakhstan"),
                                bandwidth = 3)

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** South Africa

## debug(make_tfr_surfs)
surfs_df_SA <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                country_code = get_country_codes("South Africa"),
                                bandwidth = 3)

plot_surfs_probs(surfs_df_SA,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Sri Lanka

## debug(make_tfr_surfs)
surfs_df_SL <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                 country_code = get_country_codes("Sri Lanka"),
                                continuation_condition = "NONE",
                                bandwidth = 3,
                                exceedance_condition = "NONE",
                                min_surf_length = 1)

plot_surfs_probs(surfs_df_SL,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** USA

## debug(make_tfr_surfs)
surfs_df_usa <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                 country_code = get_country_codes("United States of America"),
                                 continuation_condition = "Regain TFR, probabilistic",
                                 bandwidth = 3)

plot_surfs_probs(surfs_df_usa,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Timor-Leste

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                 country_code = get_country_codes("Timor-Leste"),
                                 continuation_condition = "Regain TFR, probabilistic",
                                 bandwidth = 5) #<<

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)
