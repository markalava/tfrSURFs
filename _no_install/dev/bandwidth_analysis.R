################################################################################
###
### DATE CREATED: 2025-09-17
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Bandwidth checks.
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
### ** Timor-Leste

## Under bw 5, the SURF staring in 1980 is not continued. Why?

surfs_df_TL <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                country_code = get_country_codes("Timor-Leste"),
                                continuation_condition = "Regain TFR, probabilistic",
                                bandwidth = 5)

plot_surfs_probs(surfs_df_TL,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Sri Lanka

## Bandwidth of 5 has a SURF 1977-1979 but bandwidth three does not.

## debug(make_tfr_surfs)
surfs_df_SL <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                 country_code = get_country_codes("Sri Lanka"),
                                continuation_condition = "Regain TFR, probabilistic",
                                 bandwidth = 3)

plot_surfs_probs(surfs_df_SL,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** USA

### TODO: CHECK USA! Why is 1969---1971 a SURF? It looks offset by at least plus one
### or two years.

## debug(make_tfr_surfs)
surfs_df_usa <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                 country_code = get_country_codes("United States of America"),
                                 continuation_condition = "Regain TFR, probabilistic",
                                 bandwidth = 3)

plot_surfs_probs(surfs_df_usa,
                  plot_ann = "TEST annotation", datestamp = TRUE)
