################################################################################
###
### DATE CREATED: 2025-10-13
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: 'median_only' debugging.
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
### ** Egypt

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Egypt"),
                             median_only = TRUE)

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)
