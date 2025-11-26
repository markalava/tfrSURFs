################################################################################
###
### DATE CREATED: 2025-10-21
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Develop plots
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

###-----------------------------------------------------------------------------
### * File Paths

###-----------------------------------------------------------------------------
### ** Local

test_results_dir <- here::here("_no_install", "dev", "test_results")

###-----------------------------------------------------------------------------
### ** Path to bayesTFR trajectories

bayesTFR_output_dir <-
    file.path(Sys.getenv("MY_LOCAL_MODEL_RUNS_DIR"),
              "bayesTFR_wpp2024/TFR1simWPP2024/TFR1unc/sim20241101")

###-----------------------------------------------------------------------------
### * Probabilistic Stalls

###-----------------------------------------------------------------------------
### ** Main Results

args_list <- list(sim.dir = bayesTFR_output_dir## ,
                  ## median_only = FALSE,
                  ## transition_condition_type = "Phase II & >= 2.1 persistently",
                  ## smoothing_method = "local_linear",
                  ## bandwidth = 3,
                  ## rate_threshold = -0.01,
                  ## rate_prob_threshold = 0.8,
                  ## continuation_condition = "Regain TFR, probabilistic",
                  ## continuation_condition_prob_threshold = 0.8,
                  ## exceedance_condition = "Max TFR > 2.1, probabilistic",
                  ## exceedance_condition_prob_threshold = 0.8,
                  ## min_surf_length = 2,
                  ## min_inter_surf_length = 2,
                  ## year_lim = c(1950, 2100),
                  ## incl_small_countries = TRUE,
                  ## ncores = -2
                  )

## tfr_surfs_lst <-
##     do.call("make_tfr_surfs",
##             args = c(## list(country_codes = c(716, 496, 250, 192, 508, 270, 480)),
##                 args_list))
## save(tfr_surfs_lst, file = file.path(test_results_dir, "tfr_surfs_lst.rda"))
## load(file.path(test_results_dir, "tfr_surfs_lst.rda"))

## tfr_surfs_median_lst <-
##     do.call("make_tfr_surfs",
##             args = c(## list(country_codes = c(716, 496, 250, 192, 508, 270, 480)),
##                 modifyList(args_list, list(median_only = TRUE))))
## save(tfr_surfs_median_lst,
##      file = file.path(test_results_dir, "tfr_surfs_median_lst.rda"))
## load(file.path(test_results_dir, "tfr_surfs_median_lst.rda"))

###-----------------------------------------------------------------------------
### * Checks

###-----------------------------------------------------------------------------
### ** South Africa

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("South Africa"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Ghana

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Ghana"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Mozambique

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Mozambique"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Kenya

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Kenya"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Zimbabwe

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Zimbabwe"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Gambia

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Gambia"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** Tanzania

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("United Republic of Tanzania"))

plot_surfs_probs(surfs_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

### -----------------------------------------------------------------------------
### ** Kazakhstan

## debug(make_tfr_surfs)
surfs_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                             country_code = get_country_codes("Kazakhstan"))

surfs_median_df <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                    country_code = get_country_codes("Kazakhstan"),
                                    median_only = TRUE)

tabulate_surf_periods(surfs_df, table_type = "concise")

plot_surfs_probs(x = surfs_df, x_alt = surfs_median_df,
                  plot_ann = "TEST annotation", datestamp = TRUE)

###-----------------------------------------------------------------------------
### ** ALL

pdf(file = file.path("probabilistic_surfs.pdf"), height = 6, width = 14)
for (n in names(tfr_surfs_lst)) {
    message(paste0("(", n, ") ", get_country_names(n)))
    plot_surfs_probs(tfr_surfs_lst[[n]], x_alt = tfr_surfs_median_lst[[n]],
                      plot_ann = "TEST annotation", datestamp = TRUE)
}
dev.off()
