################################################################################
###
### DATE CREATED: 2024-09-13
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs package
###
### DESCRIPTION: Modify format of output tables.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(bayesTFR)
library(ggplot2)
library(here)
## library(tfrSURFs)
devtools::load_all()            # Assumes working dir is this file's directory.
options(tfrSURFs.message_about_unknown_aes = FALSE)

## In source form, this file is here:
## setwd(here::here("_no_install", "dev"))

###-----------------------------------------------------------------------------
### * File Paths

###-----------------------------------------------------------------------------
### ** Local

test_results_dir <- here::here("_no_install", "dev", "test_results")

###-----------------------------------------------------------------------------
### ** Path to bayesTFR trajectories

bayesTFR_output_dir <-
    ## message("Specify directory to bayesTFR trajectories. These can be generated using the bayesTFR package (https://github.com/PPgp/bayesTFR; see below for code) or downloaded from https://bayespop.csss.washington.edu/data/bayesTFR/TFR1simWPP2024.tgz)")
    ## TEMP:
    file.path(Sys.getenv("MY_LOCAL_MODEL_RUNS_DIR"),
              "bayesTFR_wpp2024/TFR1simWPP2024/TFR1unc/sim20241101")

## ## TEMP: TESTING ONLY!!
## source(here::here("inst", "slowTests", "0_setup.R"))
## bayesTFR_output_dir <-
##     setup_bayesTFR_test_data_temp_dir(here::here("data-raw", "bigData",
##                                                  "bayesTFR_short_test.tar.gz"))

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
load(file.path(test_results_dir, "tfr_surfs_lst.rda"))

## tfr_surfs_median_lst <-
##     do.call("make_tfr_surfs",
##             args = c(## list(country_codes = c(716, 496, 250, 192, 508, 270, 480)),
##                 modifyList(args_list, list(median_only = TRUE))))
## save(tfr_surfs_median_lst,
##      file = file.path(test_results_dir, "tfr_surfs_median_lst.rda"))
load(file.path(test_results_dir, "tfr_surfs_median_lst.rda"))

## ###-----------------------------------------------------------------------------
## ### * Test Data

## ## Columns to keep when viewing data frame
## view_cols <-
##     c("year", "surf_year", "Schoumaker_stall_strong", "Schoumaker_stall_moderate",
##       "Schoumaker_stall_weak", "Schoumaker_stall_any",
##       _surf_year_start", _surf_year_group", _surf_year_len")

## view_cols_2 <- c(view_cols, "hash_1", "surf_tbl_block",
## "Schoumaker_stall_year_any", "Schoumaker_stall_year_start", "Schoumaker_stall_year_group",
## "Schoumaker_stall_year_len", "intersecting_Schoumaker_stall_groups")

## ###-----------------------------------------------------------------------------
## ### ** Make data frame

## test_surfs_df <- tfr_surfs_lst[["710"]][1:30,] # 'S Africa'
## test_surfs_df$name <- "TEST name"
## test_surfs_df$area_name <- "TEST area"
## test_surfs_df$reg_name <- "TEST region"
## test_surfs_df$country_code <- 999
## test_surfs_df$transition_condition_met <- TRUE
## test_surfs_df$year_lim_start <- min(test_surfs_df$year)
## test_surfs_df$year_lim_end <- max(test_surfs_df$year)

## ## Initialize all surfs and_surfs to FALSE
## test_surfs_df[, c(_surf_year",
##                    grep("Schoumaker_stall", colnames(test_surfs_df), value = TRUE))] <- FALSE

## ## Row 1 - no_surfs or surfs
## test_surfs_df[1, c(_surf_year",
##                       grep("Schoumaker_stall", colnames(test_surfs_df), value = TRUE))] <- FALSE

## ## Rows 2:3 - only surf
## test_surfs_df[2:3, _surf_year"] <- TRUE
## test_surfs_df[2:3, grep("Schoumaker_stall", colnames(test_surfs_df), value = TRUE)] <- FALSE

## ## Row 4 - break

## ## Rows 5:7 -_surf and surf
## test_surfs_df[5:7, c(_surf_year", "Schoumaker_stall_any")] <- TRUE
## test_surfs_df[5:6, "Schoumaker_stall_weak"] <- TRUE
## test_surfs_df[7, "Schoumaker_stall_moderate"] <- TRUE

## ## Row 8 - break

## ## Rows 9:12 -_surf and surf, but offset
## test_surfs_df[9:11, _surf_year"] <- TRUE
## test_surfs_df[10:12, "Schoumaker_stall_any"] <- TRUE
## test_surfs_df[10:11, "Schoumaker_stall_strong"] <- TRUE
## test_surfs_df[12, "Schoumaker_stall_moderate"] <- TRUE

## ## Row 13 - break

## ## Rows 14:18 -_surf with two surfs inside
## test_surfs_df[14:15, _surf_year"] <- TRUE
## test_surfs_df[17:18, _surf_year"] <- TRUE
## test_surfs_df[14:18, "Schoumaker_stall_any"] <- TRUE
## test_surfs_df[14:16, "Schoumaker_stall_weak"] <- TRUE
## test_surfs_df[17:18, "Schoumaker_stall_moderate"] <- TRUE

## ## Row 19 - break

## ## Rows 20:26 - surf with two_surfs inside
## test_surfs_df[20:26, _surf_year"] <- TRUE
## test_surfs_df[c(21:22, 24:25), "Schoumaker_stall_any"] <- TRUE
## test_surfs_df[21:22, "Schoumaker_stall_weak"] <- TRUE
## test_surfs_df[24:25, "Schoumaker_stall_strong"] <- TRUE

## ## Row 27 - break

## ## Rows 28:29 - only a_surf
## test_surfs_df[28:29, "Schoumaker_stall_any"] <- TRUE
## test_surfs_df[28, "Schoumaker_stall_moderate"] <- TRUE
## test_surfs_df[29, "Schoumaker_stall_strong"] <- TRUE

## ## Rows 29:30 no_surfs or surfs

## ###-----------------------------------------------------------------------------
## ### ** Re-do Stall Lengths

## test_surfs_df <-
##     test_surfs_df[, !colnames(test_surfs_df) %in%
##                      c(_surf_year_start", _surf_year_group", _surf_year_len",
##                        "min_surf_length", "min_inter_surf_length")]

## test_surfs_df <-
##     tfrSURFs:::apply_min_lengths(test_surfs_df, min_surf_length = 1, min_inter_surf_length = 1,
##                       year_lim = c(test_surfs_df$year_lim_start[1], test_surfs_df$year_lim_end[1]))

###-----------------------------------------------------------------------------
### * Make Tables

## surfs_tbl_1 <- tabulate_surf_periods(test_surfs_df, table_type = "tfrSURFs only")
## surfs_tbl_2 <-
##     tabulate_surf_periods(test_surfs_df, table_type = "concise")
## surfs_tbl_3 <- tabulate_surf_periods(test_surfs_df, table_type = "detailed")

## surfs_tbl_1 <- tabulate_surf_periods(tfr_surfs_lst, table_type = "surfs only")

surfs_tbl_2 <- tabulate_surf_periods(tfr_surfs_lst, table_type = "concise",
                                     incl_no_surfs = TRUE,
                                     flag_schoumaker_excl = TRUE)
openxlsx::write.xlsx(surfs_tbl_2, file = "surfs_tbl_2.xlsx", asTable = TRUE)

surfs_median_tbl_2 <- tabulate_surf_periods(tfr_surfs_median_lst, table_type = "concise",
                                     incl_no_surfs = TRUE,
                                     flag_schoumaker_excl = TRUE)
## openxlsx::write.xlsx(surfs_median_tbl_2, file = "surfs_median_tbl_2.xlsx", asTable = TRUE)

## surfs_tbl_3 <- tabulate_surf_periods(tfr_surfs_lst, table_type = "detailed")


