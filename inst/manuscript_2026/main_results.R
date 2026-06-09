################################################################################
###
### DATE CREATED: 2026-03-26
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Probabilistic TFR Stalls
###
### DESCRIPTION: Create results for main manuscript.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(bayesTFR)
library(ggplot2)
library(here)
library(openxlsx)
options("openxlsx.dateFormat" = "yyyy-mmm-dd")

library(tfrSURFs)
## devtools::load_all()            # Assumes working dir is this file's directory.
packageVersion("tfrSURFs")
options(tfrSURFs.show_ggplot_warning_note = FALSE)
options(tfrSURFs.verbose = TRUE)

###-----------------------------------------------------------------------------
### * File Paths

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
### ** Outputs of this script

dir_list <- list(output_dir = "output")
dir_list <- c(dir_list, list(
                            rdata_dir = file.path(dir_list$output_dir, "rdata"),
                            plots_dir = file.path(dir_list$output_dir, "plots")))
dir_list <- c(dir_list, list(
                            pdf_plots_dir = file.path(dir_list$plots_dir, "pdf"),
                            svg_plots_dir = file.path(dir_list$plots_dir, "svg"),
                            tables_dir = file.path(dir_list$output_dir, "tables")))

for (x in dir_list) {
    if (!dir.exists(x)) dir.create(x, recursive = TRUE)
}

surfs_list_rda_filename <- file.path(dir_list[["rdata_dir"]], "tfr_surfs_lst.rda")
surfs_median_list_rda_filename <-
    file.path(dir_list[["rdata_dir"]], "tfr_surfs_median_lst.rda")

###-----------------------------------------------------------------------------
### * TFR Trajectories

### Generate probabilistic projections of TFR using 'bayesTFR' package using
### this script:
### https://bayespop.csss.washington.edu/data/bayesTFR/TFRsimWPP2024/TFR1unc/README.r
### or download from
### 'https://bayespop.csss.washington.edu/data/bayesTFR/TFR1simWPP2024.tgz' and
### set 'bayesTFR_output_dir' to the location of the results.

###-----------------------------------------------------------------------------
### * Get SURFs Results

## Report argument values that will be used:
get_arg_defs("make_tfr_surfs")

###-----------------------------------------------------------------------------
### *** Probabilistic

if (file.exists(surfs_list_rda_filename)) {
    ## Load (if re-analyzing)
    message("Re-loading '", surfs_list_rda_filename, "'.")
    load(surfs_list_rda_filename)
} else {
    ## RUN MAIN FUNCTION
    tfr_surfs_lst <- make_tfr_surfs(sim.dir = bayesTFR_output_dir)

    ## Save Results
    save(tfr_surfs_lst, file = surfs_list_rda_filename)
}

###-----------------------------------------------------------------------------
### *** Non-Probabilistic

if (file.exists(surfs_median_list_rda_filename)) {
    ## Load (if re-analyzing)
    message("Re-loading '", surfs_median_list_rda_filename, "'.")
    load(surfs_median_list_rda_filename)
} else {
    ## RUN MAIN FUNCTION
    tfr_surfs_median_lst <- make_tfr_surfs(sim.dir = bayesTFR_output_dir,
                                           median_only = TRUE) #<<<<<<< !!!

    ## Save Results
    save(tfr_surfs_median_lst, file = surfs_median_list_rda_filename)
}

###-----------------------------------------------------------------------------
### * Results in Text

###-----------------------------------------------------------------------------
### ** Table 1. Counts and Average Lenghts

### This is the same as Appendix 2, Table A.

###-----------------------------------------------------------------------------
### ** SURF and Location Counts

###-----------------------------------------------------------------------------
### *** By Country

surf_loc_country <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_loc_by_surf(tfr_surfs_lst,
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name", "name",
                                                       "sub_saharan_africa"),
                                       time_range = "estimation")), # << estimation period
        data.frame(SURF_type = "medians_only",
                   tabulate_loc_by_surf(tfr_surfs_median_lst,
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name", "name",
                                                       "sub_saharan_africa"),
                                       time_range = "estimation"))) # << estimation period

### How Many Countries With SURFs?

## All countries (Not Small)
(countries_w_surfs <-
    addmargins(xtabs(~ has_surf + SURF_type,
                     data = transform(surf_loc_country, has_surf = surf_count > 0)),
               margin = 1))

## Among those countries with SURFs, how many are in SSA?
(ssa_countries_w_surfs <-
    addmargins(xtabs(~ SURF_type + sub_saharan_africa,
                     data = subset(surf_loc_country, surf_count > 0)), margin = 2))

## Among all SURFs, how many are in SSA?
(ssa_surfs <-
    addmargins(xtabs(surf_count ~ sub_saharan_africa + SURF_type,
                     data =
                         stats::aggregate(surf_count ~ sub_saharan_africa + SURF_type,
                                          FUN = "sum",
                                          data = surf_loc_country)), margin = 1))

## SSA countries with and without SURFs
(ssa_surf_count_country_tbl <-
    subset(surf_loc_country,
           SURF_type == "probabilistic" & sub_saharan_africa & surf_count > 0,
           select = c("reg_name", "name", "surf_count", "surf_years")))

dim(ssa_surf_count_country_tbl)

(ssa_no_surfs_country_tbl <-
    subset(surf_loc_country,
           SURF_type == "probabilistic" & sub_saharan_africa & surf_count < 1,
           select = c("reg_name", "name", "surf_count", "surf_years")))

dim(ssa_no_surfs_country_tbl)

## Neither a SURF nor a stall
cc_SURF <- unique(subset(do.call("rbind", tfr_surfs_lst),
                         surf_year & sub_saharan_africa)[["country_code"]])
cc_Schoumaker <- unique(subset(do.call("rbind", tfr_surfs_lst),
                         Schoumaker_stall_any & sub_saharan_africa)[["country_code"]])

(ssa_no_either_country_tbl <-
    subset(tfrSURFs::UNlocations_countries,
           sub_saharan_africa & !pop_lt_90k_2024 &
           !(country_code %in% cc_SURF) & !(country_code %in% cc_Schoumaker),
           select = c("reg_name", "country_code", "name")))

dim(ssa_no_either_country_tbl)

###-----------------------------------------------------------------------------
### *** By Subregion

surf_loc_subregion <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_loc_by_surf(tfr_surfs_lst,
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name"),
                                       time_range = "estimation")),
        data.frame(SURF_type = "medians_only",
                   tabulate_loc_by_surf(tfr_surfs_median_lst,
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name"),
                                       time_range = "estimation")))

### How Many Subregions With SURFs?

(subreg_w_surfs <-
    by(surf_loc_subregion, INDICES = surf_loc_subregion["SURF_type"],
       FUN = function(z) {
           z$has_surf <- "No SURFs"
           z[z["surf_count"] > 0, "has_surf"] <- "SURFs"
           addmargins(table(z["has_surf"]))
       }))

###-----------------------------------------------------------------------------
### ** Timing

## When did SURFs start?
summary(subset(do.call("rbind", tfr_surfs_lst), surf_year_start)[["year"]])

###-----------------------------------------------------------------------------
### * Appendix Tables

###-----------------------------------------------------------------------------
### ** SURF Statistics: Appendix 2, Table A

surf_count_subregion <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                       time_range = "estimation")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                       time_range = "estimation")))

surf_len_subregion <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                       time_range = "estimation")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                       time_range = "estimation")))

surf_stats_subreg <-
    base::merge(surf_count_subregion, surf_len_subregion, sort = FALSE)

cols_sub <- c("SURF_type", "area_name", "reg_name", "count", "avg_len")
surf_stats_subreg <-
    surf_stats_subreg[, cols_sub[cols_sub %in% colnames(surf_stats_subreg)]]

first_est_year <- min(tfr_surfs_lst[[1]][["year"]])
last_est_year <- tfr_surfs_lst[[1]][1, "bayesTFR_present_year"]

cols_from <- c("area_name", "reg_name", "count", "avg_len")
cols_to <- c(paste0("Count (", first_est_year, "-", last_est_year, ")"),
             paste0("Avg. Length (", first_est_year, "-", last_est_year, ")"))

surf_stats_subreg <-
    gdata::rename.vars(surf_stats_subreg, from = cols_from, to = c("Region", "Subregion", cols_to),
                       info = FALSE)

write.xlsx(list(
    definitions =
        data.frame(name = cols_to,
                   type = "numeric",
                   description =
                       c("Number of SURFs (estimation period only)",
                         "Average length of SURFs (years; estimation period only)")),
    probabilistic = subset(surf_stats_subreg, SURF_type == "probabilistic",
                           select = -SURF_type),
    non_probabilistic = subset(surf_stats_subreg, SURF_type == "medians_only",
                               select = -SURF_type)),
    file = file.path(dir_list[["tables_dir"]], "Appendix_2_Table_A.xlsx"),
    asTable = TRUE, tableStyle = "TableStyleMedium2",
    keepNA = TRUE, na.string = "-")

###-----------------------------------------------------------------------------
### ** SURF Periods: Appendix 2, Table B

### Concise Format

surfs_tbl <- tabulate_surf_periods(tfr_surfs_lst,
                                 incl_small_countries = FALSE,
                                 table_type = "concise", digits = 1)

surfs_tbl_medians <- tabulate_surf_periods(tfr_surfs_median_lst,
                                   incl_small_countries = FALSE,
                                   table_type = "concise", digits = 1)

write.xlsx(list(
    definitions = data.frame(
        name = colnames(surfs_tbl),
        type = c("character", "character", "logical", "character", "numeric range (as character)",
                 "numeric range (as character)", "character"),
        description =
            c(output_column_definitions[output_column_definitions[["name"]] == "area_name", "description"],
              output_column_definitions[output_column_definitions[["name"]] == "reg_name", "description"],
              output_column_definitions[output_column_definitions[["name"]] == "sub_saharan_africa", "description"],
              output_column_definitions[output_column_definitions[["name"]] == "name", "description"],
              "Year range of SURF",
              "Range of TFR during SURF",
              "Year range of Schoumaker stall with evidence, or note")),
    probabilistic = surfs_tbl,
    non_probabilistic = surfs_tbl_medians),
           file = file.path(dir_list[["tables_dir"]], "Appendix_2_Table_B.xlsx"),
           asTable = TRUE, tableStyle = "TableStyleMedium2")

### TFRs

summary(subset(do.call("rbind", tfr_surfs_lst),
               sub_saharan_africa & surf_year)$TFR_median)

###-----------------------------------------------------------------------------
### *** Formatted for Manuscript

### Probabilistic

surf_periods_tbl_df <-
    gdata::rename.vars(surfs_tbl,
                       from = c("area_name", "reg_name", "name",
                                "sub_saharan_africa", "surf_period",
                                "TFR", "Schoumaker_stall_period"),
                       to = c("Region", "Subregion", "Country", "Sub-Saharan Africa",
                              "SURF Period", "TFR Range",
                              "TFR Stalls (Schoumaker, 2019)"),
                       info = TRUE)

write.xlsx(list(
    Sub_Saharan_Africa =
        blankCells(subset(surf_periods_tbl_df, `Sub-Saharan Africa`, -`Sub-Saharan Africa`),
                   cols = c("Region", "Subregion", "Country")),
    Outside_Sub_Saharan_Africa =
        blankCells(subset(surf_periods_tbl_df, !`Sub-Saharan Africa`,
                          -c(`Sub-Saharan Africa`,
                             `TFR Stalls (Schoumaker, 2019)`)),
                   cols = c("Region", "Subregion", "Country"))),
           file = file.path(dir_list[["tables_dir"]],
                            "Appendix_2_Table_B_probabilistic-for-word.xlsx"),
    asTable = TRUE, tableStyle = "TableStyleMedium2")

### Non-probabilistic

surf_periods_tbl_medians_df <-
    gdata::rename.vars(surfs_tbl_medians,
                       from = c("area_name", "reg_name", "name",
                                "sub_saharan_africa", "surf_period",
                                "TFR", "Schoumaker_stall_period"),
                       to = c("Region", "Subregion", "Country", "Sub-Saharan Africa",
                              "SURF Period", "TFR Range",
                              "TFR Stalls (Schoumaker, 2019)"),
                       info = FALSE)

write.xlsx(list(
    Sub_Saharan_Africa =
        blankCells(subset(surf_periods_tbl_medians_df, `Sub-Saharan Africa`, -`Sub-Saharan Africa`),
                   cols = c("Region", "Subregion", "Country")),
    Outside_Sub_Saharan_Africa =
        blankCells(subset(surf_periods_tbl_medians_df, !`Sub-Saharan Africa`,
                          -c(`Sub-Saharan Africa`,
                             `TFR Stalls (Schoumaker, 2019)`)),
                   cols = c("Region", "Subregion", "Country"))),
           file = file.path(dir_list[["tables_dir"]],
                            "Appendix_2_Table_B_non-probabilistic-for-word.xlsx"),
    asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### ** Export 'Database' of SURFs: Appendix 2, Table C

## This is the main output object. Probabilistic and non-probabilistic runs in one
## workbook.

data(output_column_definitions)

write.xlsx(list(definitions = output_column_definitions,
                probabilistic = do.call("rbind", tfr_surfs_lst),
                non_probabilistic = do.call("rbind", tfr_surfs_median_lst)),
           file = file.path(dir_list[["tables_dir"]], "Appendix_2_Table_C_surfs_database.xlsx"),
           asTable = TRUE, tableStyle = "TableStyleMedium3")

###-----------------------------------------------------------------------------
### * Plots

###-----------------------------------------------------------------------------
### ** Line Plots: Appendix 3, Supplementary Plots

### PDFs
pdf(file = file.path(dir_list[["pdf_plots_dir"]], "Appendix_3_Supp_Plots_probabilistic_surfs.pdf"),
    height = 6, width = 14)
plot_surfs_probs(tfr_surfs_lst, x_alt = tfr_surfs_median_lst,
                  incl_small_countries = FALSE, plot = TRUE,
                  add_est_proj_ref_line = TRUE,
                  datestamp = TRUE)
dev.off()

### SVGs (separate SVG plots by country)
for (cc in remove_small_countries(names(tfr_surfs_lst))) {
    svg(file = file.path(dir_list[["svg_plots_dir"]],
                         paste0("probabilistic_surfs_",
                                tfr_surfs_lst[[cc]][1, "name"],
                                "_", cc, ".svg")),
        height = 6, width = 14)
    print(plot_surfs_probs(x = tfr_surfs_lst[[cc]],
                            x_alt = tfr_surfs_median_lst[[cc]],
                            x_alt_label = "Median only",
                            add_est_proj_ref_line = TRUE))
    dev.off()
}

graphics.off()


