################################################################################
###
### DATE CREATED: 2025-09-15
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Probabilistic tfrSURFs
###
### DESCRIPTION: Create main results for manuscript.
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
options(tfrSURFs.message_about_unknown_aes = FALSE)
options(tfrSURFs.verbose = TRUE)

###-----------------------------------------------------------------------------
### * File Paths

###-----------------------------------------------------------------------------
### ** Path to bayesTFR trajectories

bayesTFR_output_dir <-
    ## message("Specify directory to bayesTFR trajectories. These can be generated using the bayesTFR package (https://github.com/PPgp/bayesTFR; see below for code) or downloaded from https://bayespop.csss.washington.edu/data/bayesTFR/TFR1simWPP2024.tgz)")

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

###-----------------------------------------------------------------------------
### * Probabilistic SURFs

###-----------------------------------------------------------------------------
### ** Main Results

## Report argument values that will be used:
get_arg_defs("make_tfr_surfs")

###-----------------------------------------------------------------------------
### *** Probabilistic

## RUN MAIN FUNCTION
tfr_surfs_lst <- make_tfr_surfs(sim.dir = bayesTFR_output_dir)

## Save Results
save(tfr_surfs_lst, file = file.path(dir_list[["rdata_dir"]], "tfr_surfs_lst.rda"))

## Load (if re-analyzing)
load(file.path(dir_list[["rdata_dir"]], "tfr_surfs_lst.rda"))

###-----------------------------------------------------------------------------
### *** Medians

## RUN MAIN FUNCTION
tfr_surfs_median_lst <-
    make_tfr_surfs(sim.dir = bayesTFR_output_dir, median_only = TRUE)

## Save Results
save(tfr_surfs_median_lst,
     file = file.path(dir_list[["rdata_dir"]], "tfr_surfs_median_lst.rda"))

## Load (if re-analyzing)
load(file.path(dir_list[["rdata_dir"]], "tfr_surfs_median_lst.rda"))

###-----------------------------------------------------------------------------
### ** Export 'Database' of SURFs

## This is the main output object. Probabilistic and medians runs in one
## workbook.

data(output_column_definitions)

write.xlsx(list(definitions = output_column_definitions,
                probabilistic = do.call("rbind", tfr_surfs_lst),
                non_probabilistic = do.call("rbind", tfr_surfs_median_lst)),
           file = file.path(dir_list[["tables_dir"]], "Appendix_2_Table_C.xlsx"),
           asTable = TRUE, tableStyle = "TableStyleMedium3")

###-----------------------------------------------------------------------------
### ** Tables

###-----------------------------------------------------------------------------
### *** SURF Periods

### SURFs Only

surfs_tbl <- tabulate_surf_periods(tfr_surfs_lst,
                                 incl_small_countries = FALSE,
                                 table_type = "surfs only")

## write.xlsx(surfs_tbl,
##           file = file.path(dir_list[["tables_dir"]], "surf_periods_surfs_only.xlsx"),
##           asTable = TRUE, tableStyle = "TableStyleMedium2")

### Concise Format

surfs_tbl <- tabulate_surf_periods(tfr_surfs_lst,
                                 incl_small_countries = FALSE,
                                 table_type = "concise", digits = 1)

surfs_tbl_medians <- tabulate_surf_periods(tfr_surfs_median_lst,
                                   incl_small_countries = FALSE,
                                   table_type = "concise", digits = 1)

write.xlsx(list(probabilistic = surfs_tbl,
                non_probabilistic = surfs_tbl_medians),
           file = file.path(dir_list[["tables_dir"]], "Appendix_2_Table_B.xlsx"),
           asTable = TRUE, tableStyle = "TableStyleMedium2")


### Full Detail: SURF Periods Intersected with Schoumaker Stalls

surfs_and_schoumaker_stalls <-
    tabulate_surf_periods(tfr_surfs_lst,
                        incl_small_countries = FALSE,
                        table_type = "detailed")

## write.xlsx(surfs_and_schoumaker_stalls,
##           file = file.path(dir_list[["tables_dir"]], "surf_periods_detailed.xlsx"),
##           asTable = TRUE, tableStyle = "TableStyleMedium2")

### TFRs

summary(subset(do.call("rbind", tfr_surfs_lst),
               sub_saharan_africa & surf_year)$TFR_median)

###-----------------------------------------------------------------------------
### *** Location Counts

###-----------------------------------------------------------------------------
### **** By Country

surf_loc_country <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_loc_by_surf(tfr_surfs_lst, stat = c("surfs", "years"),
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name", "name",
                                                       "sub_saharan_africa"),
                                       proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_loc_by_surf(tfr_surfs_median_lst, stat = c("surfs", "years"),
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name", "name",
                                                       "sub_saharan_africa"),
                                       proj_split = "none")))

## write.xlsx(surf_loc_country,
##            file = file.path(dir_list[["tables_dir"]], "surf_loc_by_country.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")

### How Many Countries With SURFs?

## All countries (Not Small)
(countries_w_surfs <-
    addmargins(xtabs(~ has_surf + SURF_type,
                     data = transform(surf_loc_country, has_surf = surfs > 0)),
               margin = 1))

## Among those with SURFs, how many in SSA?
(ssa_countries_w_surfs <-
    addmargins(xtabs(~ SURF_type + sub_saharan_africa,
                     data = subset(surf_loc_country, surfs > 0)), margin = 2))

(ssa_surfs <-
    addmargins(xtabs(surfs ~ sub_saharan_africa + SURF_type,
                     data =
                         stats::aggregate(surfs ~ sub_saharan_africa + SURF_type,
                                          FUN = "sum",
                                          data = surf_loc_country)), margin = 1))

## SSA countries with and without SURFs
(ssa_surfs_country_tbl <-
    subset(surf_loc_country,
           SURF_type == "probabilistic" & sub_saharan_africa & surfs > 0,
           select = c("reg_name", "name", "surfs", "years")))

dim(ssa_surfs_country_tbl)

(ssa_no_surfs_country_tbl <-
    subset(surf_loc_country,
           SURF_type == "probabilistic" & sub_saharan_africa & surfs < 1,
           select = c("reg_name", "name", "surfs", "years")))

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

## Output
openxlsx::write.xlsx(lapply(setNames(list(countries_w_surfs, ssa_countries_w_surfs, ssa_surfs,
                              ssa_surfs_country_tbl, ssa_no_either_country_tbl),
                              nm = c("countries_w_surfs", "ssa_countries_w_surfs", "ssa_surfs",
                              "ssa_surfs_country_tbl", "ssa_no_either_country_tbl")),
                            FUN = as.data.frame),
                     file = file.path(dir_list[["tables_dir"]], "surf_counts_countries.xlsx"),
                     asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### **** By Subregion

surf_loc_subregion <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_loc_by_surf(tfr_surfs_lst, stat = c("surfs", "years"),
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name"),
                                       proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_loc_by_surf(tfr_surfs_median_lst, stat = c("surfs", "years"),
                                       incl_small_countries = FALSE,
                                       geographies = c("area_name", "reg_name"),
                                       proj_split = "none")))

## write.xlsx(surf_loc_subregion,
##            file = file.path(dir_list[["tables_dir"]], "surf_loc_by_subregion.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")

### How Many Subregions With SURFs?

countries_w_surfs <-
    by(surf_loc_subregion, INDICES = surf_loc_subregion["SURF_type"],
       FUN = function(z) {
           z$has_surf <- "No SURFs"
           z[z["surfs"] > 0, "has_surf"] <- "SURFs"
           addmargins(table(z["has_surf"]))
       })

###-----------------------------------------------------------------------------
### *** SURF Counts

###-----------------------------------------------------------------------------
### **** By Country

surf_count_country <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                        geographies = c("area_name", "reg_name", "name"),
                                        proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "name"),
                                        proj_split = "none")))

## write.xlsx(surf_count_country,
##           file = file.path(dir_list[["tables_dir"]], "surf_count_by_country.xlsx"),
##           asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### **** By Subregion

surf_count_subregion <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                        proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                        proj_split = "none")))

## write.xlsx(surf_count_subregion,
##           file = file.path(dir_list[["tables_dir"]], "surf_count_by_subregion.xlsx"),
##           asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### **** Sub-Saharan Africa

surf_count_sub_sah_afr <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("sub_saharan_africa", "global"),
                                        proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("sub_saharan_africa", "global"),
                                        proj_split = "none")))

## write.xlsx(surf_count_sub_sah_afr,
##           file = file.path(dir_list[["tables_dir"]], "surf_count_by_sub_sah_afr.xlsx"),
##           asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### **** Region

surf_count_region <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                        proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "count",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                        proj_split = "none")))

## write.xlsx(surf_count_region,
##            file = file.path(dir_list[["tables_dir"]], "surf_count_by_region.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### *** SURF Lengths

###-----------------------------------------------------------------------------
### **** By Country

surf_len_country <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "name", "global"),
                                       proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "name", "global"),
                                       proj_split = "none")))

## write.xlsx(surf_len_country,
##            file = file.path(dir_list[["tables_dir"]], "surf_lengths_by_country.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### **** By Subregion

surf_len_subregion <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                         proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "reg_name", "global"),
                                         proj_split = "none")))

## write.xlsx(surf_len_subregion,
##            file = file.path(dir_list[["tables_dir"]], "surf_lengths_by_subregion.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")


### Using 'by_year'

surf_len_subregion_by_year <-
    tabulate_surf_stats(tfr_surfs_lst, stat = "avg_len",
                    incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                    geographies = c("area_name", "reg_name", "global"),
                    proj_split = "by_year")

###-----------------------------------------------------------------------------
### **** Sub-Saharan Africa

surf_len_sub_sah_afr <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("sub_saharan_africa", "global"),
                                         proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("sub_saharan_africa", "global"),
                                         proj_split = "none")))

## write.xlsx(surf_len_sub_sah_afr,
##            file = file.path(dir_list[["tables_dir"]], "surf_lengths_by_sub_sah_afr.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### **** By Region

surf_len_region <-
    rbind(
        data.frame(SURF_type = "probabilistic",
                   tabulate_surf_stats(tfr_surfs_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "global"),
                                         proj_split = "none")),
        data.frame(SURF_type = "medians_only",
                   tabulate_surf_stats(tfr_surfs_median_lst, stat = "avg_len",
                                       incl_small_countries = FALSE,
                                       filter_zero_rows = FALSE,
                                       geographies = c("area_name", "global"),
                                         proj_split = "none")))

## write.xlsx(surf_len_region,
##            file = file.path(dir_list[["tables_dir"]], "surf_lengths_by_region.xlsx"),
##            asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### *** Tables in Manuscript

###-----------------------------------------------------------------------------
### **** SURF Statistics

surf_stats_subreg <-
    base::merge(surf_count_subregion, surf_len_subregion, sort = FALSE)

cols_sub <- c("SURF_type",
              "area_name", "reg_name", "count_estimates", "avg_len_estimates",
              "count_projections", "avg_len_projections",
              "count", "avg_len")
surf_stats_subreg <-
    surf_stats_subreg[, cols_sub[cols_sub %in% colnames(surf_stats_subreg)]]

## surf_stats_subreg <-
##     rbind(subset(surf_stats_subreg, area_name != "GLOBAL"),
##           subset(surf_stats_subreg, area_name == "GLOBAL"))

## format_cols <- grep("avg_len", colnames(surf_stats_subreg))
## for (j in format_cols) {
##     surf_stats_subreg[, j] <- formatC(surf_stats_subreg[, j], format = "f", digits = 1)
## }

cols_sub <- c("area_name", "reg_name", "count_estimates",
              "avg_len_estimates",
              "count_projections", "avg_len_projections",
              "count", "avg_len")
surf_stats_subreg <-
    gdata::rename.vars(surf_stats_subreg,
                       from = cols_sub[cols_sub %in% colnames(surf_stats_subreg)],
                       to = c("Region", "Subregion", "Count (estimates)",
                              "Avg. Length (estimates)",
                              "Count (projections)", "Avg. Length (projections)",
                              "Count (total)",
                              "Avg. Length (total)")[cols_sub %in% colnames(surf_stats_subreg)],
                       info = FALSE)

write.xlsx(list(probabilistic = subset(surf_stats_subreg, SURF_type == "probabilistic",
                                       select = -SURF_type),
                non_probabilistic = subset(surf_stats_subreg, SURF_type == "medians_only",
                                      select = -SURF_type)),
           file = file.path(dir_list[["tables_dir"]], "Appendix_2_Table_A.xlsx"),
           asTable = TRUE, tableStyle = "TableStyleMedium2",
           keepNA = TRUE, na.string = "-")

###-----------------------------------------------------------------------------
### **** SURF Periods

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
                            "surf_periods_concise_prob-for-word.xlsx"),
    asTable = TRUE, tableStyle = "TableStyleMedium2")

### Medians

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
                            "surf_periods_concise_medians-for-word.xlsx"),
    asTable = TRUE, tableStyle = "TableStyleMedium2")

###-----------------------------------------------------------------------------
### ** Plots

## ###-----------------------------------------------------------------------------
## ### *** Frequency Plots

## plot_df <- do.call("rbind", tfr_surfs_lst)

## ggplot(data = subset(plot_df, surf_year_start),
##        aes(x = year, fill = area_name)) +
##     geom_histogram() +
##     facet_wrap(~ reg_name)

###-----------------------------------------------------------------------------
### *** Line Plots

### PDFs
pdf(file = file.path(dir_list[["pdf_plots_dir"]], "probabilistic_surfs.pdf"),
    height = 6, width = 14)
plot_surfs_probs(tfr_surfs_lst, x_alt = tfr_surfs_median_lst,
                  incl_small_countries = FALSE, plot = TRUE,
                  add_est_proj_ref_line = TRUE,
                  datestamp = TRUE)
dev.off()

### SVGs
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


