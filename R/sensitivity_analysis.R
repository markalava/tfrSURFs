################################################################################
###
### DATE CREATED: 2025-09-09
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Probabilistic TFR tfrSURFs
###
### DESCRIPTION: Compare the impact of different conditions on tfrSURFs for
### manuscript Discussion.
###
###-----------------------------------------------------------------------------
###
################################################################################

##' Sensitivity analysis helper functions
##'
##' These functions are used in scripts that generate, save, and compare the
##' results for variations of the SURF conditions. To define a SURF variant, use
##' \code{make_tfr_surfs_arg_list}. It returns a list; multiple calls can be
##' combined to create a list of alternative SURF definitions (see
##' \dQuote{Examples}). Each variant is saved to its own subfolder of
##' \code{output_dir_name} to allow for the addition of other variants at a
##' later time.
##'
##' @details
##'
##' \subsection{Functions that create and save outputs}{
##'
##' These functions generate and summarize the results using alternative SURF
##' definitions, and save them in a hierarchical directory structure under
##' \code{file.path(root_dir, output_dir_name)}.
##'
##' The main user function is \code{make_alt_surfs} which takes a list
##' of different argument combinations as input and saves the results in the
##' hierarchy. The input list has one element per alternative definition, each
##' of these being an argument list for \code{link{make_tfr_surfs}}. Note that
##' you do not need to explicitly vary the \code{median_only} argument in your
##' alternative definitions; \code{make_alt_surfs} can take this
##' argument and modify the alternative argument list appropriately.
##'
##' \describe{
##'
##' \item{\code{make_alt_surfs_output_dir_list}}{Make output directories for an
##' alternative SURF definition; returns a list.}
##'
##' \item{\code{make_alt_surfs_filenames}}{Make filenames for an alternative
##' SURF variant; returns a list.}
##'
##' \item{\code{make_alt_surfs}}{Calls \code{\link{make_tfr_surfs}}
##' for each SURF variant; returns a list of the results, named by variant IDs.}
##'
##' \item{\code{make_alt_surfs_pdf_plots}}{Make and save pdf plots of surfs and
##' probabilities; returns the pdf filename as a string.}
##'
##' \item{\code{make_alt_surfs_svg_plots}}{Make and save svg plots of surfs and
##' probabilities; returns the svg filenames as a list.}
##'
##' \item{\code{make_alt_surfs_stats_tables}}{Make and save \file{xlsx} files with
##' tabulations of surfs by calling \code{\link{tabulate_surf_stats}}; returns
##' the filename as a string.}
##'
##' \item{\code{make_alt_surfs_periods_tables}}{Make and save \file{xlsx} files with
##' tabulations of surfs by calling
##' \code{\link{make_tabulate_surf_periods_param_df}}; returns the filename as a
##' string.}
##'
##' }}
##'
##' \subsection{Functions that return objects}{
##'
##' \describe{
##'
##' \item{\code{make_tfr_surfs_arg_list}}{Creates a list of arguments defining an
##' alternative SURF definition.}
##'
##' \item{\code{make_alt_surfs_variant_comparison_df}}{Make a comparison table of
##' multiple variants; returns a data frame.}
##'
##' }}
##'
##' @author Mark C Wheldon
##' @name sensitivity_analysis_helpers
##' @keywords internal
##'
##' @examples
##'
##' ## Create list of alternative SURF definitions:
##' alt_SURF_defs <-
##'     list(default = make_tfr_surfs_arg_list(sim.dir = bayesTFR_output_dir),
##'          level_cond_pII_only =
##'            make_tfr_surfs_arg_list(sim.dir = bayesTFR_output_dir,
##'                                desc = "Level cond: Phase II only",
##'                                alt_args = list(transition_condition_type = "Phase II only")))
##'
##' str(alt_SURF_defs)
NULL


###-----------------------------------------------------------------------------
### * Helpers

validate_tfr_surfs_args_list <- function(x) {

    ## Running variants takes a long time and are likely done in serial (since
    ## the 'make_tfr_surfs()' itself is parallelized), so this function is
    ## provided to check the alternative arguments at the outset.

    x_subs <- deparse(substitute(x))
    alt_args <- sapply(x[["make_tfr_surfs_args"]], "eval")
                                # Need to evaluate because if defaults are used,
                                # some elements will be 'calls' and 'match.arg'
                                # (below) will fail.

    ## Inclusions
    if (!identical(length(names(alt_args)), length(alt_args)))
        stop("All elements in '", x_subs, "[[\"make_tfr_surfs_args\"]]' must be named.")

    ## Admissible args are those that are actually in sig of 'make_tfr_surfs'.
    admis_args <- sapply(get_arg_defs(make_tfr_surfs), "eval")
    not_in <- names(alt_args)[!names(alt_args) %in% names(admis_args)]
    if (length(not_in))
        stop("'", x_subs, "[[\"make_tfr_surfs_args\"]]' can only include arguments to 'make_tfr_surfs'. The following are not: '",
             toString(not_in), "'.")

    ## sim.dir
    if (!dir.exists(alt_args[["sim.dir"]])) stop("'", alt_args[["sim.dir"]], "' does not exist.")

    ## Countries
    if ("country_codes" %in% names(alt_args) && !is.null(alt_args[["country_codes"]]))
        alt_args[["country_codes"]] <-
            validate_country_codes(alt_args[["country_codes"]], sim.dir = alt_args[["sim.dir"]])

    ## Categorical Arguments
    categorical_args <-
        admis_args[sapply(admis_args, function(z) is.character(z) && length(z) > 1)]
    for(a in names(alt_args)[names(alt_args) %in% names(categorical_args)]) {
        test <- try(match.arg(alt_args[[a]], categorical_args[[a]]), silent = TRUE)
        if (identical(class(test), "try-error"))
            stop("Argument '", a, "': ", test)
    }

    ## Numeric Arguments
    numeric_args <- admis_args[sapply(admis_args, "class") == "numeric"]
    numeric_args <- alt_args[names(alt_args) %in% names(numeric_args)]
    if (length(numeric_args)) {
        test <- names(numeric_args)[!sapply(numeric_args, "is.numeric")]
        if (length(test)) {
            stop("The following arguments must be numeric: '",
                 toString(test), "'.")
        }
    }

    return(x)
}


###-----------------------------------------------------------------------------
### * Alternative SURF Variant Arguments

##' @inheritParams make_tfr_surfs
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @noRd
make_tfr_surfs_args_templ <- function(sim.dir = getOption("tfrSURFs.sim.dir")) {
    stopifnot(dir.exists(sim.dir))
    out <- modifyList(get_arg_defs(make_tfr_surfs), list(sim.dir = sim.dir))
    return(out)
}


##' @inheritParams make_tfr_surfs
##' @param id Character; short ID name for the alternative SURF definition.
##' @param desc Character; textual description of alternative SURF definition.
##' @param alt_args List of arguments defining the alternative SURF definition.
##'     This is passed to \code{\link{make_tfr_surfs}} so it can only contain
##'     valid arguments to that function.
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_tfr_surfs_arg_list <- function(id = "default", desc = "Default",
                                     sim.dir = getOption("tfrSURFs.sim.dir"), alt_args = NULL) {

    ## Arg Checks
    stopifnot(is.character(id))
    stopifnot(is.character(desc))

    if (is.null(alt_args) || !length(alt_args)) {

        ## Return Default

        return(validate_tfr_surfs_args_list(
            structure(list(id = id, desc = desc,
                           make_tfr_surfs_args =
                               make_tfr_surfs_args_templ(sim.dir = sim.dir)),
                      class = c("tfr_surfs_args_list", "list"))
        ))

    } else {

        stopifnot(is.list(alt_args))

        ## Modify Args

        return(validate_tfr_surfs_args_list(
            structure(list(id = id, desc = desc,
                           make_tfr_surfs_args =
                               modifyList(make_tfr_surfs_args_templ(sim.dir = sim.dir),
                                          alt_args)),
                      class = c("tfr_surfs_args_list", "list"))
        ))
    }
}


###-----------------------------------------------------------------------------
### * Directory Structure and Filenames

##' @param alt_id ID of the alternative SURF definition.
##' @param root_dir Root directory for the output files.
##' @param output_dir_name Name of the top-level output directory (exists in
##'     \code{root_dir}.
##' @param create Logical; create the directories if they don't exist?
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_output_dir_list <- function(alt_id, root_dir = getwd(),
                                           output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name"),
                                           create = TRUE) {
    dir_list <- list(
        output_dir = file.path(root_dir, output_dir_name, alt_id))
    dir_list <- c(dir_list, list(
                                rds_dir = file.path(dir_list[["output_dir"]], "rds"),
                                plots_dir = file.path(dir_list[["output_dir"]], "plots")))
    dir_list <- c(dir_list, list(
                                pdf_plots_dir = file.path(dir_list[["plots_dir"]], "pdf"),
                                svg_plots_dir = file.path(dir_list[["plots_dir"]], "svg"),
                                tables_dir = file.path(dir_list[["output_dir"]], "tables")))

    if (create) {
        created <- unlist(lapply(setNames(nm = dir_list), function(z) {
            if (!dir.exists(z)) dir.create(z, recursive = TRUE)
            else FALSE
        }))
        if (length(created[created])) {
            if (getOption("tfrSURFs.verbose")) message("Made output directories in '", file.path(root_dir, output_dir_name), "'.")
        } else if (getOption("tfrSURFs.verbose")) message("Output directories already exist.")
    }
    return(dir_list)
}


##' @inheritParams make_alt_surfs_output_dir_list
##' @param ext File extension
##' @param suff Suffix for filename.
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_filenames <- function(alt_id, ext = c("rds", "pdf", "csv", "xlsx"), median_only = FALSE, suff = NULL) {

    if (!is.null(ext) && length(ext) > 1) {
        return(lapply(setNames(nm = ext), function(x) {
            make_alt_surfs_filenames(alt_id = alt_id, ext = x, median_only = median_only, suff = suff)
        }))
    }

    if (!is.null(suff) && length(suff) > 1)  {
        return(lapply(setNames(nm = suff), function(y) {
            make_alt_surfs_filenames(alt_id = alt_id, ext = ext, median_only = median_only, suff = y)
        }))
    }

    if (median_only) fn_add <- "median_"
    else fn_add <- ""

    filename <- paste0("tfr_surf_variant_", fn_add, alt_id)
    if (!is.null(suff)) filename <- paste0(filename, suff)
    if (!is.null(ext)) {
        if (identical(substr(ext, 1, 1), ".")) {
            warning("'ext' starts with '.'. This has _not_ removed; re-run if you don't intend to have '..' in the filename.")
        }
        filename <- paste0(filename, ".", ext)
    }

    return(filename)
}


##' @inheritParams make_alt_surfs_stats_tables
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_variant_comparison_table_filename <- function(alt_surfs, median_only = FALSE,
                                                             stat = c("count", "avg_len")) {

    ## !!! NOTE: THIS ISN'T USED (or tested) YET

    if (is.null(names(alt_surfs))) names(alt_surfs) <- sapply(alt_surfs, "[[", "id")
    ## These have to be here to get the file name correct
    stat <- match.arg(stat)

    dir_list <- make_alt_surfs_output_dir_list(names(alt_surfs)[1], output_dir_name = output_dir_name, create = FALSE)
    dir_output <- dirname(dir_list[["output_dir"]])

    fp <- file.path(dir_output, paste0("compare_surfs_", stat))
    if (median_only) fp <- paste0(fp, "_median")
    fp <- paste0(fp, ".xlsx")

    return(fp)
}


###-----------------------------------------------------------------------------
### * Create the Results

##' @inheritParams make_alt_surfs_output_dir_list
##' @param x Argument on which S3 method dispatch is performed.
##' @param median_only Logical; generate results using only posterior median
##'     TFRs as input? Filenames will have \dQuote{\file{_median}} appended.
##'     This convention is used by other functions to identify medians-only
##'     results.
##' @param ext File extension
##' @param suff Suffix for filename.
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs <- function(x, ...) {
    UseMethod("make_alt_surfs")
}


##' @param verbose Logical; print informational messages?
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs.list <- function(x, ...,
                                median_only = FALSE,
                                output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name"),
                                overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite")) {

    x <- lapply(x, validate_tfr_surfs_args_list)

    if (is.null(names(x))) names(x) <- sapply(x, "[[", "id")

    if (getOption("tfrSURFs.verbose")) message("\nMaking surf variants >>>>>>>>>>")
    out <- lapply(X = x, FUN = make_alt_surfs,
                  median_only = median_only,
                  output_dir_name = output_dir_name,
                  overwrite = overwrite)
    if (getOption("tfrSURFs.verbose")) message("\n\n<<<<<<<<<< Finished making surf variants.")
    return(invisible(out))
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs.tfr_surfs_args_list <- function(x,
                                                median_only = FALSE,
                                                output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name"),
                                                overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite")) {

    x <- validate_tfr_surfs_args_list(x)

    if (median_only) {
        x[["make_tfr_surfs_args"]][["median_only"]] <- TRUE
        x <- validate_tfr_surfs_args_list(x)
        msg <- paste0("Computing surfs for id '", x[["id"]], "' (median only).")
    } else {
        msg <- paste0("Computing surfs for id '", x[["id"]], "'.")
    }

    dir_list <- make_alt_surfs_output_dir_list(x[["id"]], output_dir_name = output_dir_name)
    rds_filename <-
        file.path(dir_list[["rds_dir"]],
                  make_alt_surfs_filenames(x[["id"]], median_only = median_only)[["rds"]])

    if (!file.exists(rds_filename) || isTRUE(overwrite)) {
        if (getOption("tfrSURFs.verbose")) message(msg)
        tfr_surf_df_list <- do.call(make_tfr_surfs, as.list(x[["make_tfr_surfs_args"]]))
        saveRDS(tfr_surf_df_list, file = rds_filename)
    } else {
        if (getOption("tfrSURFs.verbose")) message("Reading surfs from '", basename(rds_filename), "'.")
        tfr_surf_df_list <- readRDS(rds_filename)
    }

    return(invisible(tfr_surf_df_list))
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
get_rds_filepath <- function(alt_id,
                             median_only = FALSE,
                             output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name"),
                             overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite")) {
    dir_list <- make_alt_surfs_output_dir_list(alt_id, output_dir_name = output_dir_name)
    rds_filename <-
        file.path(dir_list[["rds_dir"]],
                  make_alt_surfs_filenames(alt_id, median_only = median_only)[["rds"]])
    return(rds_filename)
}


###-----------------------------------------------------------------------------
### * Make Tables

###-----------------------------------------------------------------------------
### ** SURF Stats

###-----------------------------------------------------------------------------
### *** Individual Tables

##' @inheritParams make_alt_surfs_plots
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_db_tables <- function(x, ...) {
    UseMethod("make_alt_surfs_db_tables")
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_db_tables.list <- function(x, ...,
                                          median_only = FALSE,
                                          incl_small_countries = FALSE,
                                          overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                          output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {

    if (is.null(names(x))) names(x) <- sapply(x, "[[", "id")

    out <- sapply(names(x), function(this_alt_id) {
        this_x <- x[[this_alt_id]]
        make_alt_surfs_db_tables(this_alt_id,
                                    median_only = median_only,
                                    incl_small_countries = incl_small_countries,
                                    overwrite = overwrite,
                                    output_dir_name = output_dir_name)
    })
    if (getOption("tfrSURFs.verbose")) message("Tables written to '", dirname(head(out, 1)), "', ..., '",   dirname(tail(out, 1)), "'.")
    return(invisible(out))
}




##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_db_tables.character <- function(x, ...,
                                          median_only = FALSE,
                                          incl_small_countries = FALSE,
                                          overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                          output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {

    if (length(x) > 1) stop("'x' can only be of length 1.")

    rds_filename <-
        file.path(make_alt_surfs_output_dir_list(x, create = FALSE)[["rds_dir"]],
                  make_alt_surfs_filenames(x, median_only = median_only)[["rds"]])

    tbl_filename <- file.path(make_alt_surfs_output_dir_list(x, create = FALSE)[["tables_dir"]],
                              make_alt_surfs_filenames(x, ext = "xlsx", suff = "_surfs_db"))

    if (!overwrite && file.exists(tbl_filename)) {
        if (file.mtime(tbl_filename) > file.mtime(rds_filename))
            if (getOption("tfrSURFs.verbose")) message(toString(x), ": Nothing to do; 'overwrite' is 'FALSE' and 'xlsx' file is newer than 'rds' file.")
        return(invisible(tbl_filename))
    } else {
        alt_surfs_res_list <- readRDS(rds_filename)
    }

    openxlsx::write.xlsx(do.call("rbind", alt_surfs_res_list), file = tbl_filename,
                         asTable = TRUE, tableStyle = "TableStyleMedium3")

    if (getOption("tfrSURFs.verbose")) message("Tables written to '", toString(tbl_filename))
    return(invisible(tbl_filename))
}


##' @inheritParams make_alt_surfs_plots
##' @param fn_suff Filename suffix.
##' @param stat,geographies Passed to \code{\link{tabulate_surf_stats}}.
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_stats_tables <- function(x, ...) {
    UseMethod("make_alt_surfs_stats_tables")
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_stats_tables.list <- function(x, ...,
                                             median_only = FALSE,
                                             incl_small_countries = FALSE,
                                             overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                             stat = c("count", "avg_len"),
                                             output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {
    if (is.null(names(x))) names(x) <- sapply(x, "[[", "id")
    out <- sapply(names(x), function(this_alt_id) {
        this_x <- x[[this_alt_id]]
        make_alt_surfs_stats_tables(this_alt_id,
                                    median_only = median_only,
                                    incl_small_countries = incl_small_countries,
                                    overwrite = overwrite,
                                    stat = stat,
                                    output_dir_name = output_dir_name)
    })
    if (getOption("tfrSURFs.verbose")) message("Tables written to '", dirname(head(out, 1)), "', ..., '",   dirname(tail(out, 1)), "'.")
    return(invisible(out))
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_stats_tables.character <- function(x,
                                                  median_only = FALSE,
                                                  incl_small_countries = FALSE,
                                                  overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                                  stat = c("count", "avg_len"),
                                                  output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {

    if (length(x) > 1) stop("'x' can only be of length 1.")

    geographies <- c("area_name", "reg_name", "name")

    stat <- match.arg(stat)

    fn_suff <- paste0("__stat_", toString(stat), "__by_country")
    dir_list <- make_alt_surfs_output_dir_list(x, output_dir_name = output_dir_name, create = FALSE)

    tbl_filename <-
        file.path(dir_list[["tables_dir"]],
                  make_alt_surfs_filenames(x, median_only = median_only, suff = fn_suff)[["xlsx"]])

    rds_filename <-
        file.path(dir_list[["rds_dir"]],
                  make_alt_surfs_filenames(x, median_only = median_only)[["rds"]])
    if (!overwrite && file.exists(tbl_filename)) {
        if (file.mtime(tbl_filename) > file.mtime(rds_filename))
            if (getOption("tfrSURFs.verbose")) message(toString(x), ": Nothing to do; 'overwrite' is 'FALSE' and 'xlsx' file is newer than 'rds' file.")
        return(invisible(tbl_filename))
    } else {
        alt_surfs_res_list <- readRDS(rds_filename)
    }

    openxlsx::write.xlsx(tabulate_surf_stats(alt_surfs_res_list, stat = stat,
                                             geographies = geographies,
                                             incl_small_countries = incl_small_countries),
                         file = tbl_filename,
                         asTable = TRUE, tableStyle = "TableStyleMedium2")
    if (getOption("tfrSURFs.verbose")) message("Tables written to '", toString(tbl_filename))
    return(invisible(tbl_filename))
}


###-----------------------------------------------------------------------------
### *** Comparison Tables

##' @inheritParams make_alt_surfs_stats_tables
##' @param alt_surfs Either a character vector of alternative SURF definition ID
##'     names, or a list. If the latter, the names will be taken as alternative
##'     SURF ID names.
##' @param timevar Controls how the output table is reshaped. If \code{"geog"},
##'     the most granular of \code{geographies} is used.
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_variant_comparison_df <- function(alt_surfs, median_only = FALSE,
                                                 stat = c("count", "avg_len"),
                                                 incl_small_countries = FALSE,
                                                 filter_zero_rows = TRUE,
                                                 geographies = c("area_name", "reg_name", "name", "sub_saharan_africa", "global"),
                                                 timevar = c("alt_surf", "geog"),
                                                 output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {

    ## Only need the names of 'alt_surfs' because results will be loaded from disc.
    id_desc <- do.call("rbind", lapply(alt_surfs, function(z) data.frame(id = z[["id"]], desc = z[["desc"]])))
    if (is.list(alt_surfs)) alt_surfs <- names(alt_surfs)
    stat <- match.arg(stat)
    geographies <- match.arg(geographies, several.ok = TRUE)
    timevar <- match.arg(timevar)

    if (identical(timevar, "geog") && length(geographies[geographies != "global"]) > 1)
        stop("'timevar' = 'geog': 'geographies' must have only one element (plus, optionally, 'global'.)")

    out <- list()
    for (this_alt_id in alt_surfs) {
        dir_list <- make_alt_surfs_output_dir_list(this_alt_id, output_dir_name = output_dir_name, create = FALSE)
        rds_filename <-
            file.path(dir_list[["rds_dir"]],
                      make_alt_surfs_filenames(this_alt_id, median_only = median_only)[["rds"]])
        out <-
            c(out,
              list(data.frame(
                  var = this_alt_id,
                  tabulate_surf_stats(readRDS(file = rds_filename),
                                      stat = stat,
                                      incl_small_countries = incl_small_countries,
                                      filter_zero_rows = filter_zero_rows,
                                      geographies = geographies))))
    }

    out <- do.call("rbind", out)

    if (identical(timevar, "alt_surf")) {

        newcols <- paste(stat, unique(out[["var"]]), sep = ".")
        out <- stats::reshape(out,
                              direction = "wide",
                              idvar = geographies,
                              timevar = "var", v.names = stat)

    } else if (identical(timevar, "geog")) {

        ## TEMP: (2025-09-12) This is _hack_ to fix missing 'Northern America' in sub_reg
        if ("reg_name" %in% colnames(out)) {
            out[out[["reg_name"]] == "", "reg_name"] <- "Northern America"
            }

        for (geo in c("name", "reg_name", "area_name", "sub_saharan_africa", "global")) {
            if (geo %in% geographies) {
                tv <- geo
                newcols <- paste(stat, unique(out[[geo]]), sep = ".")
                break() # should break out of the 'for' loop.
            }
        }

        out <- stats::reshape(out[, colnames(out) != "global"],
                              direction = "wide",
                              idvar = "var",
                              timevar = tv, v.names = stat)

        out <- base::merge(out, id_desc,
                           by.x = "var", by.y = "id", all.x = TRUE, all.y = FALSE,
                           sort = FALSE)
        out <- out[, c("var", "desc", colnames(out)[!colnames(out) %in% c("var", "desc")])]
    }

    colnames(out)[colnames(out) %in% newcols] <-
        gsub(paste0(stat, "."), "", colnames(out)[colnames(out) %in% newcols], fixed = TRUE)

    return(out)
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_variant_comparison_table <- function(alt_surfs,
                                                    median_only = FALSE,
                                                    stat = c("count", "avg_len"),
                                                    incl_small_countries = FALSE,
                                                    filter_zero_rows = TRUE,
                                                    overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                                    output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {


    if (is.null(names(alt_surfs))) names(alt_surfs) <- sapply(alt_surfs, "[[", "id")

    ## These have to be here to get the file name correct
    stat <- match.arg(stat)

    dir_list <- make_alt_surfs_output_dir_list(names(alt_surfs)[1], output_dir_name = output_dir_name, create = FALSE)
    dir_output <- dirname(dir_list[["output_dir"]])

    fp <- file.path(dir_output, paste0("compare_surfs_", stat))
    if (median_only) fp <- paste0(fp, "_median")
    fp <- paste0(fp, ".xlsx")

    if (file.exists(fp) && isFALSE(overwrite)) {
        rds_mtimes <- lapply(names(alt_surfs), function(z) {
            rds_fn <- file.path(make_alt_surfs_output_dir_list(z, output_dir_name = output_dir_name, create = FALSE)[["rds_dir"]],
                                make_alt_surfs_filenames(z, median_only = median_only)[["rds"]])
            return(file.mtime(rds_fn))
        })
        if (all(unlist(lapply(rds_mtimes, function(z) z < file.mtime(fp))))) {
            if (getOption("tfrSURFs.verbose")) message("Nothing to do: the table file '", fp, "' is newer than all '.rds' files.")
            return(invisible(fp))
        }
    }

    geog_sheets <-
        list(countries = c("area_name", "reg_name", "name", "global"),
             subregions = c("reg_name", "global"),
             regions = c("area_name", "global"),
             SSA = c("sub_saharan_africa", "global")
             )

    out <- lapply(geog_sheets, function(z) {
        if ("name" %in% z) tv <- "alt_surf"
        else tv <- "geog"
        make_alt_surfs_variant_comparison_df(alt_surfs, stat = stat,
                                             incl_small_countries = incl_small_countries,
                                             median_only = median_only,
                                             filter_zero_rows = filter_zero_rows,
                                             geographies = z,
                                             timevar = tv)
    })

    if (!length(out)) {
        warning("No comparison tables produced: 'make_alt_surfs_variant_comparison_df()' returned no tables to save.")
        return(NULL)
    } else {
        openxlsx::write.xlsx(out, file = fp,
                             asTable = TRUE, tableStyle = "TableStyleMedium2")
        if (getOption("tfrSURFs.verbose")) message("Comparison table written to '", fp, "'.")
        return(invisible(fp))
    }
}


###-----------------------------------------------------------------------------
### ** SURF Periods

##' @inheritParams make_alt_surfs_stats_tables
##' @param fn_suff Filename suffix.
##' @param stat,geographies Passed to \code{\link{tabulate_surf_stats}}.
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_periods_tables <- function(x, ...) {
    UseMethod("make_alt_surfs_periods_tables")
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_periods_tables.list <- function(x, ...,
                                             median_only = FALSE,
                                             incl_small_countries = FALSE,
                                             overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                             table_type = c("concise", "surfs only", "detailed"),
                                             output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {
    if (is.null(names(x))) names(x) <- sapply(x, "[[", "id")
    table_type <- match.arg(table_type)
    out <- sapply(names(x), function(this_alt_id) {
        this_x <- x[[this_alt_id]]
        make_alt_surfs_periods_tables(this_alt_id,
                                    median_only = median_only,
                                    incl_small_countries = incl_small_countries,
                                    overwrite = overwrite,
                                    table_type = table_type,
                                    output_dir_name = output_dir_name)
    })
    if (getOption("tfrSURFs.verbose")) message("Tables written to '", dirname(head(out, 1)), "', ..., '",   dirname(tail(out, 1)), "'.")
    return(invisible(out))
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_periods_tables.character <- function(x,
                                                  median_only = FALSE,
                                                  incl_small_countries = FALSE,
                                                  overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                                  table_type = c("concise", "surfs only", "detailed"),
                                                  output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {

    if (length(x) > 1) stop("'x' can only be of length 1.")
    table_type <- match.arg(table_type)

    fn_suff <- paste0("__periods_", toString(table_type))
    dir_list <- make_alt_surfs_output_dir_list(x, output_dir_name = output_dir_name, create = FALSE)

    tbl_filename <-
        file.path(dir_list[["tables_dir"]],
                  make_alt_surfs_filenames(x, median_only = median_only, suff = fn_suff)[["xlsx"]])

    rds_filename <-
        file.path(dir_list[["rds_dir"]],
                  make_alt_surfs_filenames(x, median_only = median_only)[["rds"]])
    if (!overwrite && file.exists(tbl_filename)) {
        if (file.mtime(tbl_filename) > file.mtime(rds_filename))
            if (getOption("tfrSURFs.verbose")) message(toString(x), ": Nothing to do; 'overwrite' is 'FALSE' and 'xlsx' file is newer than 'rds' file.")
        return(invisible(tbl_filename))
    } else {
        alt_surfs_res_list <- readRDS(rds_filename)
    }

    openxlsx::write.xlsx(tabulate_surf_periods(alt_surfs_res_list, table_type = table_type,
                                             incl_small_countries = incl_small_countries),
                         file = tbl_filename,
                         asTable = TRUE, tableStyle = "TableStyleMedium2")
    if (getOption("tfrSURFs.verbose")) message("Tables written to '", toString(tbl_filename))
    return(invisible(tbl_filename))
}


###-----------------------------------------------------------------------------
### * Make Plots

##' @inheritParams make_alt_surfs
##' @inheritParams make_tfr_surfs
##' @param alt_surfs_res_list List of results of running SURF variants, as
##'     created by \code{\link{make_alt_surfs}}. If this is
##'     \code{NULL}, the results list will be loaded from files pointed to by
##'     \code{dir_list}.
##' @param plot_ann Text string to add to plots describing the variant.
##' @param add_median_TFR_surfs Logical; add the medians-only
##'     \code{\link[=ggplot2::geom_rug]{rugs}} to the plot?
##' @param overwrite Logical; overwrite any existing plot files.
##'
##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_plots <- function(x, ...) {
    UseMethod("make_alt_surfs_plots")
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_plots.list <- function(x, file_type = c("pdf", "svg"), ...,
                                      plot_ann = NULL,
                                      add_median_TFR_surfs = TRUE,
                                      incl_small_countries = FALSE,
                                      overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                      output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {
    if (is.null(names(x))) names(x) <- sapply(x, "[[", "id")
    for (this_alt_id in names(x)) {
        this_x <- x[[this_alt_id]]
        make_alt_surfs_plots(x = this_alt_id,
                             file_type = file_type,
                             plot_ann = paste0("SURF Variant: ", this_x[["desc"]]),
                             add_median_TFR_surfs = add_median_TFR_surfs,
                             overwrite = overwrite,
                             output_dir_name = output_dir_name)
    }
}


##' @rdname sensitivity_analysis_helpers
##' @keywords internal
##' @export
make_alt_surfs_plots.character <- function(x, file_type = c("pdf", "svg"),
                                           plot_ann = paste0("SURF Variant: ", toString(x)),
                                           add_median_TFR_surfs = TRUE,
                                           incl_small_countries = FALSE,
                                           overwrite = getOption("tfrSURFs.sensitivity_analysis_overwrite"),
                                           output_dir_name = getOption("tfrSURFs.sensitivity_analysis_output_dir_name")) {

    file_type <- match.arg(file_type)
    op <- options(tfrSURFs.message_about_unknown_aes = FALSE)
    on.exit(options(op), add = TRUE, after = FALSE)
    on.exit(graphics.off(), add = TRUE, after = FALSE)

    dir_list <- make_alt_surfs_output_dir_list(x, output_dir_name = output_dir_name, create = FALSE)

    rds_filename <-
        file.path(dir_list[["rds_dir"]], make_alt_surfs_filenames(x)[["rds"]])
    if (add_median_TFR_surfs) {
        rds_median_filename <-
            file.path(dir_list[["rds_dir"]], make_alt_surfs_filenames(x, median_only = TRUE)[["rds"]])
    }

    if (identical(file_type, "pdf")) {

        ## -------* PDF Plots

        filename_s <-
            file.path(dir_list[["pdf_plots_dir"]], make_alt_surfs_filenames(x)[["pdf"]])

        if (!overwrite && file.exists(filename_s) &&
            file.mtime(filename_s) > file.mtime(rds_filename)) {
            if (getOption("tfrSURFs.verbose")) message(toString(x), ": Nothing to do; 'overwrite' is 'FALSE' and 'pdf' file is newer than 'rds' file.")
            return(invisible(filename_s))
        } else {
            alt_surfs_res_list <- readRDS(rds_filename)
        }
        if (add_median_TFR_surfs) {
            if (!file.exists(rds_median_filename)) {
                add_median_TFR_surfs <- FALSE
                warning("'", rds_median_filename, "' not found: setting 'add_median_filename' to 'FALSE'.")
            } else {
                alt_surfs_res_list_medians <- readRDS(rds_median_filename)
            }
        }

        if (!incl_small_countries)
            alt_surfs_res_list <- remove_small_countries(alt_surfs_res_list)

        pdf(file = filename_s, height = 6, width = 14)
        for (cc in names(alt_surfs_res_list)) {
            if (!add_median_TFR_surfs) {
                print(plot_surfs_probs(alt_surfs_res_list[[cc]],
                                        plot_ann = plot_ann,
                                        datestamp = TRUE))
            } else {
                print(plot_surfs_probs(x = alt_surfs_res_list[[cc]],
                                        x_alt = alt_surfs_res_list_medians[[cc]],
                                        x_alt_label = "Median only",
                                        plot_ann = plot_ann,
                                        datestamp = TRUE))
            }
        }

        dev.off()

        if (getOption("tfrSURFs.verbose")) message("Plots saved to '", toString(filename_s))

    } else if (identical(file_type, "svg")) {

        ## -------* SVG Plots

        filename_s <- character()
        flag_already_exist <- FALSE

        alt_surfs_res_list <- readRDS(rds_filename)
        if (!incl_small_countries)
            alt_surfs_res_list <- remove_small_countries(alt_surfs_res_list)

        for (cc in names(alt_surfs_res_list)) {
            svg_filename_cc <-
                file.path(dir_list[["svg_plots_dir"]],
                          make_alt_surfs_filenames(x, ext = "svg",
                                                   suff = paste0("_", get_country_names(cc), "_", cc)))

            filename_s <- c(filename_s, basename(svg_filename_cc))

            if (add_median_TFR_surfs) {
                if (!file.exists(rds_median_filename)) {
                    add_median_TFR_surfs <- FALSE
                    warning("'", rds_median_filename, "' not found: setting 'add_median_filename' to 'FALSE'.")
                } else {
                    alt_surfs_res_list_medians <- readRDS(rds_median_filename)
                }
            }

            if (!overwrite && file.exists(svg_filename_cc) &&
                file.mtime(svg_filename_cc) > file.mtime(rds_filename)) {
                flag_already_exist <- TRUE
                next()
            }

            try({
                svg(file = svg_filename_cc, height = 6, width = 14)
                if (!add_median_TFR_surfs) {
                    print(plot_surfs_probs(alt_surfs_res_list[[cc]],
                                            plot_ann = plot_ann,
                                            datestamp = TRUE))
                dev.off()
                } else {
                    print(plot_surfs_probs(alt_surfs_res_list[[cc]],
                                            x_alt = alt_surfs_res_list_medians[[cc]],
                                            x_alt_label = "Median only",
                                            plot_ann = plot_ann,
                                            datestamp = TRUE))
                    dev.off()
                }
            })
        }
        if (flag_already_exist)
            message("Argument 'overwrite' is 'FALSE' and some svg files already exist and are newer than the rds file.")
        if (getOption("tfrSURFs.verbose")) message("Plots saved to '", toString(dir_list[["svg_plots_dir"]]))
    }

    options(op)
    return(invisible(filename_s))
}
