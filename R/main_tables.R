################################################################################
###
### Tabulation functions
###
################################################################################

###-----------------------------------------------------------------------------
### * Counts of Locations by SURF Status

##' Tables of Locations by tfrSURFs
##'
##' Produces tables of counts of countries and geographic regions by
##' presence/absence of tfrSURFs.
##'
##' @param x Output of \code{\link{make_tfr_surfs}}.
##' @param incl_small_countries Logical; exclude countries with populations less
##'     than 90000 in mid 2024?
##' @param ... Passed to other methods.
##' @return A data frame.
##' @author Mark C Wheldon
##'
##' @family Tabulation
##'
##' @export
tabulate_loc_by_surf <- function(...) {
    UseMethod("tabulate_loc_by_surf")
}

##' @rdname tabulate_loc_by_surf
##' @export
tabulate_loc_by_surf.list <- function(x, incl_small_countries = TRUE, ...,
                                      geographies = c("area_name", "reg_name", "name", "sub_saharan_africa", "global"),
                                      stat = c("years", "surfs"), by_surf = FALSE) {
    stopifnot(is.logical(incl_small_countries))
    if (!incl_small_countries) x <- remove_small_countries(x)

    out <- tabulate_loc_by_surf(do.call("rbind", x),
                                geographies = geographies, stat = stat,
                                by_surf = by_surf, ...)
    row.names(out) <- NULL
    return(out)
}

##' @rdname tabulate_loc_by_surf
##' @export
tabulate_loc_by_surf.data.frame <- function(x, ...,
                                            geographies = c("area_name", "reg_name", "name", "sub_saharan_africa", "global"),
                                            stat = c("years", "surfs"), by_surf = FALSE) {

    ## -------* Arg Checks

    geographies <- match.arg(geographies, several.ok = TRUE)
    stat <- match.arg(stat, several.ok = TRUE)
    stopifnot(is.logical(by_surf))

    ## -------* BODY

    ## -------** Handle `"global"` in `geographies`

    if ("global" %in% geographies) {
        if (length(geographies) > 1) {
            out <- tabulate_loc_by_surf(x = x, stat = stat,
                                        geographies = "global", by_surf = by_surf,
                                       ...)
            out <- data.frame(as.data.frame(lapply(setNames(nm = geographies[!geographies == "global"]),
                                                   function(z) "GLOBAL")),
                              out)
            ## vvv EARLY RETURN
            return(rbind(data.frame(tabulate_loc_by_surf(x = x, stat = stat,
                                                         geographies = geographies[!geographies == "global"],
                                                         by_surf = by_surf,
                                                        ...),
                                    global = FALSE),
                         out))
            ## ^^^ EARLY RETURN
        } else {
            x$global <- TRUE
        }
    }

    ## -------** Main Tabulation

    x_stat_cols <- c(surfs = "surf_year_start", years = "surf_year")[stat]
    if (by_surf) geographies <- c(geographies, "surf_year_group")

    out <- stats::aggregate(x[, x_stat_cols, drop = FALSE],
                                by = x[, geographies, drop = FALSE],
                            FUN = "sum", na.rm = TRUE)

    ## Rename columns to 'stat'
    for (j in seq_along(x_stat_cols)) {
        colnames(out)[which(colnames(out) == x_stat_cols[j])] <- names(x_stat_cols)[j]
    }

    ## -------* END

    return(out[do.call("order", unname(out[, geographies, drop = FALSE])), , drop = FALSE])
}

###-----------------------------------------------------------------------------
### * SURF Frequencies and Lengths

##' Tables of SURF summary statistics
##'
##' Produces tables of SURF counts and average lengths by geographic regions and
##' other geographies.
##'
##' Some tfrSURFs occur in the future (defined by \code{last_est_year}), and
##' hence are based on projected fertility rates. Using \code{proj_split}, these
##' can be separately tabulated as follows:
##' \describe{
##' \item{\code{"none"}}{No distinction is made based on projection or estimation years.}
##' \item{\code{"by_surf"}}{A SURF is considered wholly within the projection period if more than half of it occurs after \code{last_est_year}.}
##' \item{\code{"by_year"}}{The average lengths are computed based on the actual number of years in each period (estimation or projection).}
##' }
##'
##' @param x Output of \code{\link{make_tfr_surfs}}.
##' @param stat Character; the statistic to tabulate.
##' @param geographies Cross-classifying geographies for the table. These must
##'     be columns in \code{x}.
##' @param filter_zero_rows Logical; remove rows where \code{stat} is zero for
##'     all columns. For example, don't include countries with not tfrSURFs in the
##'     table?
##' @param proj_split Character; determines how the tfrSURFs are split between
##'     estimation and projection types. See \dQuote{Details}.
##' @param last_est_year Numeric; the first year of the projection period, for
##'     the purposes of \code{proj_split}. This is ignored if \code{proj_split}
##'     \code{=} \code{"none"}.
##' @param ... Passed to other methods.
##' @inheritParams tabulate_loc_by_surf
##' @return A data frame.
##' @author Mark C Wheldon
##'
##' @family Tabulation
##'
##' @export
tabulate_surf_stats <- function(x, ...) {
    UseMethod("tabulate_surf_stats")
}

##' @rdname tabulate_surf_stats
##' @export
tabulate_surf_stats.list <- function(x, stat = c("count", "avg_len"), incl_small_countries = TRUE,
                                     geographies = c("area_name", "reg_name", "name", "sub_saharan_africa", "global"),
                                     filter_zero_rows = TRUE,
                                     proj_split = c("none", "by_surf", "by_year"),
                                     last_est_year = x[[1]][1, "bayesTFR_present_year"]) {
    stopifnot(is.logical(incl_small_countries))
    if (!incl_small_countries) x <- remove_small_countries(x)
    return(tabulate_surf_stats(do.call("rbind", c(x, list(make.row.names = TRUE))),
                               stat = stat, geographies = geographies, filter_zero_rows = filter_zero_rows,
                               proj_split = proj_split,
                                last_est_year = last_est_year))
}


##' @rdname tabulate_surf_stats
##' @export
tabulate_surf_stats.data.frame <- function(x, stat = c("count", "avg_len"),
                                           geographies = c("area_name", "reg_name", "name", "sub_saharan_africa", "global"),
                                           filter_zero_rows = TRUE,
                                           proj_split = c("none", "by_surf", "by_year"),
                                           last_est_year = x[1, "bayesTFR_present_year"]) {

    ## -------* Arg Checks

    stat <- match.arg(stat)
    proj_split <- match.arg(proj_split)
    geographies <- match.arg(geographies, several.ok = TRUE)
    stopifnot(is.logical(filter_zero_rows))

    if (identical(proj_split, "by_year")) {
        if (identical(stat, "count")) stop("Cannot use 'proj_split = \"by_year\"' with 'stat = \"count\"'.")
    }

    ## -------* Functions

    if (identical(stat, "count")) {
        tbl_fn <- function(z, f) {
            z <- table(z[z[["surf_year_start"]], geographies, drop = FALSE])
            z <- as.data.frame(z, responseName = "count", stringsAsFactors = FALSE)
            z <- z[z$count > 0, , drop = FALSE]
        }

    } else if (identical(stat, "avg_len")) {

        if (!identical(proj_split, "by_year")) {
            tbl_fn <- function(z, f) {
                z <- stats::aggregate(z[z[["surf_year_start"]], "surf_year_len", drop = FALSE],
                                      by = z[z[["surf_year_start"]], geographies, drop = FALSE],
                                      FUN = "mean", drop = TRUE)
                colnames(z)[colnames(z) == "surf_year_len"] <- "avg_len"
                return(z)
            }

        } else {
            tbl_fn <- function(z, f) {
                ## If 'proj_split' is '"by_year"', the 'surf_year_start' and
                ## 'surf_year_len' columns are no longer useable. The surf
                ## lengths have to be recomputed by the new 'est/proj' grouping :(
                idx <- z[["surf_year"]]
                z <- as.data.frame(table(z[idx, c("country_code", "surf_year_group")]),
                                   responseName = "avg_len", stringsAsFactors = FALSE)
                z <- z[z[["avg_len"]] > 0, , drop = FALSE]
                if (any(c("sub_saharan_africa", "global") %in% geographies)) {
                    nrz <- nrow(z)
                    z <- base::merge(x = z,
                                     y = unique(x[, c("country_code",
                                                      c("sub_saharan_africa", "global")[c("sub_saharan_africa", "global") %in% geographies])]),
                                     by = "country_code",
                                     all.x = TRUE, all.y = FALSE)
                    if (!identical(nrz, nrow(z))) stop("Something wrong with merge (1).")
                }
                if (length(geographies[!geographies %in% c("sub_saharan_africa", "global")])) {
                    nrz <- nrow(z)
                    z <- base::merge(x = z,
                                     y = unique(x[, c("country_code", geographies[geographies %in% c("name", "area_name", "reg_name")])]),
                                     by = "country_code",
                                     all.x = TRUE, all.y = FALSE)
                    if (!identical(nrz, nrow(z))) stop("Something wrong with merge (2).")
                }
                z <- stats::aggregate(z[, "avg_len", drop = FALSE],
                                      by = z[, geographies, drop = FALSE],
                                      FUN = "mean", drop = TRUE)
                return(z)
            }
        }
    }

    ## -------* BODY

    ## -------** Handle `"global"` in `geographies`

    if ("global" %in% geographies) {
        if (length(geographies) > 1) {
            out <- tabulate_surf_stats(x = x, stat = stat,
                                       geographies = "global", filter_zero_rows = filter_zero_rows,
                                       proj_split = proj_split, last_est_year = last_est_year)
            out <- data.frame(as.data.frame(lapply(setNames(nm = geographies[!geographies == "global"]),
                                                   function(z) "GLOBAL")),
                              out)
            ## vvv EARLY RETURN
            return(rbind(data.frame(tabulate_surf_stats(x = x, stat = stat,
                                                        geographies = geographies[!geographies == "global"],
                                                        filter_zero_rows = filter_zero_rows,
                                                        proj_split = proj_split,
                                                        last_est_year = last_est_year),
                                    global = FALSE),
                         out))
            ## ^^^ EARLY RETURN
        } else {
            x$global <- TRUE
        }
    }

    ## -------** Main Tabulation

    out <- tbl_fn(x, geographies)

    if (!identical(proj_split, "none")) {

        if (identical(proj_split, "by_year")) {
            ## Need to re-define the 'surf_in_proj' column.
            x <- add_surf_in_proj(x, last_est_year = last_est_year, proj_split = proj_split)
        }

        no_proj_idx <- which(!x[["surf_in_proj"]])
        if (length(no_proj_idx)) {
            out <- base::merge(out,
                               tbl_fn(x[no_proj_idx, , drop = FALSE], geographies),
                               all = TRUE, by = geographies,
                               suffixes = c("", "_estimates"))
        }

        has_proj_idx <- which(x[["surf_in_proj"]])
        if (length(has_proj_idx)) {
            out <- base::merge(out,
                               tbl_fn(x[has_proj_idx, , drop = FALSE], geographies),
                               all = TRUE, by = geographies,
                               suffixes = c("", "_projections"))
        }
    }

    ## Add geographies with zero stats
    if (!filter_zero_rows) {
        by_geog <- intersect(geographies, c("area_name", "reg_name", "name"))
        by_geog <- intersect(by_geog, colnames(UNlocations_countries))
        if (length(by_geog)) {
            out <- base::merge(x = unique(UNlocations_countries[, by_geog, drop = FALSE]),
                               y = out,
                               by = by_geog, all = TRUE)
        }
    }

    ## -------* END

    return(out[do.call("order", unname(out[, geographies, drop = FALSE])), , drop = FALSE])
}

###-----------------------------------------------------------------------------
### * Tabulate SURF Periods

##' Mark blocks of surfs
##'
##' Based on \code{\link[FPPlateaus]{get_fp_plateau_countries}} and
##' \code{\link[FPPlateaus]{make_main_results_df}}.
##'
##' @param x Output of \code{\link{make_tfr_surfs}}.
##' @param table_type see \code{\link{tabulate_surf_periods}}.
##' @return \code{x} with extra columns.
##' @author Mark Wheldon
##'
##' @family Tabulation
##'
##' @noRd
add_surf_periods_blocks <- function(x, table_type) {

    ## Functions
    tbl_blocks_fn <- function(z) {

        z <- z[, c("Schoumaker_stall_any", "transition_condition_met", "year")]
        colnames(z)[colnames(z) == "Schoumaker_stall_any"] <- "surf_year"

        ## if (any(!z[["transition_condition_met"]])) {
        ##     z2 <- z[!z[["transition_condition_met"]], ]
        ##     z2[["transition_condition_met"]] <- TRUE
        ##     z <- rbind(add_surf_lengths(z2, min_surf_length = 1),
        ##                add_surf_lengths(z, min_surf_length = 1))
        ##     z <- z[order(z[["year"]]), ]
        ## } else {
            z <- add_surf_lengths(z, min_surf_length = 1)
        ## }

        for (nm in c("surf_year_start_min_length",
                     "surf_year_group_min_length", "surf_year_len_min_length")) {
            colnames(z)[colnames(z) == nm] <-
                paste0("Schoumaker_",
                       gsub("_min_length$", "",
                            gsub("surf_", "stall_", nm, fixed = TRUE),
                            fixed = FALSE))
        }

        z$Schoumaker_stall_year_any <- !is.na(z$Schoumaker_stall_year_group)

        z <- z[, c("year", "Schoumaker_stall_year_any",
                   "Schoumaker_stall_year_start",
                   "Schoumaker_stall_year_group", "Schoumaker_stall_year_len")]

        return(z)
    }

    ## Need to make sure there is only one indicator
    if (!identical(length(na.omit(unique(x[["indicator"]]))), 1L))
        stop("'indicator' must have exactly one unique value.")

    ## Any surfs?
    if (identical(table_type, "surfs only")) {
        tfr_surfs <- any(na.omit(x[["surf_year"]]))
    } else {
        tfr_surfs <- any(x[["surf_year"]] | x[["Schoumaker_stall_any"]])
    }

    if (any(tfr_surfs)) {

        ## ## TEMP: ('Schoumaker_stall_type' should already be in 'x' as of 2025-08-19)
        ## ## -->|
        ## x <- x |>
        ##     dplyr::mutate(Schoumaker_stall_type =
        ##                       dplyr::case_when(Schoumaker_stall_strong ~ "Strong+ evidence",
        ##                                        Schoumaker_stall_moderate ~ "Moderate evidence",
        ##                                        Schoumaker_stall_weak ~ "Limited evidence",
        ##                                        TRUE ~ as.character(NA)))
        ## ## |<--

        ## Stall years
        x <- x |>
            dplyr::arrange(reg_name, name, year)

        ## Create hash column
        if (identical(table_type, "surfs only")) {
            x <- x |>
                dplyr::mutate(hash_1 = paste(name, surf_year,
                                             transition_condition_met),
                                # Need level condition ^here because
                                # level condition could change part
                                # way through intervals of
                                # plateaus/surfs.
                              surf_tbl_block = 1)

        } else if (identical(table_type, "concise")) {
            ## If 'table_type' is "concise", need to create the column with
            ## Schoumaker stall ranges and types.

            ## Get stall groupings for Schoumaker stalls. Don't distinguish
            ## between evidence type, but do "no stalls" separately.
                tmp_ <- tbl_blocks_fn(x)

            ## Merge on to main data frame
            x <-
                base::merge(x, tmp_, all = TRUE, sort = TRUE)

            ## Schoumaker stalls that intersect tfrSURFs
            tmp_ <- list(surf_year_group = numeric(),
                         intersecting_Schoumaker_stall_groups = list())
            for (i in unique(na.omit(x[, "surf_year_group"]))) {
                tmp_$surf_year_group <- c(tmp_$surf_year_group, i)
                schoumaker_stall_year_groups <-
                    x[x$surf_year_group == i,
                                  "Schoumaker_stall_year_group"]
                if (all(is.na(schoumaker_stall_year_groups))) {
                    tmp_$intersecting_Schoumaker_stall_groups <-
                        c(tmp_$intersecting_Schoumaker_stall_groups, list(NA))
                } else {
                    tmp_$intersecting_Schoumaker_stall_groups <-
                        c(tmp_$intersecting_Schoumaker_stall_groups,
                          list(unique(na.omit(schoumaker_stall_year_groups))))
                }
            }
            tmp_ <- list2DF(tmp_)

            ## Merge on to main data frame
            x <- base::merge(x, tmp_, all = TRUE, sort = FALSE)

            ## Create 'hash' to define table blocks
            x$hash_1 <- "No event"

            ## tfrSURFs
            idx <- !is.na(x[, "surf_year_group"])
            x[idx, "surf_tbl_block_type"] <- "SURF"
            x[idx, "hash_1"] <-
                paste0("SURF ", x[idx, "surf_year_group"])

            ## Schoumaker stalls
            idx <- !is.na(x[, "Schoumaker_stall_year_group"]) & is.na(x[, "surf_year_group"])
            x[idx, "surf_tbl_block_type"] <- "Schoumaker - only"
            x[idx, "hash_1"] <-
                paste0("Schoumaker ", x[idx, "Schoumaker_stall_year_group"])

            ## Roll any Schoumaker stalls that intersect tfrSURFs into that
            ## SURF's hash.
            for (j in unique(na.omit(x[, "surf_year_group"]))) {
                idx <- which(x[, "surf_year_group"] == j)
                int_Schoumaker <-
                    unique(unlist(x[idx, "intersecting_Schoumaker_stall_groups"]))
                if (any(!is.na(int_Schoumaker))) {
                    ## idx <- which(x[idx, "Schoumaker_stall_year_group"] %in% int_Schoumaker)
                    x[idx, "surf_tbl_block_type"] <- "SURF + Schoumaker"
                    x[idx, "hash_1"] <- paste0("SURF ", j, " + Schoumaker ", toString(int_Schoumaker))
                }
            }

            ## Mark any Schoumaker-only stall years that are part of a
            ## Schoumaker stall that intersects a SURF in other year(s).
            idx <- which(x[["surf_tbl_block_type"]] == "Schoumaker - only" &
                         (x[["Schoumaker_stall_year_group"]] %in%
                          x[["intersecting_Schoumaker_stall_groups"]]))
            x[idx, "surf_tbl_block_type"] <- "Schoumaker - intersecting"

            x <- x |>
                dplyr::mutate(hash_1 = paste(name, transition_condition_met, hash_1),
                                # Need level condition ^here because
                                # level condition could change part
                                # way through intervals of
                                # plateaus/stalls.
                              surf_tbl_block = 1)

        } else if (identical(table_type, "detailed")) {

            x <- x |>
                dplyr::mutate(hash_1 = paste(name, surf_year, Schoumaker_stall_type,
                                             transition_condition_met),
                                # Need level condition ^here because
                                # level condition could change part
                                # way through intervals of
                                # plateaus/surfs.
                              surf_tbl_block = 1)
        }

        x <- x[order(x$year), ]
        for (i in 2:nrow(x)) {
            if (identical(x[i, "hash_1"], x[i - 1, "hash_1"]))
                x[i, "surf_tbl_block"] <- x[i - 1, "surf_tbl_block"]
            else x[i, "surf_tbl_block"] <- x[i - 1, "surf_tbl_block"] + 1
        }

        ## x <- x |> dplyr::select(-starts_with("hash"))
        row.names(x) <- NULL

    } else {
        x <- x[NULL, ] #data frame with 0 rows
    }

    return(x)
}


##' Create main summary output table.
##'
##' Summarizes the results of the surf detection procedure in a
##' single table. The following columns are always present:
##' \code{c("area_name", "reg_name", "sub_saharan_africa", "name",
##' "surf_period", "TFR", "surf_year")}.
##'
##' If \code{table_type} is \code{"concise"}, the column "Schoumaker_stall_type"
##' is added to the output. Any Schoumaker (2019) stalls that intersect a SURF,
##' regardless of strength of evidence, are noted in this column for the
##' respective SURF. A Schoumaker stall my be noted in multiple rows if it
##' intersects multiple tfrSURFs.
##'
##' If \code{table_type} is \code{"detailed"}, the column
##' "Schoumaker_stall_type" is also added to the output, but tfrSURFs will
##' potentially be broken across multiple lines of the table if they intersect,
##' but not exactly, with a Schoumaker stall. Moreover, Schoumaker stalls are
##' split by strength of evidence.
##'
##' @param x A list of data frames as output by \code{\link{make_tfr_surfs}} or
##'     a data frame as output by \code{\link{make_tfr_surfs}}.
##' @param table_type Logical; cross-classify SURF periods by the TFR surf
##'     periods identified by \cite{Schoumaker (2019)}. This will add both
##'     columns and rows; see \dQuote{Details} for information.
##' @param digits Number of digits to round numeric columns to (e.g., applies to
##'     the TFR column). This is passed directly to \code{formatC(..., format = "f")}.
##' @inheritParams tabulate_surf_stats
##' @return A data frame.
##' @author Mark Wheldon
##'
##' @family Tabulation
##'
##' @references
##'
##' Schoumaker, B. (2019),
##' \dQuote{Stalls in Fertility Transitions in sub-Saharan Africa: Revisiting the Evidence},
##' Studies in Family Planning, 50, 257–278. \url{https://doi.org/10/gf5zpt}
##'
##' @export
tabulate_surf_periods <- function(x, ...) {
    UseMethod("tabulate_surf_periods")
}

##' @rdname tabulate_surf_periods
##' @export
tabulate_surf_periods.list <- function(x, incl_small_countries = TRUE,
                                       table_type = c("concise", "surfs only", "detailed"),
                                       incl_no_surfs = TRUE,
                                       flag_schoumaker_excl = TRUE,
                                       digits = 1) {

    stopifnot(is.logical(incl_small_countries))
    if (!incl_small_countries) x <- remove_small_countries(x)
    out <- do.call("rbind", lapply(x, FUN = function(z) {
        tmp_ <- tabulate_surf_periods(z, table_type = table_type,
                                      incl_no_surfs = incl_no_surfs,
                                      flag_schoumaker_excl = flag_schoumaker_excl,
                                      digits = digits)
        ## `tabulate_surf_periods.data.frame()` should have already sorted by
        ## start year of surf, so can use the row names to resort the combined
        ## output:
        tmp_[["sort_col"]] <- as.numeric(row.names(tmp_))
        return(tmp_)
    }))
    out <- out[order(out[["area_name"]], out[["reg_name"]], out[["name"]], out[["sort_col"]]), ]
    out[["sort_col"]] <- NULL
    row.names(out) <- NULL
    return(out)
}

##' @rdname tabulate_surf_periods
##' @export
tabulate_surf_periods.data.frame <- function(tfr_surfs_df,
                                             table_type = c("concise", "surfs only", "detailed"),
                                             incl_no_surfs = TRUE,
                                             flag_schoumaker_excl = TRUE,
                                             digits = 1) {

    ## -------* Set Up

    ## -------** Check args

    table_type <- match.arg(table_type)
    stopifnot(is.logical(incl_no_surfs))

    uq_cc <- unique(tfr_surfs_df[["country_code"]])
    if (length(uq_cc) > 1) {
        stop("'tfr_surfs_df' must have data for only one country; the following country codes are present: '",
             toString(uq_cc))
    }

    ## -------** Initialize Outputs

    tbl_no_value_string <- "---"

    loc_cols <- c("area_name", "reg_name", "sub_saharan_africa", "name")
    if (incl_no_surfs) loc_info <- tfr_surfs_df[1, loc_cols]

    out_cols <- character()
    if (identical(table_type, "surfs only")) {
        out_cols <- c(loc_cols, "surf_period", "TFR")
    } else {
        if (identical(table_type, "detailed")) {
            out_cols <- c(loc_cols, "surf_period", "TFR", "surf_year", "Schoumaker_stall_type")
        } else {
            if (identical(table_type, "concise")) {
                out_cols <- c(loc_cols, "surf_period", "TFR", "Schoumaker_stall_period")
            }
        }
    }

    out <- data.frame(lapply(setNames(nm = out_cols), function(z) character())) # Need to initialize this

    ## -------** Prepare Inputs

    ## -------* Make Table

    if (any(tfr_surfs_df[["surf_year"]] | tfr_surfs_df[["Schoumaker_stall_any"]])) {

        ## -------** Any tfrSURFs or Stalls

        tfr_surfs_df <- add_surf_periods_blocks(tfr_surfs_df, table_type = table_type)

        ## Subset, keeping only periods with a stall of either type.
        if (!identical(table_type, "surfs only")) {
            idx <- which(tfr_surfs_df[["surf_year"]] | tfr_surfs_df[["Schoumaker_stall_any"]])
            x <- tfr_surfs_df[idx, ]
        } else {
            x <- tfr_surfs_df[which(tfr_surfs_df[["surf_year"]]), ]
        }

        ## Keep only years that satisfy transition condition
        x <- x[x[["transition_condition_met"]], ]

        if (nrow(x)) {

            if (table_type %in% c("surfs only", "detailed")) {

                x <- x |>
                    dplyr::group_by(surf_tbl_block) |>
                    dplyr::mutate(surf_period =
                                      dplyr::case_when(identical(min(year), max(year)) ~ as.character(floor(min(year))),
                                                       TRUE ~ paste0(floor(min(year)),
                                                                     "-",
                                                                     floor(max(year)))),
                                  TFR =
                                      dplyr::case_when(identical(min(year), max(year)) ~ as.character(formatC(TFR_median, digits = digits, format = "f")),
                                                       TRUE ~ paste0(formatC(min(TFR_median), digits = digits, format = "f"),
                                                                     "-",
                                                                     formatC(max(TFR_median), digits = digits, format = "f")))) |>
                    dplyr::slice(1) |>
                    dplyr::ungroup()

                if (identical(table_type, "surfs only")) {
                    out <- as.data.frame(dplyr::select(x, c("area_name", "reg_name", "sub_saharan_africa", "name", "surf_period", "TFR")))

                } else if (identical(table_type, "detailed")) {
                    out <- as.data.frame(dplyr::select(x, c("area_name", "reg_name", "sub_saharan_africa", "name", "surf_period", "TFR",
                                                            "surf_year", "Schoumaker_stall_type")))
                }

            } else if (identical(table_type, "concise")) {

                x <- x |>
                    dplyr::group_by(surf_tbl_block) |>
                    dplyr::mutate(surf_period =
                                      dplyr::case_when(grepl("SURF", surf_tbl_block_type) & floor(min(year)) == floor(max(year)) ~ as.character(floor(min(year))),
                                                       grepl("SURF", surf_tbl_block_type) ~ paste0(floor(min(year)),
                                                                                                   "-",
                                                                                                   floor(max(year))),
                                                       TRUE ~ NA),
                                  TFR =
                                      dplyr::case_when(identical(min(year), max(year)) ~ as.character(formatC(TFR_median, digits = digits, format = "f")),
                                                       TRUE ~ paste0(formatC(min(TFR_median), digits = digits, format = "f"),
                                                                     "-",
                                                                     formatC(max(TFR_median), digits = digits, format = "f")))) |>
                    dplyr::slice(1) |>
                    dplyr::ungroup() |>
                    as.data.frame()

                x$Schoumaker_stall_period <- rep(NA, nrow(x))
                for (blk in x[x[, "surf_tbl_block_type"] == "SURF + Schoumaker", "surf_tbl_block"]) {
                    surf_period <- character()
                    evidence <- character()
                    blk_idx <- which(tfr_surfs_df[, "surf_tbl_block"] == blk)
                    for (j in unique(na.omit(unlist(tfr_surfs_df[blk_idx, "intersecting_Schoumaker_stall_groups"])))) {
                        tmp_ <- tfr_surfs_df[!is.na(tfr_surfs_df[, "Schoumaker_stall_year_group"]) &
                                              tfr_surfs_df[, "Schoumaker_stall_year_group"] == j, ]
                        if (nrow(tmp_)) {
                            for (k in unique(tmp_[, "Schoumaker_stall_type"])) {
                                tmp2_ <- tmp_[tmp_[, "Schoumaker_stall_type"] == k, ]
                                if (identical(min(tmp2_$year), max(tmp2_$year))) {
                                    surf_period <- c(surf_period, floor(min(tmp2_$year)))
                                } else {
                                    surf_period <-
                                        c(surf_period,
                                          paste0(floor(min(tmp2_$year)), "-",
                                                 floor(max(tmp2_$year))))
                                }
                                evidence <- c(evidence, unique(tmp2_$Schoumaker_stall_type))
                            }
                        }
                    }

                    x[x[, "surf_tbl_block"] == blk, "Schoumaker_stall_period"] <-
                        paste(paste0(surf_period, " (", evidence, ")"), collapse = "; ")
                }

                for (i in which(x[, "surf_tbl_block_type"] == "Schoumaker - only")) {
                    surf_period <- character()
                    evidence <- character()
                    tmp_ <-
                        tfr_surfs_df[!is.na(tfr_surfs_df[, "Schoumaker_stall_year_group"]) &
                                      tfr_surfs_df[, "Schoumaker_stall_year_group"] == x[i, "Schoumaker_stall_year_group"], ]
                    if (nrow(tmp_)) {
                        for (k in unique(tmp_[, "Schoumaker_stall_type"])) {
                            tmp2_ <- tmp_[tmp_[, "Schoumaker_stall_type"] == k, ]
                            if (identical(min(tmp2_$year), max(tmp2_$year))) {
                                surf_period <- c(surf_period, tail(start_year, 1))
                            } else {
                                surf_period <-
                                    c(surf_period,
                                      paste0(floor(min(tmp2_$year)), "-",
                                             floor(max(tmp2_$year))))
                            }
                            evidence <- c(evidence, unique(tmp2_$Schoumaker_stall_type))
                        }
                    }
                    x[i, "Schoumaker_stall_period"] <-
                        paste(paste0(surf_period, " (", evidence, ")"), collapse = "; ")
                }

                x <- x[!x[["surf_tbl_block_type"]] == "Schoumaker - intersecting", ]

                x[is.na(x[["surf_period"]]), "surf_period"] <- tbl_no_value_string

                x <- x[order(x[["year"]]), ]
                row.names(x) <- NULL
            }
            out <- as.data.frame(x[, out_cols])

            ## Replace 'NA's with string
            col_idx <- which(colnames(out) %in% c("Schoumaker_stall_type", "Schoumaker_stall_period"))
            if (length(col_idx)) {
                for (j in col_idx) {
                    out[is.na(out[, j]), j] <- tbl_no_value_string
                }
            }
        }
    } else {

        ## -------** NO tfrSURFs, NO Stalls

        if (incl_no_surfs) {
            out <- tfr_surfs_df[1, loc_cols]
            other_cols <-
                c("surf_period", "TFR", "surf_year", "Schoumaker_stall_period", "Schoumaker_stall_type")
            idx <- which(other_cols %in% out_cols)
            out[, other_cols[idx]] <- rep(tbl_no_value_string, length(idx))
        }
    }

    ## -------** Schoumaker Excluded

    if (flag_schoumaker_excl) {
        if (nrow(out) && "Schoumaker_stall_period" %in% colnames(out) &&
            any(!tfr_surfs_df[["Schoumaker_included"]])) {
            out[["Schoumaker_stall_period"]] <- "(not included)"
        }
    }

    return(out)
}

