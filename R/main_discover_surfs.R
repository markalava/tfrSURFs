################################################################################
###
### These are the main user-level functions for probabilistic identification of
### TFR surfs. They are all EXPORTED.
###
################################################################################


##' Identify TFR surfs by country.
##'
##' This function will loop over all countries given by argument
##' \code{country_codes} (in parallel if \code{ncores} is not \code{NULL}). If
##' \code{country_codes} has only one element, a data frame will be returned,
##' otherwise a list of data frames, one for each country. If
##' \code{country_codes} is \code{NULL}, all countries in \code{\link{iso_all}}
##' will be included. A completed model run from \pkg{bayesTFR} is required. Use
##' \code{sim.dir} to point to the results directory on disc. If
##' \code{country_codes} references any countries that are not in the results
##' pointed to by \code{sim.dir} an error will be signalled.
##'
##' Argument \code{ncores} controls parallelization. If \code{country_codes} has
##' only one element, operations will be parallelized over the MCMC trajectories
##' in \code{sim.dir}, otherwise over countries. If \code{ncores < 0} and
##' package \pkg{parallelly} is installed, \code{ncores} will be reset to
##' \code{parallelly::availableCores(omit = -ncores)}.
##'
##' This function is based on \code{\link[FPPlateaus]{make_all_results}},
##' \code{\link[FPPlateaus]{make_stall_prob_df}},
##' \code{\link[FPPlateaus]{add_plateau_lengths}}, and
##' \code{\link[FPPlateaus]{add_schoumaker_tfr_stalls}}.
##'
##' @param country_codes Vector of 3-digit numeric country code(s). Will be
##'     coerced to numeric via \code{\link{as.numeric}}. Must be in
##'     \code{\link{iso_all}$iso} and \code{sim.dir}.
##' @param sim.dir Directory with the MCMC simulation results; see
##'     \code{\link{bayesTFR::get.tfr.mcmc}}.
##' @param median_only Logical; should all analysis be performed using only the
##'     posterior medians? If \code{FALSE} (default), all trajectories are used
##'     and the SURF analysis is probabilistic.
##' @param transition_condition_type Character string; key word giving the level
##'     condition to apply.
##' @param rate_threshold Rate condition; any smoothed estimates of annual
##'     change \emph{greater} than \code{rate_threshold} will be identifed as a
##'     surf.
##' @param rate_prob_threshold Probability condition (a real number between 0
##'     and 1).
##' @param exceedance_condition Only surfs where the maximum TFR is at least
##'     \code{exceedance_crition} with probability
##'     \code{exceedance_condition_prob_threshold} will be kept.
##' @param exceedance_condition_prob_threshold Probability threshold applied to
##'     \code{exceedance_condition}.
##' @param min_surf_length Positive integer; minimum length (years) of any surf.
##' @param min_inter_surf_length Positive integer; minimum length (years) of any
##'     inter-surf period.
##' @param continuation_condition Character; keyword that specifies
##'     additional conditions required for a surf to end.
##' @param continuation_condition_prob_threshold Probability with which TFR must
##'     return to level at beginning of surf; only relevant if
##'     \code{continuation_condition} = \code{"Regain TFR, probabilistic"}.
##' @param smoothing_method Character string giving type of local smoothing to
##'     use.
##' @param bandwidth Positive, odd integer; smoothing bandwidth.
##' @param year_lim Vector, length 2, giving the start and end years (inclusive)
##'     defining the interval within which to limit the analysis. Stalls outside
##'     this period will not be included.
##' @param incl_small_countries Logical; include countries with populations less
##'     than 90000 in mid-2024?
##' @param ncores Number of cores to use to run in parallel; see
##'     \dQuote{Details}.
##'
##' @return A data frame if \code{country_codes} has only one element, otherwise
##'     a list, with one element per country code. See the dataset
##'     \code{link{output_column_definitions}} for the column definitions.
##'
##' @author Mark Wheldon
##'
##' @seealso \code{\link{iso_all}}, the \pkg{FPPlateaus} package
##'     (https://github.com/markalava/FPPlateaus),
##'     \code{\link{output_column_definitions}}
##'
##' @family Main TFR surfs functions
##'
##' @export
make_tfr_surfs <- function(country_codes = NULL,
                            sim.dir = getOption("tfrSURFs.sim.dir"),
                            median_only = FALSE,
                            transition_condition_type = c("Phase II & >= 2.1 persistently",
                                                     "Phase II & never < 2.1",
                                                     "Phase II only"),
                            smoothing_method = c("local_linear", "annual_difference"),
                            bandwidth = 3,
                            rate_threshold = -0.01,
                            rate_prob_threshold = 0.8,
                            continuation_condition = c("Regain TFR, median, 2yr",
                                                       "Regain TFR, probabilistic",
                                                       "NONE"),
                            continuation_condition_prob_threshold = 0.8,
                            exceedance_condition = c("Max TFR > 2.1, median",
                                                     "Max TFR > 2.1, probabilistic",
                                                     "NONE"),
                            exceedance_condition_prob_threshold = 0.8,
                            min_surf_length = 2,
                            min_inter_surf_length = 2,
                            year_lim = c(1950, 2050),
                            incl_small_countries = FALSE,
                            ncores = getOption("cl.cores", -2)) {

    verbose <- getOption("tfrSURFs.verbose")

    ## -------*  Check inputs

    stopifnot(dir.exists(sim.dir))

    ## -------** Country Codes

    country_codes <- validate_country_codes(country_codes, sim.dir)

    ## Small countries
    if (!incl_small_countries) country_codes <- remove_small_countries(country_codes)

    ## -------** The Rest

    stopifnot(is.logical(median_only))
    transition_condition_type <- match.arg(transition_condition_type)
    stopifnot(is.numeric(rate_threshold))
    stopifnot(is.numeric(rate_prob_threshold) && rate_prob_threshold >= 0 && rate_prob_threshold <= 1)
    if (!is.wholenumber(min_surf_length)) {
        min_surf_length <- as.integer(min_surf_length)
        warning("'min_surf_length' is not a whole number; it has been set to '", min_surf_length, "'.")
    }
    exceedance_condition <- match.arg(exceedance_condition)
    continuation_condition <- match.arg(continuation_condition)
    if (!(bandwidth %% 2) > 0) stop("'bandwidth' must be an odd number.")
    smoothing_method <- match.arg(smoothing_method)
    stopifnot(is.numeric(year_lim) && year_lim[1] <= year_lim[2])
    if (!is.null(ncores)) stopifnot(is.wholenumber(ncores))

    ## ............................................................

    ## -------* MULTIPLE COUNTRIES (Early Return)

    if (length(country_codes) > 1) {

        if (verbose) {
            tfr_est <- bayesTFR::get.tfr.mcmc(sim.dir = sim.dir)
            message("bayesTFR.mcmc.set object has a total of '",
                    bayesTFR::get.stored.mcmc.length(tfr_est[["mcmc.list"]]),
                    "' stored iterations.")
            tfr_pred <- bayesTFR::get.tfr.prediction(sim.dir = sim.dir)
            message("bayesTFR.prediction object has '", tfr_pred[["nr.traj"]],
                    "' trajectories (burnin = '", tfr_pred[["burnin"]], "', thin = '", tfr_pred[["thin"]], "').")
            rm(tfr_est, tfr_pred)
        }

        cl <- set_cores_make_cluster(ncores = ncores, maxjobs = length(country_codes))

        args_list <- names(formals(make_tfr_surfs))
        args_list <- args_list[!(args_list %in% c("country_codes", "ncores"))]
        args_list <- lapply(setNames(nm = args_list), function(z) z = get(z))

        if (!is.null(cl)) {
            parallel::clusterExport(cl = cl,
                                    varlist = c("UNlocations_countries", "schoumaker_2019_tfr_stalls"),
                                    envir = environment())
            on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
        }

        ## vvv EARLY RETURN vvv
        return(pbapply::pblapply(setNames(nm = country_codes),
                                 function(z, args_list) {
                                     err <- try(do.call("make_tfr_surfs",
                                                        args = c(list(country_codes = z, ncores = NULL), args_list)))
                                     if (!inherits(err, "try-error")) return(err)
                                     else stop("Error at country '", z, " (", get_country_names(z), ")'")
                                 }, args_list = args_list,
                                 cl = cl))
        ## ^^^ EARLY RETURN ^^^
    }

    ## ............................................................

    ## -------* MAIN PART that Creates tfrSURFs

    ## -------** Create Main Data Frame

    ## Trajectories from bayesTFR
    ## year limits are applied here.
    traj <- make_tfr_estproj_traj(country = country_codes, sim.dir = sim.dir,
                                  year_lim = year_lim,
                                  out_format = "FPPlateaus")

    ## Quantiles
    surfs_df <-
        as.data.frame(aperm(
            apply(traj[["arr"]], 1:2, "quantile", probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE),
            c(2, 3, 1)))
    rownames(surfs_df) <- NULL
    colnames(surfs_df) <-
        gsub(pattern = ".50%", "_median", colnames(surfs_df), fixed = TRUE)
    colnames(surfs_df) <-
        gsub(pattern = "(\\.[0-9]+)%", "\\1pc", colnames(surfs_df), fixed = FALSE)

    surfs_df[["country_code"]] <- country_codes
    surfs_df[["year"]] <- as.numeric(dimnames(traj[["arr"]])[[1]])
    surfs_df[["indicator"]] <- "TFR" # Need this to make merges work well

    ## Meta columns
    surfs_df <- data.frame(surfs_df, traj[["meta"]])

    ## Keep sorted by year
    surfs_df <- surfs_df[order(surfs_df[["year"]]), ]

    ## -------** Median Only

    ## If 'median_only = TRUE', overwrite the 'traj' with a matrix containing
    ## only the medians.
    if (median_only) {
        traj[["arr"]] <- array(surfs_df[["TFR_median"]],
                               dim = c(dim(traj[["arr"]])[1:2], 1),
                               dimnames = dimnames(traj[["arr"]]))

        if (!identical(continuation_condition, "Regain TFR, median, 2yr")) {
            continuation_condition <- "Regain TFR, median, 2yr"
            warning("'median_only' is 'TRUE'; 'continuation_condition' has been set to 'Regain TFR, median, 2yr'.")
        }
        if (!identical(exceedance_condition, "Max TFR > 2.1, median")) {
            exceedance_condition <- "Max TFR > 2.1, median"
            warning("'median_only' is 'TRUE'; 'exceedance_condition' has been set to 'Max TFR > 2.1, median'.")
        }
    }

    ## -------** Apply the Conditions

    ## -------*** Rate and Probability Conditions

    surfs_df <-
        apply_probability_condition(x = surfs_df, traj = traj[["arr"]],
                                    smoothing_method = smoothing_method,
                                    bandwidth = bandwidth,
                                    rate_threshold = rate_threshold,
                                    rate_prob_threshold = rate_prob_threshold)

    ## -------*** Transition condition

    surfs_df <-
        apply_transition_condition(x = surfs_df,
                                   transition_condition_type = transition_condition_type,
                                   country_codes = country_codes, sim.dir = sim.dir)

    ## -------*** Min Inter-Stall Lengths (First Pass)

    ## Fill in any single-year breaks before removing any single-year tfrSURFs.

    surfs_df <- apply_min_inter_surf_lengths(x = surfs_df,
                                               min_inter_surf_length = min_inter_surf_length)

    ## -------*** Min Stall Lengths (First Pass)

    ## Finally, remove any remaining single-year tfrSURFs.

    surfs_df <- apply_min_surf_lengths(x = surfs_df,
                                         min_surf_length = min_surf_length)

    ## -------*** Tidy Columns

    ## Remove length-related columns and re-name logging columns.

    surfs_df <- remove_length_cols(surfs_df)

    colnames(surfs_df)[colnames(surfs_df) == "min_inter_surf_length_surf_year"] <-
        "min_inter_surf_length_surf_year_pre_continuation"
    colnames(surfs_df)[colnames(surfs_df) == "min_surf_length_surf_removed"] <-
        "min_surf_length_surf_removed_pre_continuation"

    ## -------*** Continuation Condition

    ## Apply this condition before requiring minimum lengths. This condition
    ## tends to increase surf lengths. If minimum lengths are applied first,
    ## some surfs might be filtered out prematurely.

    surfs_df <- apply_continuation_condition(x = surfs_df, traj = traj[["arr"]],
                                              continuation_condition = continuation_condition,
                                              continuation_condition_prob_threshold = continuation_condition_prob_threshold,
                                              ncores = if (dim(traj[["arr"]])[3] > 5e4) { ncores } else { NULL })

    ## -------*** Exceedance Condition

    ## Apply this next to temper any effects on continuation condition at low
    ## TFRs.

    surfs_df <- apply_exceedance_condition(x = surfs_df, traj = traj[["arr"]],
                                            exceedance_condition = exceedance_condition,
                                            exceedance_condition_prob_threshold = exceedance_condition_prob_threshold,
                                            ncores = if (dim(traj[["arr"]])[3] > 5e4) { ncores } else { NULL })

    ## -------*** Min Inter-Stall Lengths (Second Pass)

    ## Fill in any single-year breaks before removing any single-year tfrSURFs.

    surfs_df <- apply_min_inter_surf_lengths(x = surfs_df,
                                               min_inter_surf_length = min_inter_surf_length)

    ## -------*** Min Stall Lengths (Second Pass)

    ## Finally, remove any remaining single-year tfrSURFs.

    surfs_df <- apply_min_surf_lengths(x = surfs_df,
                                         min_surf_length = min_surf_length)

    ## -------* Extra Columns

    ## -------** Estimation or Projection Period Indicator

    surfs_df <-
        add_surf_in_proj(surfs_df,
                          last_est_year = surfs_df[1, "bayesTFR_present_year"],
                          proj_split = "by_surf")

    ## -------** Extra Columns and Tidy up

    surfs_df <- data.frame(surfs_df,
                            year_lim_start = year_lim[1], year_lim_end = year_lim[2])

    ## Schoumaker surfs
    surfs_df <- add_schoumaker_tfr_stalls(x = surfs_df)

    ## Geographic regions
    nr <- nrow(surfs_df)
    surfs_df <- base::merge(surfs_df,
                             UNlocations_countries,
                             all.x = TRUE,
                             by = "country_code")
    if (!identical(nrow(surfs_df), nr))
        stop("Merging with 'UNlocations_countries' went wrong somewhere.")

    ## -------*  FINISH

    ## Put columns in a sensible order
    cols_in_order <- c(
        ## Basic ID
        "country_code", "name", "year",

        ## SURF indicators
        "transition_condition_met",
        "surf_prob", "surf_year", "rate_prob_surf_year",
        "min_inter_surf_length_surf_year_pre_continuation",
        "min_surf_length_surf_removed_pre_continuation",
        "continuation_surf_year",
        "exceedance_condition_surf_removed",
        "min_inter_surf_length_surf_year",
        "min_surf_length_surf_removed",

        ## SURF periods/groups
        "surf_year_start", "surf_year_start_pre_continuation",
        "surf_year_group", "surf_year_len",
        "surf_in_proj",

        ## Schoumaker stalls
        "Schoumaker_stall_type", "Schoumaker_stall_strong",
        "Schoumaker_stall_moderate", "Schoumaker_stall_weak",
        "Schoumaker_stall_pretransitional", "Schoumaker_stall_none",
        "Schoumaker_stall_any",
        "Schoumaker_included",

        ## Indicator info
        "indicator", "TFR.2.5pc",
        "TFR.10pc", "TFR_median", "TFR.90pc", "TFR.97.5pc",

        ## Other country info
        "reg_name", "area_name", "sub_saharan_africa",
        "pop_lt_90k_2024",

        ## Condition settings (same in every row)
        "smoothing_method", "transition_condition_type",
        "bandwidth", "rate_threshold", "rate_prob_threshold", "continuation_condition",
        "continuation_condition_prob_threshold", "exceedance_condition", "exceedance_condition_prob_threshold",
        "min_inter_surf_length", "min_surf_length",

        ## Other info (same in every row)
        "bayesTFR_present_year", "bayesTFR_wpp_year",
        "year_lim_start", "year_lim_end")

    ## Keep only those columns that exist in _surfs_df'
    cols_in_order <- intersect(cols_in_order, colnames(surfs_df))

    ## Add any columns that were missed
    cols_in_order <- c(cols_in_order, setdiff(colnames(surfs_df), cols_in_order))

    ## DONE
    return(data.frame(surfs_df[order(surfs_df[["year"]]), cols_in_order]))
}



