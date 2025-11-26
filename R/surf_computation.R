################################################################################
###
### NON-exported utility functions for creating TFR surf estimates and objects.
###
### **Most (probably all) of these functions are designed to work on data frames
### **with results for _ONE_ country only.
###
################################################################################

###-----------------------------------------------------------------------------
### * TFR Projection

##' Get years and TFRs that start phase II and phase III
##'
##' The definitions of phase II and phase III are given in the
##' documentation and references to \pkg{bayesTFR}. This extracts the
##' start years and TFRs for the phases from the meta data stored with
##' a \pkg{bayesTFR} model run. This function will return \code{NA} as
##' the start year of phase III and the TFR if phase III is not
##' entered during the observation period.
##'
##' @inheritParams bayesTFR::get.tfr.estimation
##' @param plot Logical; create a plot of TFR?
##' @return A 2x2 matrix giving years and TFRs at the start of phase II and phase III.
##' @author Mark Wheldon
##' @keywords internal
##' @noRd
get_phase_starts <- function(mcmc.list = NULL, country = NULL, sim.dir = NULL, plot = FALSE) {

    ## -------* Get objects

    if (is.null(mcmc.list)) mcmc.list <- bayesTFR::get.tfr.mcmc(sim.dir = sim.dir)

    c_idx_in_tfr_mcmc <-
        which(as.numeric(dimnames(mcmc.list[["meta"]]$tfr_matrix)[[2]]) == country)
    if (!length(c_idx_in_tfr_mcmc)) stop("'country' '", country, "' is not included in the bayesTFR output.")

    tfr_quantile <-
        bayesTFR::get.tfr.estimation(mcmc.list = mcmc.list,
                                     country = country, probs = 0.5)$tfr_quantile
    year_vec <- tfr_quantile[["year"]]


    ## -------*  Phase II

    tau_c <- mcmc.list[["meta"]]$tau_c[c_idx_in_tfr_mcmc]
                                # tau_c is an index into year_vec

    ## Before start of observation period
    if (identical(as.integer(tau_c), as.integer(-1))) {
        tau_c <- 1 # set to first year of observation
    }

    f_II <- tfr_quantile$V1[tau_c]
    tau_c <- year_vec[tau_c]


    ## -------* Phase III

    ## Some countries are observed to enter phase III, others might enter
    ## it during projection. The definition is different in each
    ## case. \code{lambda_c} in the meta list is a vector of indices into
    ## the year vector, pointing to the start year of phase III if in the
    ## estimation period, otherwise the last observation year.

    lambda_c <- mcmc.list[["meta"]]$lambda_c[c_idx_in_tfr_mcmc]
                                # lambda_c is an index into year_vec
    if (identical(as.numeric(lambda_c), as.numeric(length(year_vec)))) {
        f_III <- lambda_c <- NA
    } else {
        f_III <- tfr_quantile$V1[lambda_c]
        lambda_c <- year_vec[lambda_c]
    }


    ## -------* Finish

    out <- matrix(c(tau_c, lambda_c, f_II, f_III), ncol = 2,
                  dimnames = list(phase = c("II", "III"),
                                  quantity = c("year", "TFR")))

    if (plot) {
        plot(tfr_quantile[["year"]], tfr_quantile$V1, type = "b",
             main = paste0("Country code ", country))
        abline(v = tau_c, lty = 2, col = "blue")
        abline(v = lambda_c, lty = 2, col = "blue")
        abline(h = 2.1, lty = 3, col = "grey")
        return(invisible(out))
    } else {
        return(out)
    }
}


##' Combine estimate and projection trajectories
##'
##' Joins the posterior trajectories from the estimation and projection steps of
##' \pkg{bayesTFR}.
##'
##' @inheritParams make_tfr_surfs
##' @param country Passed to \code{\link{bayesTFR::get.tfr.estimation}}.
##' @param out_format How to format the output.
##' @return Array of trajectories.
##' @author Mark Wheldon
##' @keywords internal
##' @noRd
make_tfr_estproj_traj <- function(country, sim.dir, year_lim, out_format = c("bayesTFR", "FPPlateaus")) {

    verbose <- getOption("tfrSURFs.verbose")

    out_format <- match.arg(out_format)
    if (!dir.exists(sim.dir)) stop("'sim.dir' does not exist.")
    if (!length(dir(sim.dir))) stop("'sim.dir' is empty.")
    if (!bayesTFR::has.tfr.prediction(sim.dir)) stop("'sim.dir' does not have predictions; use 'tfr.predict'.")

    ## Load predictions reference from disc
    tfr.pred <- bayesTFR::get.tfr.prediction(sim.dir = sim.dir)

    ## Get estimates trajectories
    tfr.est <-
        bayesTFR::get.tfr.estimation(mcmc.list = tfr.pred[["mcmc.set"]], country = country, probs = 0.5)

    ## Get projections trajectories
    tfr.traj <- bayesTFR::get.tfr.trajectories(tfr.pred, country = country)

    if (!identical(dim(tfr.est[["tfr_table"]])[1], dim(tfr.traj)[2]))
        stop("'tfr.est' and 'tfr.traj' don't have the same number of iterations.")

    ## Merge. Creates a matrix with rows = trajectories, col = years.
    ## [Re-purpose name 'tfr.traj']
    tfr.traj <- cbind(matrix(tfr.est[["tfr_table"]], nrow = nrow(tfr.est[["tfr_table"]]),
                              ncol = ncol(tfr.est[["tfr_table"]]),
                              dimnames = list(NULL, tfr.est[["tfr_quantile"]]$year)),
                       t(tfr.traj))

    dup_years <- which(duplicated(dimnames(tfr.traj)[[2]]))
    if (length(dup_years)) tfr.traj <- tfr.traj[, -dup_years]

    ## Apply year limits
    tfr.traj <- tfr.traj[, as.character(year_lim[1]:year_lim[2])]

    ## Format and exit
    if (identical(out_format, "bayesTFR")) {
        return(tfr.traj)
    } else {
        ## Put in FPPlateuas format: 3D array with rows = years, cols =
        ## indicator, slides = trajectories.
        meta_items <- c("present.year", "wpp.year")
        return(list(arr = aperm(array(tfr.traj, dim = c(nrow(tfr.traj), ncol(tfr.traj), 1),
                           dimnames = list(rownames(tfr.traj), colnames(tfr.traj), "TFR")),
                           perm = c(2, 3, 1)),
                    meta = setNames(bayesTFR::get.mcmc.meta(tfr.pred)[meta_items],
                                    nm = paste0("bayesTFR_", gsub("\\.", "_", meta_items)))
                    ))
    }
}


## Takes the 'phase_starts' data and a vector of years, returns a logical
## vector the same length of years indicating which ones are strictly in
## Phase III (assuming 'years' has no NAs)
is_in_phaseIII <- function(years, phase_starts) {
    if (any(is.na(years))) warning("'years' contains NAs; 'is_in_phaseIII()' may not work as intended.")
    if (!is.na(phase_starts["III", "year"])) {
        return(years >= phase_starts["III", "year"])
    } else return(rep_len(FALSE, length(years)))
}


## Takes the 'phase_starts' data and a vector of years, returns a logical
## vector the same length of years indicating which ones are strictly in
## Phase II (assuming 'years' has no NAs)
is_in_phaseII <- function(years, phase_starts) {
    if (any(is.na(years))) warning("'years' contains NAs; 'is_in_phaseII()' may not work as intended.")
    if (!is.na(phase_starts["II", "year"])) {
        return(years >= phase_starts["II", "year"] & !is_in_phaseIII(years, phase_starts))
    } else return(rep_len(FALSE, length(years)))
}


## apply_fn_to_traj <- function(fn, traj, ...)


##     if (is.array(traj)) {
##         ## Must be in the format produced by 'make_tfr_estproj_traj(...,
##         ## out_format = "FPPlateaus")'.
##         stopifnot(identical(length(dim(traj)), 3L))
##         stopifnot(identical(dim(traj)[2], 1L))
##         if (!is.null(dimnames(traj)))
##             stopifnot(identical(dimnames(traj)[[2]], "TFR"))

##         cl <- set_cores_make_cluster(ncores = ncores, maxjobs = dim(traj)[3])
##         if (!is.null(cl)) {
##             parallel::clusterExport(cl = cl, fn, envir = environment())
##             on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
##         }

##         ## Turns 'traj' into a logical array of same dimension, each element
##         ## indicating whether traj satisfies the condition.
##         traj <- pbapply::pbapply(X = traj, MARGIN = c(2, 3),
##                                 FUN = function(z, sys) {
##                                     do.call(fn_, args = , traj = z)
##                                 }, sys = surf_year_start,
##                                 fn_ = fn, ...,
##                                 cl = cl)

##         return(rowMeans(traj[, 1, , drop = FALSE]))
##     }


###-----------------------------------------------------------------------------
### * Level Condition

##' Add level condition indicator to surfs data frame
##'
##' Takes \code{x = surfs_df} and adds logical column \code{"transition_condition_met"}.
##'
##' @inheritParams make_tfr_surfs
##' @param x \code{surfs_df} data frame.
##' @return \code{x} is returned with \code{"transition_condition_met"} column added.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
apply_transition_condition <- function(x, transition_condition_type, country_codes, sim.dir) {

    ## bayesTFR phases
    phase_starts <- get_phase_starts(country = country_codes, sim.dir = sim.dir)

    ## -------* Determine Level Condition Periods

    ## Phase II years as a logical index
    in_pII_tf <- is_in_phaseII(x[["year"]], phase_starts)

    ## Phase III years as a logical index
    in_pIII_tf <- is_in_phaseIII(x[["year"]], phase_starts)

    if (identical(transition_condition_type, "Phase II only")) {

         x[["transition_condition_met"]] <- in_pII_tf

    } else if (identical(transition_condition_type, "Phase II & never < 2.1")) {

        if (any(in_pII_tf)) {
            pII_TFR_lt_2.1_tf <- in_pII_tf & x[["TFR_median"]] < 2.1
            if (any(pII_TFR_lt_2.1_tf)) {
                pII_TFR_lt_2.1_first_time_i <- min(which(pII_TFR_lt_2.1_tf))
                x[["transition_condition_met"]] <- in_pII_tf &
                    !(1:nrow(x) >= pII_TFR_lt_2.1_first_time_i)
            } else {
                x[["transition_condition_met"]] <- in_pII_tf
            }
        } else x[["transition_condition_met"]] <- FALSE

    } else if (identical(transition_condition_type, "Phase II & >= 2.1 persistently")) {

        if (any(in_pII_tf)) {
            pII_TFR_gt_2.1_tf <- in_pII_tf & x[["TFR_median"]] >= 2.1
            if (any(pII_TFR_gt_2.1_tf)) {
                pII_TFR_lt_2.1_last_time_i <- max(which(pII_TFR_gt_2.1_tf))
                x[["transition_condition_met"]] <- in_pII_tf &
                    !(1:nrow(x) > pII_TFR_lt_2.1_last_time_i)
            } else {
                x[["transition_condition_met"]] <- in_pII_tf
            }
        } else x[["transition_condition_met"]] <- FALSE

    } else {
        stop("Something wrong with argument 'transition_condition_type'.")
    }

    ## -------* Apply tfrSURFs

    x[, "surf_year"] <- x[["surf_year"]] & x[["transition_condition_met"]]

    ## END
    return(data.frame(x,
                      transition_condition_type = transition_condition_type))
}


###-----------------------------------------------------------------------------
### * Rate Condition

##' Local linear smoothing
##'
##' The subfunction that does the local linear smoothing.
##'
##' @param x \code{surfs_df} data frame.
##' @inheritParams make_tfr_surfs
##' @param return_value Return the regression coefficients (betas) or the predicted values (Y-hats)?
##' @return Array.
##' @author Mark Wheldon
##' @keywords internal
##' @noRd
lm_local_arr <- function(x, bandwidth, return_value = c("beta", "Y_hat")) {

    stopifnot((bandwidth %% 2) > 0)
    return_value <- match.arg(return_value, several.ok = TRUE)
    if (identical(sort(return_value), sort(c("beta", "Y_hat")))) return_value <- "both"

    X <- matrix(c(rep(1, bandwidth), scale(1:bandwidth, scale = FALSE)), ncol = 2)
    window_edge <- bandwidth - 1 # years that we can't produce betas for
    Y_idx <- rep(1:(dim(x)[1] - (window_edge)), each = bandwidth) +
        rep(0:(bandwidth - 1), length.out = bandwidth * (dim(x)[1] - window_edge))
                                # Y_idx is used to select the
                                # 'bandwidth'-year-long windows of
                                # trajectory values

    lm_local <- function(z, model_matrix, Y_idx, bandwidth, return_value) {
        Y <- cbind(matrix(NA, nrow = bandwidth, ncol = window_edge / 2),
                   matrix(z[Y_idx], nrow = bandwidth, ncol = length(z) - window_edge),
                   matrix(NA, nrow = bandwidth, ncol = window_edge / 2))
                                # 'Y' is a matrix with ncol = number
                                # of years, nrow = bandwidth. Each col
                                # has the trajectory values ('z') for
                                # the 'bandwidth'-year-long window for
                                # that colum's year. E.g., if
                                # bandwidth is 3, col 2 has trajectory
                                # values for year 1, year 2, year 3; a
                                # three-year window centred at year 2.
        beta <- solve(t(model_matrix) %*% model_matrix) %*% t(model_matrix) %*% Y
        if (identical(return_value, "beta")) return(beta[2,])
        else {
            Y_hat <- (model_matrix %*% beta)
            if (identical(return_value, "Y_hat")) return(Y_hat[2,])
            else return(cbind(beta = beta[2,], Y_hat = Y_hat[2,]))
        }
    }

    apply_lm_local <- function(x, model_matrix, Y_idx, bandwidth, return_value) {
        apply(X = x, MARGIN = 2, FUN = "lm_local", model_matrix = model_matrix,
              Y_idx = Y_idx, bandwidth = bandwidth, return_value = return_value)
    }

    if (!identical(return_value, "both")) {
        return(apply_lm_local(x = x, model_matrix = X, Y_idx = Y_idx,
                              bandwidth = bandwidth, return_value = return_value))
    } else {
        return(array(apply_lm_local(x = x, model_matrix = X, Y_idx = Y_idx,
                                    bandwidth = bandwidth, return_value = return_value),
                     dim = c(dim(x)[1], 2, dim(x)[2]),
                     dimnames = list(year = dimnames(x)[[1]],
                                     parameter = c("beta", "Y_hat"),
                                     traj = 1:dim(x)[2])))
    }
}


###-----------------------------------------------------------------------------
### * Probability + Rate Condition

##' Apply the rate and probability conditions.
##'
##' \code{x} is returned with surf columns added.
##'
##' @inheritParams make_tfr_surfs
##' @param x \code{surfs_df}
##' @param traj Array of posterior TFR trajectories.
##' @return \code{x} is returned with appropriate columns added.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
apply_probability_condition <- function(x, traj, smoothing_method, bandwidth, rate_threshold, rate_prob_threshold) {

    ## Local smoothing
    if (identical(smoothing_method, "annual_difference")) {
        ## Annual differences
        traj_diffs <-
                apply(traj, MARGIN = 2:3, "diff",
                              differences = 1, lag = bandwidth,
                              simplify = TRUE)
    } else if (identical(smoothing_method, "local_linear")) {
        ## Local linear smooth
        traj_diffs <-
            aperm(array(apply(traj, MARGIN = 2, "lm_local_arr",
                              bandwidth = bandwidth, return_value = "beta",
                              simplify = TRUE),
                        dim = c(dim(traj)[1], dim(traj)[3], dim(traj)[2]),
                        dimnames = list(dimnames(traj)[[1]], dimnames(traj)[[3]],
                                        dimnames(traj)[[2]])),
                  c(1,3,2))
    }

    ## ## TESTING ONLY: plot 'diffs'
    ## matplot(x = as.numeric(dimnames(traj_diffs)[[1]]), y = traj_diffs[,1,],
    ##         ylab = "diff", xlab = "year", type = "l", lty = 1, col = "grey")

    ## -------* Probability

    ## Indicators -> probabilities
    traj_probs <- apply(traj_diffs > rate_threshold, 1:2, "mean")
    x <-
        base::merge(x,
                    data.frame(year = as.numeric(rownames(traj_probs)),
                               tidyr::gather(as.data.frame(traj_probs),
                                             key = "indicator",
                                             value = "surf_prob")),
                    all = TRUE)

    ## Keep sorted by year
    x[order(x[["year"]]), ]

    ## Stall probabilities.
    x[["surf_year"]] <- FALSE
    rate_prob_surf_idx <- which(!is.na(x[["surf_prob"]]) & (x[["surf_prob"]] >= rate_prob_threshold))
    x[rate_prob_surf_idx, "surf_year"] <- TRUE

    ## Record surfs generated using this condition only
    x$rate_prob_surf_year <- FALSE
    x[rate_prob_surf_idx, "rate_prob_surf_year"] <- TRUE

    ## -------* END

    return(data.frame(x,
                      smoothing_method = smoothing_method,
                      bandwidth = bandwidth,
                      rate_threshold = rate_threshold,
                      rate_prob_threshold = rate_prob_threshold))
}


###-----------------------------------------------------------------------------
### * Stall Length Condition

##' Calculate length of surf periods
##'
##' Used after surf probabilities have been calculated and level condition
##' applied. Calculates the length of surfs. Derived from
##' \code{\link{FPPlateaus::add_plateau_lengths}}, but only applies to a single
##' country, single indicator.
##'
##' @inheritParams make_tfr_surfs
##' @param x Data frame with the required columns.
##' @return A data frame with columns indicating surf lengths.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
get_surf_lengths <- function(x, min_surf_length) {

    if (!"transition_condition_met" %in% colnames(x)) {
        x[["transition_condition_met"]] <- TRUE
        warning("'transition_condition_met' is not a column in 'x'; it will be added and set to 'TRUE' in every year.")
    }

    ## -------* Checks

    req_cols <- c("year", "transition_condition_met", "surf_year")

    if (!all(req_cols %in% colnames(x)))
        stop("'x' must have columns '", toString(req_cols), "'.")

    if (any(is.na(x[, "surf_year"])))
        stop("'x[, c(", toString("surf_year"), ")]' has missing values.")

    if ("country_code" %in% colnames(x)) {
        if (length(unique(x[["country_code"]])) > 1)
            stop("'x' has column 'country_code' which has and more than one unique value; this function can only process one country at a time.")
    }
    if ("indicator" %in% colnames(x)) {
        if (length(unique(x[["indicator"]])) > 1)
            stop("'x' has column 'indicator' which has and more than one unique value; this function can only process one indicator at a time.")
    }

    ## Keep only the required columns
    x <- x[, req_cols]

    ## -------* Sub-functions

    ## Calculates the extra surf columns: surf start year, surf
    ## group, surf length, and again for "surf_periods", surf
    ## periods that are at least of length 'min_surf_length'. Returns a
    ## data frame with the new columns.
    surf_periods <- function(x, surf_col_name, min_surf_length) {
        ## Call routine
        surf_info_cols <- function(surf, surf_col_name = NULL) {
            nrx <- length(surf)
            nrx_1 <- nrx - 1

            surf_start <- rep(FALSE, nrx)
            surf_start[1] <- surf[1]
            surf_start[2:nrx] <- surf[2:nrx] & !surf[1:nrx_1]

            surf_group <- rep(NA, nrx)
            surf_group[surf] <- cumsum(surf_start)[surf]

            surf_len_tbl <- tapply(surf_group, surf_group, "length")
            surf_len <- rep(NA, nrx)
            for (i in as.numeric(names(surf_len_tbl))) surf_len[surf_group == i] <- surf_len_tbl[i]

            x <- data.frame(surf_start, surf_group, surf_len)
            if (!is.null(surf_col_name))
                colnames(x) <-
                    c(paste0(surf_col_name, c("_start", "_group", "_len")))
            return(x)
        }

        ## Apply twice. Need to do the second pass to re-compute lengths for
        ## surfs that satisfy 'min_surf_length'.
        x1 <- surf_info_cols(x[[surf_col_name]] & x[["transition_condition_met"]], #<<<
                              surf_col_name)
        x2 <- surf_info_cols(x[[surf_col_name]] & x[["transition_condition_met"]] #<<<
                              & x1[[paste0(surf_col_name, "_len")]] >= min_surf_length,
                              surf_col_name)

        ## Rename columns
        colnames(x2) <- paste0(colnames(x2), "_min_length")

        return(data.frame(x1, x2, check.names = FALSE))
    }

    ## -------* Main Body

    ## -------** Prepare Inputs

    x <- x[order(x[["year"]]), ]

    ## -------** Add Lengths

    x <- data.frame(year = x[["year"]],
                    surf_periods(x,
                                  surf_col_name = "surf_year",
                                  min_surf_length = min_surf_length),
                    min_surf_length = min_surf_length)

    return(x)
}


##' Add lengths of surfs to existing data frame
##'
##' Calls \code{\link{get_surf_lengths}} on \code{x} and performs a left join
##' to merge \code{x} onto the result. Merging is done by \code{"year"}, so this
##' column must be present in \code{x}. The result is then re-sorted by
##' \code{"year"}. \emph{NOTE:} The \code{"surf_year"} column in \code{x} is
##' \emph{not} updated! To do this, use \code{apply_min_lengths()}.
##'
##' @inheritParams make_tfr_surfs
##' @param x Data frame with the required columns.
##' @return \code{x} with extra columns, sorted.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
add_surf_lengths <- function(x, min_surf_length) {

    stopifnot("year" %in% colnames (x))

    ## Add surf length columns
    x <- base::merge(x,
                     get_surf_lengths(x = x,
                                       min_surf_length = min_surf_length),
                     by = "year", all.x = TRUE)
    ## Keep in order
    x <- x[order(x[["year"]]), ]

    return(x)
}


##' Remove length-related columns from data frame
##'
##' Removes from \code{x} the columns added by \code{add_surf_lengths}.
##'
##' @param x Data frame with the required columns.
##' @return \code{x} with columns removed.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
remove_length_cols <- function(x) {
    x[, c("min_inter_surf_length",
                  "surf_year_start", "surf_year_group",
                  "surf_year_len", "min_surf_length")] <- NULL
    return(x)
}


##' Replace length-related columns in a data frame
##'
##' Replaces the length-related columns added by \code{add_surf_lengths}. This
##' function is exactly
##'
##' \preformatted{
##' add_surf_lengths(remove_length_cols(x),
##'                   min_surf_length = min_surf_length)
##' }
##'
##' @param x Data frame with the required columns.
##' @return \code{x} with columns replaced.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
replace_surf_lengths <- function(x, min_surf_length) {
    return(add_surf_lengths(remove_length_cols(x),
                             min_surf_length = min_surf_length))
}


##' Apply minimum surf and inter-surf lengths
##'
##' @inheritParams make_tfr_surfs
##' @param x \code{surfs_df}
##' @return A data frame with appropriate columns.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
apply_min_inter_surf_lengths <- function(x, min_inter_surf_length) {

    ## -------* Set Up

    ## Will need this later.
    inter_surfs_too_short_years <- NA

    year_lim <- c(min(x[["year"]]), max(x[["year"]]))

    ## -------* Inter-Stall Minimum Lengths

    if (any(x[["surf_year"]])) {

        if (min_inter_surf_length > 1) {

            ## -------** Min Inter-Stall Length

            ## If 'min_inter_surf_length' is greater than one, need to review
            ## inter-surf lengths. To get lengths of inter-surf periods, simply
            ## negate the surf indicator and use 'get_surf_lengths()'.

            ## -------*** Column Names

            ## Explanation of column names created by 'get_surf_lengths()'

            ## ~ "surf_year": Logical; is this year a surf year based on all
            ## applied conditions so far?
            ##
            ## ~ "surf_year_len": Numeric; for years that are surf years, the
            ## length of the surf, regardless of the minimum surf length,
            ## otherwise NA.
            ##
            ## ~ "surf_year_group_min_length": Numeric; for surf years that meet
            ## the minimum length condition, the actual surf length, otherwise NA.

            ## -------*** Data Sets

            ## Create a dataset with inter-surf periods in the same format as the
            ## 'x' that can be passed to 'get_surf_lengths()'.
            inter_x <- x

            ## Replace the surf indicator column with an indicator for _non-surf_
            ## years, accounting for 'min_surf_length'. Need to run
            ## 'get_surf_lengths()' on 'x' to get this column.
            inter_x[["surf_year"]] <-
                is.na(get_surf_lengths(x = x,
                                        min_surf_length = 1)[["surf_year_group_min_length"]])

            ## Get lengths of inter-surf periods.
            inter_x <- get_surf_lengths(x = inter_x, #<<<
                                         min_surf_length = min_inter_surf_length #<<<
                                         )

            ## The column 'surf_year_len' has the lengths of all
            ## inter-surf periods. Years where this is less than
            ## 'min_inter_surf_len' should be reclassified as surfs in
            ## 'x'. In years where there is a surf, it will be 'NA', so
            ## need to use 'which()'
            inter_surfs_too_short_years <-
                inter_x[which(inter_x[["surf_year_len"]] < min_inter_surf_length),
                        "year"]

            ## Remove years at the start and end of period. If, e.g., 1950 is in
            ## 'inter_surfs_too_short_years', remove it because it's OK to have a
            ## 1-year break here. Similarly 2100 (if that's the last year).
            inter_surfs_too_short_years <-
                inter_surfs_too_short_years[!inter_surfs_too_short_years %in% year_lim]

            ## Similarly, remove any years that are immediately after or immediately
            ## before a year that fails the transition condition. It's OK to have
            ## 1-year breaks immediately before or after the start or end of a
            ## period which fails the transition condition.

            start_trans_years <-
                x[["year"]][(find_breaks(x[["transition_condition_met"]], first_is_break = FALSE)) &
                            (!x[["transition_condition_met"]])] - 1
                                # These are the last year(s) in which the
                                # transition condition is met before a period(s) in
                                # which it is not met.

            end_trans_years <-
                x[["year"]][(find_breaks(x[["transition_condition_met"]], first_is_break = FALSE)) &
                            (x[["transition_condition_met"]])] + 1
                                # These are the first year(s) in which the
                                # transition is met after a period(s) in which
                                # it is not met.

            inter_surfs_too_short_years <-
                inter_surfs_too_short_years[!(inter_surfs_too_short_years %in% start_trans_years |
                                               inter_surfs_too_short_years %in% end_trans_years)]

            ## This will overwrite the values in the column 'surf_year'
            x[x[["year"]] %in% inter_surfs_too_short_years, "surf_year"] <- TRUE

        }

    }

    ## Record minimum surf length
    x[["min_inter_surf_length"]] <- min_inter_surf_length

    ## Note which surfs were added under min inter surf length condition
    x[["min_inter_surf_length_surf_year"]] <- FALSE
    if (!all(is.na(inter_surfs_too_short_years)) && length(inter_surfs_too_short_years) > 0) {
        x[which(x[["year"]] %in% inter_surfs_too_short_years), "min_inter_surf_length_surf_year"] <- TRUE
    }

    ## -------* END

    return(x)
}


##' Apply minimum surf and inter-surf lengths
##'
##' @inheritParams make_tfr_surfs
##' @param x \code{surfs_df}
##' @return A data frame with appropriate columns.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
apply_min_surf_lengths <- function(x, min_surf_length) {

    ## -------* Stall Minimum Lengths

    ## -------**  Add lengths of surf periods.

    ## This will add surf length columns and the 'min_surf_length' info
    ## column, but _not_ alter the "surf_year" column.
    x <- add_surf_lengths(x = x,
                           min_surf_length = min_surf_length)

    ## -------** Fix up columns

    ## 1. Note any years that had surfs removed because they were too short
    surfs_too_short_idx <- which(is.na(x[["surf_year_len_min_length"]]))
    x[["min_surf_length_surf_removed"]] <- FALSE
    x[intersect(which(x[["surf_year"]]), surfs_too_short_idx), "min_surf_length_surf_removed"] <- TRUE

    ## 2. Overwrite the 'surf_year' indicator to reflect minimum lengths
    x[["surf_year"]][surfs_too_short_idx] <- FALSE

    ## 3. Remove extraneous columns
    surf_len_cols <- c("surf_year_start", "surf_year_group", "surf_year_len")
    x <- x[, !colnames(x) %in% surf_len_cols]

    ## 4. Rename "_min_length" columns
    colnames(x)[match(paste0(surf_len_cols, "_min_length"), colnames(x))] <-
        surf_len_cols

    ## -------* END

    return(x)
}


###-----------------------------------------------------------------------------
### * Continuation Condition

##' Get surfs periods due to mid-surf TFR remaining above initial level
##'
##' This function takes two vectors:
##' \describe{
##' \item{\code{surf_year_start}}{A logical vector with one element per year,
##' indicating whether or not the year is the first year of a surf period. This
##' is generated by \code{\link{get_surf_lengths}}.}
##' \item{\code{tfr}}{A numeric vector of total fertility rates, or array of
##' posterior TFR trajectories.}
##' }
##' For each surf start-year, any years between the start-year and the year in which \code{tfr}
##' falls below the level observed in that start year for at least two
##' consecutive years will be classified as a surf. Both of the two years below
##' start-level TFR will be considered non-surf years. This function does not
##' check that the level condition holds but
##' \code{\link{apply_continuation_condition}} does.
##'
##' This function can be applied to a vector of TFRs or an array of trajectories
##' from the posterior distribution generated by \pkg{bayesTFR}. In the former
##' case, the result will be a logical vector indicating whether or not the year
##' is a surf based on this condition. In the latter case, the result
##' will be a numeric vector of probabilities that the regain condition holds.
##'
##' @inheritParams make_tfr_surfs
##' @param surf_year_start Logical vector indicating whether the corresponding
##'     year is the start of a surf; computed by
##'     \code{\link{get_surf_lengths}}.
##' @param tfr Vector of TFRs \emph{or} array of posterior TFR trajectories.
##' @return Vector indicating surf years based on \code{surf_year_start}. If
##'     \code{surf_year_start} is a vector this will be logical, otherwise
##'     numeric. See \dQuote{Details}.
##' @author Mark C Wheldon
##' @family TFR regain functions
##' @family Stall exit criteria functions
##' @keywords internal
##' @noRd
get_tfr_regain_surf_year <- function(surf_year_start, tfr, continuation_condition,
                                      look_ahead = 1,
                                      ncores = getOption("cl.cores", -2)) {

    stopifnot(is.wholenumber(look_ahead))
    stopifnot(look_ahead %in% c(1,2))

    if (!any(surf_year_start)) {
        return(invisible(numeric()))
    } else {

        stopifnot(is.logical(surf_year_start))
        stopifnot(is.numeric(tfr))
        if (!is.null(ncores)) stopifnot(is.wholenumber(ncores))

        N <- length(surf_year_start)

        if (is.null(dimnames(tfr))) nm_tfr <- names(tfr)
        else nm_tfr <- dimnames(tfr)[[1]]

        ## -------------------- ARRAY
        if (is.array(tfr)) {
            ## Must be in the format produced by 'make_tfr_estproj_traj(...,
            ## out_format = "FPPlateaus")'.
            stopifnot(identical(length(dim(tfr)), 3L))
            stopifnot(identical(dim(tfr)[2], 1L))
            if (!is.null(dimnames(tfr)))
                stopifnot(identical(dimnames(tfr)[[2]], "TFR"))

            cl <- set_cores_make_cluster(ncores = ncores, maxjobs = dim(tfr)[3])
            if (!is.null(cl)) {
                ## parallel::clusterCall(cl = cl, assign, "get_tfr_regain_surf_year", get_tfr_regain_surf_year, envir = environment())
                parallel::clusterExport(cl = cl, "get_tfr_regain_surf_year", envir = environment())
                on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
            }

            ## Turns 'tfr' into a logical array of same dimension, each element
            ## indicating whether TFR satisfies the 'regain' condition.
            tfr <- pbapply::pbapply(X = tfr, MARGIN = c(2, 3),
                                    FUN = function(z, sys, ccond) {
                                        get_tfr_regain_surf_year(surf_year_start = sys, tfr = z,
                                                                  continuation_condition = ccond,
                                                                  ncores = NULL)
                                    }, sys = surf_year_start, ccond = continuation_condition,
                                    simplify = TRUE, cl = cl)

            ## 'tfr' will be a 3D array with first dim a collapsed version of
            ## 'continuation_condition'. Need to re-expand.
            tfr <- array(tfr, dim = c(N, sum(surf_year_start), dim(tfr)[-1]),
                         dimnames = list(nm_tfr, nm_tfr[surf_year_start]))

            return(apply(X = tfr[ , , 1, , drop = FALSE],
                         MARGIN = c(1, 2), FUN = "mean"))
        }
        ## -------------------- |

        continuation_surf <- array(FALSE, dim = c(N, sum(surf_year_start)),
                                    dimnames = list(nm_tfr, nm_tfr[surf_year_start]))
                                # Need to track which 'surf_start_year' is creating the SURF
        surf_start_i <- which(surf_year_start)

        for (k in seq_along(surf_start_i)) {
            i <- surf_start_i[k]
            if (i < N) {
                i_plus_1_to_end <- (i + 1):N
                continuation_surf[i_plus_1_to_end, k] <- tfr[i_plus_1_to_end] < tfr[i]
                if (identical(as.numeric(look_ahead), 2)) {
                    if (i < (N - 1)) {
                        i_plus_2_to_end <- c((i + 2):N, i)
                    } else if (i < N) {
                        i_plus_2_to_end <- i
                    }
                    continuation_surf[i_plus_1_to_end, k] <-
                        continuation_surf[i_plus_1_to_end, k] & (tfr[i_plus_2_to_end] < tfr[i])
                }
            }
        }

        return(continuation_surf)
    }
}


##' Apply the continuation condition
##'
##' @inheritParams make_tfr_surfs
##' @param x \code{surfs_df}
##' @return \code{surfs_df} suitably modified
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
apply_continuation_condition <- function(x, traj, continuation_condition = NULL,
                                         continuation_condition_prob_threshold = NULL,
                                         ncores = getOption("cl.cores", -2)) {




    ## Will need these later.
    continuation_surf_TF <- FALSE
    surf_year_start <- NA

    if (any(x[["surf_year"]])) {

        if (!is.null(continuation_condition) && !identical(continuation_condition, "NONE")) {

            if (!"transition_condition_met" %in% colnames(x)) {
                x[["transition_condition_met"]] <- TRUE
                warning("'transition_condition_met' is not a column in 'x'; it will be added and set to 'TRUE' in every year.")
            }

            ## Need to know the start of surfs.

            if (!"surf_year_start" %in% colnames(x)) {
                surf_year_start <-
                    add_surf_lengths(x = x, min_surf_length = 1)[["surf_year_start"]]
            } else {
                surf_year_start <- x[["surf_year_start"]]
            }

            if (identical(continuation_condition, "Regain TFR, median, 2yr")) {

                continuation_surf_PROB <-
                    get_tfr_regain_surf_year(surf_year_start,
                                          tfr = setNames(object = x[["TFR_median"]], nm = x[["year"]]),
                                          continuation_condition = continuation_condition,
                                          look_ahead = 2,
                                          ncores = ncores)

            } else if (continuation_condition %in% c("Regain TFR, probabilistic")) {

                if (is.null(continuation_condition_prob_threshold))
                    stop("continuation_condition, 'Regain TFR, probabilistic' but 'continuation_condition_prob_threshold' is 'NULL'.")

                continuation_surf_PROB <-
                    get_tfr_regain_surf_year(surf_year_start,
                                              tfr = traj,
                                              continuation_condition = continuation_condition,
                                              look_ahead = 1,
                                              ncores = ncores)

            }

            continuation_surf_TF <-
                array(FALSE, dim = dim(continuation_surf_PROB),
                      dimnames = dimnames(continuation_surf_PROB))

            i_len <- nrow(continuation_surf_TF)

            for (j in seq_len(ncol(continuation_surf_TF))) {
                ## Row index of the start-year in 'continuation_surf_TF'
                start_yr_idx <-
                    which(dimnames(continuation_surf_TF)[[1]] ==
                          dimnames(continuation_surf_TF)[[2]][j])

                ## Row indices of all years following start-year.
                i_idx <- (start_yr_idx + 1):i_len

                ## For all rows following start-year, set elements to TRUE
                ## if probability at least the threshold.
                continuation_surf_TF[i_idx, j] <-
                    continuation_surf_PROB[i_idx, j] >= continuation_condition_prob_threshold

                ## Set values in 'continuation_surf_TF' column to 'TRUE' for
                ## all years where the probability is at least the
                ## threshold, but only for years from start-year + 1 up to
                ## the year before the first year the TFR returns to
                ## start-year level.
                if (any(continuation_surf_TF[i_idx, j])) {
                    ## If there is a point at which the TFR goes below start year:
                    first_TRUE <- match(TRUE, continuation_surf_TF[i_idx, j])
                    first_TRUE <- first_TRUE[1]
                    continuation_surf_TF[i_idx[1:(first_TRUE - 1)], j] <- TRUE
                    continuation_surf_TF[i_idx[first_TRUE:length(i_idx)], j] <- FALSE
                } else {
                    ## Otherwise, set the 'first_TRUE' index such that all
                    ## remaining years will be selected in the next step:
                    continuation_surf_TF[i_idx, j] <- TRUE
                }
            }

            ## Apply the transition condition
            for (j in seq_len(ncol(continuation_surf_TF))) {
                continuation_surf_TF[,j] <-
                    x[["transition_condition_met"]] & continuation_surf_TF[,j]
            }

            ## Add continuation surfs
            continuation_surf_TF <- rowSums(continuation_surf_TF) > 0
            x[["surf_year"]] <- x[["surf_year"]] | continuation_surf_TF

        } else {
            continuation_condition <- "NONE"
        }

        ## END

    }

    ## Add meta-info columns

    x$continuation_condition <- continuation_condition
    x$surf_year_start_pre_continuation <- surf_year_start

    if (!is.null(continuation_condition_prob_threshold)) {
        x$continuation_condition_prob_threshold <- continuation_condition_prob_threshold
    } else {
        x$continuation_condition_prob_threshold <- NA
    }

    x$continuation_surf_year <- continuation_surf_TF

    return(x)
}


###-----------------------------------------------------------------------------
### * Stall TFR Max Condition

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param x_df
##' @param tfr
##' @param max_tfr_threshold
##' @param ncores
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
get_max_tfr_surf_year <- function(x_df,
                              tfr,
                              max_tfr_threshold,
                              ncores = getOption("cl.cores", -2)) {

    stopifnot(is.data.frame(x_df))
    stopifnot(all(c("year", "surf_year_group") %in% colnames(x_df)))
    stopifnot(is.numeric(x_df$year))
    stopifnot(is.numeric(x_df$surf_year_group))

    stopifnot(is.numeric(tfr))
    stopifnot(is.numeric(max_tfr_threshold))
    if (!is.null(ncores)) stopifnot(is.wholenumber(ncores))

    if (is.array(tfr)) {
        ## Must be in the format produced by 'make_tfr_estproj_traj(...,
        ## out_format = "FPPlateaus")'.
        stopifnot(identical(length(dim(tfr)), 3L))
        stopifnot(identical(dim(tfr)[2], 1L))
        if (!is.null(dimnames(tfr)))
            stopifnot(identical(dimnames(tfr)[[2]], "TFR"))
        ## Make sure years are the same and in order
        stopifnot(identical(as.numeric(dimnames(tfr)[[1]]),
                            as.numeric(x_df[["year"]])))

        cl <- set_cores_make_cluster(ncores = ncores, maxjobs = dim(tfr)[3])
        if (!is.null(cl)) {
            parallel::clusterExport(cl = cl, "get_max_tfr_surf_year", envir = environment())
            on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
        }

        ## Turns 'tfr' into a logical array of same dimension, each element
        ## indicating whether TFR satisfies the condition.
        tfr <- pbapply::pbapply(X = tfr, MARGIN = c(2, 3),
                                FUN = function(z, sgd, mtt) {
                                    get_max_tfr_surf_year(x_df = sgd,
                                                           tfr = z,
                                                           max_tfr_threshold = mtt,
                                                           ncores = NULL)
                                },
                                sgd = x_df,
                                mtt = max_tfr_threshold,
                                cl = cl)

        return(rowMeans(tfr[, 1, , drop = FALSE]))
    }

    ## Make sure 'x_df' is sorted by 'year'
    x_df <- x_df[order(x_df[["year"]]), ]

    ## Make output data frame
    x_df <- data.frame(tfr = tfr,
                       x_df[, c("year", "surf_year_group")],
                       row.names = NULL)

    ## Get max TFR in each surf period
    group_meets_max_tfr <-
        tapply(X = x_df$tfr, INDEX = x_df$surf_year_group,
               FUN = function(z) max(z, na.rm = TRUE) > max_tfr_threshold)

    group_meets_max_tfr <- rbind(data.frame(surf_year_group = as.numeric(names(group_meets_max_tfr)),
                                            group_meets_max_tfr = group_meets_max_tfr, row.names = NULL),
                                 data.frame(surf_year_group = NA,
                                            group_meets_max_tfr = FALSE))

    x_df <- base::merge(x_df, group_meets_max_tfr, by = "surf_year_group",
                        all.x = TRUE, sort = FALSE)

    ## Make sure 'x_df' is sorted by 'year'
    x_df <- x_df[order(x_df[["year"]]), ]

    return(x_df[, "group_meets_max_tfr"])
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param x
##' @param traj
##' @param exceedance_condition
##' @param exceedance_condition_prob_threshold
##' @param ncores
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
apply_exceedance_condition <- function(x, traj, exceedance_condition = NULL,
                                       exceedance_condition_prob_threshold = NULL,
                                       ncores = getOption("cl.cores", -2)) {

    ## Will need this later.
    exceedance_condition_surf_removed <- FALSE

    if (any(x[["surf_year"]])) {

        if (!is.null(exceedance_condition) && !identical(exceedance_condition, "NONE")) {

            ## Need to know the start of surfs.

            surfs_w_lengths_df <-
                add_surf_lengths(x = x, min_surf_length = 1)

            if (identical(exceedance_condition, "Max TFR > 2.1, median")) {

                ## Based only on median

                exceedance_condition_satisfied <-
                    get_max_tfr_surf_year(
                    data.frame(year = surfs_w_lengths_df[["year"]],
                               surf_year_group = surfs_w_lengths_df[["surf_year_group"]]),
                    tfr = surfs_w_lengths_df[["TFR_median"]],
                    max_tfr_threshold = 2.1,
                    ncores = ncores)

                exceedance_condition_surf_removed <-
                    x[["surf_year"]] & !exceedance_condition_satisfied

                x[["surf_year"]] <- x[["surf_year"]] & exceedance_condition_satisfied

            } else if (identical(exceedance_condition, "Max TFR > 2.1, probabilistic")) {

                ## Probabilistic

                if (is.null(exceedance_condition_prob_threshold))
                    stop("'exceedance_condition' is 'Max TFR > 2.1, probabilistic' but 'exceedance_condition_prob_threshold' is 'NULL'.")

                meets_max_pr <-
                    get_max_tfr_surf_year(data.frame(year = surfs_w_lengths_df[["year"]],
                                                      surf_year_group = surfs_w_lengths_df[["surf_year_group"]]),
                                           tfr = traj,
                                           max_tfr_threshold = 2.1,
                                           ncores = ncores)

                exceedance_condition_satisfied <- meets_max_pr > exceedance_condition_prob_threshold

                exceedance_condition_surf_removed <-
                    x[["surf_year"]] & !exceedance_condition_satisfied

                x[["surf_year"]] <-
                    x[["surf_year"]] & exceedance_condition_satisfied
            }

        } else {
            exceedance_condition <- "NONE"
            exceedance_condition_satisfied <- FALSE
            exceedance_condition_surf_removed <- FALSE
        }

        ## END

    }

    x <- data.frame(x, exceedance_condition = exceedance_condition,
                    exceedance_condition_surf_removed = exceedance_condition_surf_removed)

    if (!is.null(exceedance_condition_prob_threshold)) {
        x <- data.frame(x, exceedance_condition_prob_threshold = exceedance_condition_prob_threshold)
    } else {
        x <- data.frame(x, exceedance_condition_prob_threshold = NA)
    }

    return(x)
}

###-----------------------------------------------------------------------------
### * Estimation or Projection Period

##' Add column indicating whether SURF extends into projection period
##'
##' A column \code{surf_in_proj} is added to indicator whether a SURF is in the
##' projection period. If more than half of a SURF happens after
##' \code{last_est_year}, \code{surf_in_proj} is set to \code{TRUE}.
##'
##' @param x Output of \code{\link{make_tfr_surfs}}.
##' @param last_est_year
##' @return
##' @author Mark C Wheldon
##'
##' @keywords internal
##' @noRd
add_surf_in_proj <- function(x, last_est_year = 2023, proj_split = c("by_surf", "by_year")) {
    proj_split <- match.arg(proj_split)
    x$surf_in_proj <- NA

    if (any(x[["surf_year"]])) {
        x[x[["surf_year"]], "surf_in_proj"] <- FALSE

        if (identical(proj_split, "by_surf")) {
            tmp_ <- x[x[["surf_year_start"]], c("country_code", "surf_year_group", "year")]
            colnames(tmp_)[colnames(tmp_) == "year"] <- "surf_first_year"
            x <- base::merge(x, tmp_, all.x = TRUE, sort = FALSE)
            idx <- which(x[["surf_year"]] &
                         (x[["surf_first_year"]] + x[["surf_year_len"]] / 2 > last_est_year + 1))
                                # Needs to be 'last_est_year + 1' to ensure that
                                # preference goes to the estimation period. Try
                                # it for 'surf_first_year' = 2022,
                                # 'surf_year_len' = 4 and 'last_est_year' =
                                # 2023.
            x[idx, "surf_in_proj"] <- TRUE
            x[["surf_first_year"]] <- NULL
            x <- x[order(x[["year"]]), ]

        } else if (identical(proj_split, "by_year")) {
            idx <- which(x[["surf_year"]])
            x[idx, "surf_in_proj"] <- x[idx, "year"] > last_est_year
        }
    }
    return(x)
}

###-----------------------------------------------------------------------------
### * Schoumaker Stalls

##' Add Schoumaker (2019) TFR stalls.
##'
##' Uses dataset \code{\link{schoumaker_2019_tfr_stalls}}.
##'
##' @param x \code{surfs_df}
##' @param check Check result of merge by comparing dims.
##' @return \code{x} is returned with columns added.
##' @author Mark C Wheldon
##'
##' @examples
##' data(schoumaker_2019_tfr_stalls)
##' head(schoumaker_2019_tfr_stalls)
##'
##' @keywords internal
##' @noRd
add_schoumaker_tfr_stalls <- function(x, check = TRUE) {

    nrx <- nrow(x)

    x <- base::merge(x = x,
                     y = schoumaker_2019_tfr_stalls[, colnames(schoumaker_2019_tfr_stalls) != "name"],
                     all.x = TRUE,
                     all.y = FALSE, # 'schoumaker_2019_tfr_stalls' includes all
                                    # countries; might not want them all.
                     by = c("year", "country_code"))

    if (check) {
        if (!identical(nrow(x), nrx))
            stop("Something went wrong merging 'x' and 'schoumaker_2019_tfr_stalls'.")
    }

    ## Add column indicating if country was in Schoumaker's dataset
    schoumaker_cc <- unique(schoumaker_2019_tfr_stalls[["country_code"]])
    x[["Schoumaker_included"]] <- x[["country_code"]] %in% schoumaker_cc

    ## Fill in all the country-years not in 'x' with 'FALSE' for all the logical
    ## columns (i.e., excluding 'Schoumaker_stall_type').
    schoumaker_cols <- grep("^Schoumaker_stall_[^t]", colnames(x), value = TRUE)
    x[, schoumaker_cols][is.na(x[, schoumaker_cols])] <- FALSE

    ## Keep ordered
    x <- x[order(x[["year"]], x[["country_code"]]), ]

    return(x)
}
