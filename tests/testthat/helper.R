

###-----------------------------------------------------------------------------
### * Functions

###-----------------------------------------------------------------------------
### ** Tabulations

###-----------------------------------------------------------------------------
### *** Create Things

###-----------------------------------------------------------------------------
### **** Argument Combinations

## Confusingly, these all have 'param_df' in their names. Should probably be
## 'args_df' ...

##' Create all combinations of arguments for `tabulate_loc_by_surf()`.
##'
##' @return A data frame
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
make_tabulate_loc_by_surf_param_df <- function() {
    outdf <-
        expand.grid(incl_small_countries = c(TRUE, FALSE),
                    time_range = get_arg_defs("tabulate_loc_by_surf.list",
                                              arg = "time_range"),
                    last_est_year = c(2000, 1950, 2100),
                    by_surf = c(TRUE, FALSE),
                    stringsAsFactors = FALSE)
    return(outdf)
}

##' Create all combinations of arguments for `tabulate_tfr_stats()`.
##'
##' @return A data frame
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
make_tabulate_surf_stats_param_df <- function() {
    outdf <-
        expand.grid(stat = get_arg_defs("tabulate_surf_stats.list", arg = "stat"),
                    incl_small_countries = c(TRUE, FALSE),
                   time_range = get_arg_defs("tabulate_surf_stats.list",
                                             arg = "time_range"),
                    last_est_year = c(2000, 1950, 2100),
                    stringsAsFactors = FALSE)
    return(outdf)
}

##' Create all combinations of arguments for `tabulate_surf_periods()`.
##'
##' @return A data frame
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
make_tabulate_surf_periods_param_df <- function() {
    outdf <-
        expand.grid(incl_small_countries = c(TRUE, FALSE),
                    digits = c(2, 4),
                    table_type = c("concise", "surfs_only", "detailed"),
                    incl_no_surfs = c(TRUE, FALSE),
                    flag_schoumaker_excl = c(TRUE, FALSE),
                    stringsAsFactors = FALSE)
    return(outdf)
}

###-----------------------------------------------------------------------------
### **** Control Tables

##' Create control tables to compare current outputs against.
##'
##' @param x
##' @param stat
##' @param time_range
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
make_control_table_loc_by_surf <- function(x, by_surf,
                                           time_range = c("estimation", "projection", "all")) {
    time_range <- match.arg(time_range)
    tabulate_loc_by_surf(x = x, stat = "count",
                              incl_small_countries = FALSE,
                              geographies = c("area_name", "reg_name", "name", "global"),
                              time_range = time_range,
                              last_est_year = 2023,
                              by_surf = by_surf)
}

make_control_table_stats <- function(x, stat = c("count", "avg_len"),
                                     time_range = c("estimation", "projection", "all")) {
    stat <- match.arg(stat)
    time_range <- match.arg(time_range)
    tabulate_surf_stats(x = x, stat = "count",
                        incl_small_countries = FALSE,
                        filter_zero_rows = TRUE,
                        geographies = c("area_name", "reg_name", "name", "global"),
                        time_range = time_range,
                        last_est_year = 2023)
}

##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
make_control_table_periods <- function(x, table_type) {
    tabulate_surf_periods(x,
                          table_type = table_type,
                          incl_no_surfs = TRUE,
                          flag_schoumaker_excl = TRUE,
                          digits = 1)
}

###-----------------------------------------------------------------------------
### *** Test Things

##' Expectation wrappers for testing tabulation functions
##'
##' @param tabulate_fn
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
expect_tabulate_S3_df_default <- function(tabulate_fn) {
    tabulate_fn <- match.fun(tabulate_fn)
    data(test_data_tfrSURFs_list)
    for (k in names(test_data_tfrSURFs_list)[1:3]) {
        message("\n↓↓ Country code = '", k, "' ↓↓")
        expect_s3_class(
            tabulate_fn(
                test_data_tfrSURFs_list[[k]]),
            "data.frame")
    }
}

##' Expectation wrappers for testing tabulation functions
##'
##' @param tabulate_fn
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
expect_tabulate_fail_no_countries <- function(tabulate_fn) {
    tabulate_fn <- match.fun(tabulate_fn)
        data(test_data_small_c_tfrSURFs_list)
        expect_error(tabulate_fn(test_data_small_c_tfrSURFs_list,
                                          incl_small_countries = FALSE),
                     "no countries left")
}

##' Expectation wrappers for testing tabulation functions
##'
##' @param make_param_df_fn
##' @param tabulate_fn
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
expect_tabulate_S3_df_all_comb <- function(make_param_df_fn, tabulate_fn) {

    stopifnot(is.character(make_param_df_fn) && is.character(tabulate_fn))

    make_param_df_fn <- match.fun(make_param_df_fn)
    tabulate_fn_list_method <- getS3method(tabulate_fn, class = "list")
    tabulate_fn <- match.fun(tabulate_fn)

    data(test_data_tfrSURFs_list)

    param_df <- make_param_df_fn()

    geographies_all <- get_arg_defs(tabulate_fn_list_method, arg = "geographies")

    has_geographies <- isTRUE(length(geographies_all) > 0)

    if (has_geographies) { # tabulate surf periods doesn't have 'geographies' arg
        if (requireNamespace("gtools", quietly = TRUE)) {
            geog_combs <-
                lapply(seq_along(geographies_all),
                       function(z) gtools::combinations(n = length(geographies_all), r = z)
                       )
        } else {
            geog_combs <-
                lapply(seq_along(geographies_all), function(z) matrix(1:z, nrow = 1))
        }
    } else {
        ## data frame in list so that it works like ^^
        geog_combs <- list(x = data.frame(x = "(no geogs arg)"))
    }

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        for (fi_comb in seq_along(geog_combs)) {
            for (fi in seq_len(nrow(geog_combs[[fi_comb]]))) {
                geogs <- geographies_all[geog_combs[[fi_comb]][fi, ]]
                message("\n\nArguments: [", param_i, " of ", nrow(param_df), "]: ",
                        paste(colnames(param_df), paste0(pars, "; "), sep = " = "),
                        "\n\tgeographies: '", toString(geogs), "'",
                        "\n\t|\n\t↓")
                arg_list <- c(list(x = test_data_tfrSURFs_list),
                              lapply(setNames(nm = colnames(pars)), function(z) pars[[z]]))
                if (has_geographies) {
                    arg_list <-
                        c(arg_list,
                          list(geographies = geographies_all[geog_combs[[fi_comb]][fi, ]]))
                }
                expect_s3_class(do.call(tabulate_fn, args = arg_list), "data.frame")
            }
        }
    }
}

