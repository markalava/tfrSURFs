

###-----------------------------------------------------------------------------
### * Functions


##' Create all combinations of arguments for `tabulate_loc_by_surf()`.
##'
##' @return A data frame
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
make_tabulate_loc_by_surf_param_df <- function() {
    outdf <-
        expand.grid(count_what = get_arg_defs("tabulate_loc_by_surf.list", arg = "count_what"),
                    incl_small_countries = c(TRUE, FALSE),
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
                    table_type = c("concise", "surfs only", "detailed"),
                    incl_no_surfs = c(TRUE, FALSE),
                    flag_schoumaker_excl = c(TRUE, FALSE),
                    stringsAsFactors = FALSE)
    return(outdf)
}


##' Create control tables to compare current outputs against.
##'
##' @param x
##' @param stat
##' @param time_range
##' @return
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
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
##' @rdname make_control_table_stats
##' @noRd
make_control_table_periods <- function(x) {
    tabulate_surf_periods(x,
                          table_type = "concise",
                          incl_no_surfs = TRUE,
                          flag_schoumaker_excl = TRUE,
                          digits = 1)
}
