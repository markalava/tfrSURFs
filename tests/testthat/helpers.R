

###-----------------------------------------------------------------------------
### * Functions


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
                    proj_split = get_arg_defs("tabulate_surf_stats.list",
                                              arg = "proj_split"),
                    last_est_year = c(2000, 1950, 2100),
                    stringsAsFactors = FALSE)
    outdf <- outdf[!(outdf[["stat"]] == "count" & outdf[["proj_split"]] == "by_year"), ]
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




