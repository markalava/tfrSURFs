###-----------------------------------------------------------------------------
### * Data Frame Methods

###-----------------------------------------------------------------------------
### ** `tabulate_tfr_stats()`

test_that("'tabulate_surf_stats()` works on a data frame with default arg values.", {
    data(test_data_tfrSURFs_list)
                for (k in names(test_data_tfrSURFs_list)[1:3]) {
                    message("Country code = '", k, "'")
                    expect_s3_class(
                        tabulate_surf_stats(
                            test_data_tfrSURFs_list[[k]]),
                        "data.frame")
                    }
})

###-----------------------------------------------------------------------------
### ** `tabulate_surf_periods()`

test_that("'tabulate_surf_periods()' works on a data frame, default arg values.", {

    data(test_data_tfrSURFs_list)
        for (k in names(test_data_tfrSURFs_list)[1:3]) {
            message("Country code = '", k, "'")
            expect_s3_class(tabulate_surf_periods(test_data_tfrSURFs_list[[k]]),
                            "data.frame")
        }
})

###-----------------------------------------------------------------------------
### * List Methods

###-----------------------------------------------------------------------------
### ** `tabulate_surf_stats()`

test_that("'tabulate_surf_stats()` works on a list, all combos of arg values.", {

    data(test_data_tfrSURFs_list)

    param_df <- make_tabulate_surf_stats_param_df()

    geographies_all <- get_arg_defs("tabulate_surf_stats.list", arg = "geographies")

    if (requireNamespace("gtools", quietly = TRUE)) {
        geog_combs <-
            lapply(seq_along(geographies_all),
                   function(z) gtools::combinations(n = length(geographies_all), r = z)
                   )
    } else {
        geog_combs <-
            lapply(seq_along(geographies_all), function(z) matrix(1:z, nrow = 1))
    }

    for (fi_comb in seq_along(geog_combs)) {
        for (fi in nrow(geog_combs[[fi_comb]])) {
            for (param_i in seq_len(nrow(param_df))) {
                pars <- param_df[param_i, , drop = FALSE]
            message("[", param_i, " of ", nrow(param_df), "]: ",
                    paste(colnames(param_df), paste0(pars, "; "), sep = " = "))
                    expect_s3_class(
                        tabulate_surf_stats(
                            test_data_tfrSURFs_list,
                            stat = pars[["stat"]],
                            incl_small_countries = pars[["incl_small_countries"]],
                            proj_split = pars[["proj_split"]],
                            last_est_year = pars[["last_est_year"]],
                            geographies = geographies_all[geog_combs[[fi_comb]][fi, ]]),
                        "data.frame")
            }
        }
    }
})

###-----------------------------------------------------------------------------
### ** `tabulate_surf_periods()`

test_that("'tabulate_rfr_surfs()' fails properly when 'no small countries' results in no countries.", {
    data(test_data_small_c_tfrSURFs_list)
    expect_error(tabulate_surf_periods(test_data_small_c_tfrSURFs_list,
                                     incl_small_countries = FALSE),
                 "no countries left")
})

test_that("'tabulate_surf_periods()' works on a list, all combos of arguments.", {

    data(test_data_tfrSURFs_list)

    param_df <- make_tabulate_surf_periods_param_df()

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        message("[", param_i, " of ", nrow(param_df), "]: ",
                paste(colnames(param_df), paste0(pars, "; "), sep = " = "))
        expect_s3_class(tabulate_surf_periods(test_data_tfrSURFs_list,
                                            incl_small_countries = pars[["incl_small_countries"]],
                                            table_type = pars[["table_type"]],
                                            digits = pars[["digits"]]),
                        "data.frame")

    }
})

###-----------------------------------------------------------------------------
### * Control Tables

## Test whether tabulation functions produce output different from the most
## recently generated controls.

