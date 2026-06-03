###-----------------------------------------------------------------------------
### * Data Frame Methods

###-----------------------------------------------------------------------------
### ** `tabulate_loc_by_surf()`

test_that("'tabulate_loc_by_surf()` works on a data frame with default arg values.", {
    data(test_data_tfrSURFs_list)
    for (k in names(test_data_tfrSURFs_list)[1:3]) {
        message("Country code = '", k, "'")
        expect_s3_class(
            tabulate_loc_by_surf(
                test_data_tfrSURFs_list[[k]]),
            "data.frame")
    }
})

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
### ** `tabulate_loc_by_surf()`

test_that("'tabulate_loc_by_surf()' fails properly when 'no small countries' results in no countries.", {
    data(test_data_small_c_tfrSURFs_list)
    expect_error(tabulate_loc_by_surf(test_data_small_c_tfrSURFs_list,
                                     incl_small_countries = FALSE),
                 "no countries left")
})


test_that("'tabulate_loc_by_surf()` works on a list, all combos of arg values.", {

    data(test_data_tfrSURFs_list)

    param_df <- make_tabulate_loc_by_surf_param_df()

    geographies_all <- get_arg_defs("tabulate_loc_by_surf.list", arg = "geographies")

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
                        tabulate_loc_by_surf(
                            test_data_tfrSURFs_list,
                            count_what = pars[["count_what"]],
                            incl_small_countries = pars[["incl_small_countries"]],
                            time_range = pars[["time_range"]],
                            last_est_year = pars[["last_est_year"]],
                            geographies = geographies_all[geog_combs[[fi_comb]][fi, ]],
                            by_surf = pars[["by_surf"]]
                        ),
                        "data.frame")
            }
        }
    }
})


###-----------------------------------------------------------------------------
### ** `tabulate_surf_stats()`

test_that("'tabulate_surf_stats()' fails properly when 'no small countries' results in no countries.", {
    data(test_data_small_c_tfrSURFs_list)
    expect_error(tabulate_surf_stats(test_data_small_c_tfrSURFs_list,
                                       incl_small_countries = FALSE),
                 "no countries left")
})


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
                            time_range = pars[["time_range"]],
                            last_est_year = pars[["last_est_year"]],
                            geographies = geographies_all[geog_combs[[fi_comb]][fi, ]]),
                        "data.frame")
            }
        }
    }
})

###-----------------------------------------------------------------------------
### ** `tabulate_surf_periods()`

test_that("'tabulate_surf_periods()' fails properly when 'no small countries' results in no countries.", {
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

test_that("'tabulate_surf_periods()' reproduces control table from 2026-06-02.", {
    input_lst <- readRDS(test_path("fixtures", "control_table_input_data_2026-06-02.rds"))
    ctrl_tbl <- readRDS(test_path("fixtures", "control_table_period_2026-06-02.rds"))
    expect_equal(make_control_table_periods(input_lst), ctrl_tbl)
})

test_that("'tabulate_surf_stats()' reproduces control tables from 2026-06-02.", {
    input_lst <- readRDS(test_path("fixtures", "control_table_input_data_2026-06-02.rds"))
    for (st in c("count", "avg_len")) {
        for (tr in c("estimation", "projection", "all")) {
            message("stat = '", st, "'; time_range = '", tr, "'.")
            ctrl_tbl <- readRDS(test_path("fixtures", paste0("control_table_stat_", st, "_", tr,
                                                             "_", "2026-06-02", ".rds")))
            expect_equal(make_control_table_stats(input_lst, stat = st, time_range = tr), ctrl_tbl)
        }
    }
})
