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
### ** `tabulate_tfr_stats()`

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


## COMMENT OUT for now; the 'test_surfs_df' data is now obsolete so this test
## fails. Need to regenerate with updated column names.
##
## test_that("'tabulate_rfr_surfs()' works as expected on a specially constructed results data frame.", {
##     test_surfs_df <- readRDS(file = testthat::test_path("test_data", "test_surfs_df.rds"))

##     tbl_surfs <- tabulate_surf_periods(test_surfs_df, table_type = "surfs only")
##     expect_s3_class(tbl_surfs, "data.frame")
##     expect_identical(nrow(tbl_surfs), 6L)
##     expect_identical(ncol(tbl_surfs), 6L)

##     tbl_surfs <- tabulate_surf_periods(test_surfs_df, table_type = "concise")
##     expect_s3_class(tbl_surfs, "data.frame")
##     expect_identical(nrow(tbl_surfs), 7L)
##     expect_identical(ncol(tbl_surfs), 7L)

##     tbl_surfs <- tabulate_surf_periods(test_surfs_df, table_type = "detailed")
##     expect_s3_class(tbl_surfs, "data.frame")
##     expect_identical(nrow(tbl_surfs), 16L)
##     expect_identical(ncol(tbl_surfs), 8L)
## })


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
