###-----------------------------------------------------------------------------
### * Data Frame Methods

###-----------------------------------------------------------------------------
### ** Test All Tabulation Fns with Default Args

test_that("'tabulate_loc_by_surf()` works on a data frame with default arg values.", {
    expect_tabulate_S3_df_default("tabulate_loc_by_surf")
})

test_that("'tabulate_surf_stats()` works on a data frame with default arg values.", {
    expect_tabulate_S3_df_default("tabulate_surf_stats")
})

test_that("'tabulate_surf_periods()` works on a data frame with default arg values.", {
    expect_tabulate_S3_df_default("tabulate_surf_periods")
})

###-----------------------------------------------------------------------------
### ** Test All Tabulation Fns with No Countries

test_that("'tabulate_loc_by_surf()' fails properly when 'no small countries' results in no countries.", {
    expect_tabulate_fail_no_countries("tabulate_loc_by_surf")
})

test_that("'tabulate_surf_stats()' fails properly when 'no small countries' results in no countries.", {
    expect_tabulate_fail_no_countries("tabulate_surf_stats")
})

test_that("'tabulate_surf_periods()' fails properly when 'no small countries' results in no countries.", {
    expect_tabulate_fail_no_countries("tabulate_surf_periods")
})

###-----------------------------------------------------------------------------
### ** Test All Tabulation Functions on All Combs of Args

test_that("'tabulate_loc_by_surf()` works on a list, all combos of arg values.", {
    expect_tabulate_S3_df_all_comb("make_tabulate_loc_by_surf_param_df", "tabulate_loc_by_surf")
})

test_that("'tabulate_surf_stats()` works on a list, all combos of arg values.", {
    expect_tabulate_S3_df_all_comb("make_tabulate_surf_stats_param_df", "tabulate_surf_stats")
})

test_that("'tabulate_surf_periods()` works on a list, all combos of arg values.", {
    expect_tabulate_S3_df_all_comb("make_tabulate_surf_periods_param_df", "tabulate_surf_periods")
})

###-----------------------------------------------------------------------------
### * Control Tables

## Test whether tabulation functions produce output different from the most
## recently generated controls.

test_that("'tabulate_loc_by_surv()' reproduces control table from 2026-06-02.", {
    input_lst <- readRDS(test_path("fixtures", "control_table_input_data_2026-06-02.rds"))
    for (bs in c(TRUE, FALSE)) {
        for (tr in c("estimation", "projection", "all")) {
            message("by_surf = '", bs, "'; time_range = '", tr, "'.")
            ctrl_tbl <-
                readRDS(test_path("fixtures",
                                  paste0("control_table_loc_by_surf_",
                                         "by_surf-", bs,
                                         "_", tr,
                                         "_", "2026-06-02", ".rds")))
            expect_equal(make_control_table_loc_by_surf(input_lst, by_surf = bs, time_range = tr),
                         ctrl_tbl)
        }
    }
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

test_that("'tabulate_surf_periods()' reproduces control table from 2026-06-02.", {
    input_lst <- readRDS(test_path("fixtures", "control_table_input_data_2026-06-02.rds"))
    for (tt in c("concise", "surfs_only", "detailed")) {
        ctrl_tbl <- readRDS(file = here::here("tests", "testthat", "fixtures",
                                              paste0("control_table_period_", tt, "_",
                                                     "2026-06-02", ".rds")))
        expect_equal(make_control_table_periods(input_lst, table_type = tt), ctrl_tbl)
    }
})
