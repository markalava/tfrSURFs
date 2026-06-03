################################################################################
###
### DATE CREATED: 2026-06-02
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Create control tables to check output of main table functions.
###
###-----------------------------------------------------------------------------
###
### INSTRUCTIONS: Do _not_ run this every time the package is updated /
### installed. When the associated tests fail, figureout what went wrong and
### decide if you need to re-create the control tables.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(tfrSURFs)

source(here::here("tests", "testthat", "helper.R"))

date_string <- "2026-06-02"

###-----------------------------------------------------------------------------
### * Input Data

data(test_data_tfrSURFs_list)
saveRDS(test_data_tfrSURFs_list,
        file = here::here("tests", "testthat", "fixtures",
                          paste0("control_table_input_data_", date_string, ".rds")))

###-----------------------------------------------------------------------------
### * Make Tables

###-----------------------------------------------------------------------------
### ** Periods

saveRDS(make_control_table_periods(test_data_tfrSURFs_list),
        file = here::here("tests", "testthat", "fixtures",
                          paste0("control_table_period_", date_string, ".rds")))

###-----------------------------------------------------------------------------
### ** Stats

for (st in c("count", "avg_len")) {
    for (tr in c("estimation", "projection", "all")) {
        saveRDS(make_control_table_stats(test_data_tfrSURFs_list,
                                         stat = st, time_range = tr),
                file = here::here("tests", "testthat", "fixtures",
                                  paste0("control_table_stat_", st, "_", tr,
                                         "_", date_string, ".rds")))
    }
}
