################################################################################
###
### DATE CREATED: 2025-10-30
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Definitions of the columns in the data frame returned by
### 'make_tfr_surfs()'.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

stopifnot(requireNamespace("here", quietly = TRUE))
stopifnot(requireNamespace("withr", quietly = TRUE))
withr::local_dir(here::here("data-raw"), .local_envir = globalenv())

###-----------------------------------------------------------------------------
### * Create Data

output_column_definitions <- read.csv(here::here("data-raw", "output_cols_definitions.csv"))

###-----------------------------------------------------------------------------
### ** Export

usethis::use_data(output_column_definitions, internal = FALSE, overwrite = TRUE)
