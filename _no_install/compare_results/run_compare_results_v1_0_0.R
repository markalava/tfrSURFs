################################################################################
###
### DATE CREATED: 2026-05-28
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Run tests in this directory.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(testthat)
library(tfrSURFs)

archived_results_dirname <- "archived_results_v1.0.0"
source(here::here("_no_install", "compare_results", "_0_setup.R"),
       echo = TRUE, max.deparse.length = 9e6)

test_file(here::here("_no_install", "compare_results", "_compare_results.R"))
