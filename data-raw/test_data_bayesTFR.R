################################################################################
###
### DATE CREATED: 2025-06-03
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Do a short run of bayesTFR for testing.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

## library(bayesTFR)
## library(here)
## library(withr)

## withr::local_dir(here::here("data-raw"))

## my_tfr_outputs <- here::here("data-raw", "bigData", "bayesTFR_short_test")

## ###-----------------------------------------------------------------------------
## ### * Short Run

## tfr_phaseII <-
##   run.tfr.mcmc(nr.chains = 2, iter = 10, thin = 2, # default iter: nr.chains = 3, iter = 62000, thin = 1,
##         output.dir = my_tfr_outputs,
##     annual = TRUE, #<< 1-year periods, not five-year periods
##     start.year = 1950, present.year = 2023,
##     wpp.year = 2024, #<< refers to WPP 2024
##     uncertainty = TRUE, #<< Do use uncertainty in estimation phase
##     replace.output = TRUE, #<< use 'TRUE' to overwrite existing results
##     seed = 1, parallel = TRUE)

## tfr_phaseIII <-
##   run.tfr3.mcmc(nr.chains = 2, iter = 10, thin = 2, # default iter: nr.chains = 3, iter = 50000, thin = 10,
##         sim.dir = my_tfr_outputs,
##         replace.output = TRUE, #<< use 'TRUE' to overwrite existing results
##         seed = 1, parallel = TRUE)

## tfr_proj <-
##     tfr.predict(burnin = 1, burnin3 = 1,# default iter: nr.traj = NULL, thin = NULL, burnin = 2000, burnin3 = 2000
##         sim.dir = my_tfr_outputs, end.year = 2100,
##         use.correlation = FALSE,
##         uncertainty = TRUE, #<< Do use uncertainty in estimation phase
##         replace.output = TRUE #<< use 'TRUE' to overwrite existing results
##         )

## ###-----------------------------------------------------------------------------
## ### * Archive

## ## Don't keep the raw results in the package or a GitHub repository. There are
## ## over 4700 files. Any operation, such as searching (e.g., by grep) or indexing
## ## (e.g., by git) will take eons. You will go crazy. Instead, archive (and
## ## compress) it. When you need to use it, decompress it into a temporary folder
## ## and use the temporary version.

## owd <- getwd()
## setwd(file.path(my_tfr_outputs, ".."))
## tar("bayesTFR_short_test.tar.gz", files = basename(my_tfr_outputs),
##     compression = "gzip",
##     tar = "internal")
## setwd(owd)

## unlink(my_tfr_outputs, recursive = TRUE)

## ## TODO: You need to delete the uncompressed directory.
