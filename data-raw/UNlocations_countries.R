################################################################################
###
### DATE CREATED: 2025-06-18
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: Get and subset the 'UNlocations' table from the wpp2024
### package.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

stopifnot(requireNamespace("here", quietly = TRUE))
stopifnot(requireNamespace("withr", quietly = TRUE))
library(wpp2024) # library(devtools)
                 # options(timeout = 600)
                 # install_github("PPgp/wpp2024")

withr::local_dir(here::here("data-raw"), .local_envir = globalenv())

###-----------------------------------------------------------------------------
### * Create Data

###-----------------------------------------------------------------------------
### ** Sources

data(UNlocations)

## Copied from 'World Population Prospects 2024. File 0-1: Location list with
## codes, description, SDGs region, subregion and UN development group, World
## Bank income groups, year of population peak and countries with explicit
## HIV/AIDS mortality modelling in WPP 2024 revision POP/DB/WPP/Rev.2024/F0-1.
## https://population.un.org/wpp/downloads?folder=Documentation&group=Documentation
## (accessed 2025-06-18)'.

small_countries <-
    c("654", "234", "831", "833", "20", "292", "336", "674", "438", "492",
      "660", "535", "92", "136", "212", "500", "652", "659", "663", "534",
      "796", "850", "238", "60", "304", "666", "584", "520", "580", "585", "16",
      "184", "570", "772", "798", "876")

###-----------------------------------------------------------------------------
### ** Create

UNlocations_countries <-
    UNlocations[UNlocations[["location_type"]] == 4,
                c("name", "country_code", "reg_name", "area_name", "agcode_1834000")]

## Put N. Am. as a subregion
UNlocations[UNlocations[["area_name"]] == "Northern America", "reg_name"] <- "Northern America"

## Add SSA
UNlocations_countries[["sub_saharan_africa"]] <- FALSE
UNlocations_countries[["sub_saharan_africa"]][UNlocations_countries[["agcode_1834000"]] == 1834] <- TRUE

UNlocations_countries[["agcode_1834000"]] <- NULL

## Add small countries
UNlocations_countries[["pop_lt_90k_2024"]] <- FALSE
UNlocations_countries[UNlocations_countries[["country_code"]] %in%
                      as.numeric(small_countries),
                      "pop_lt_90k_2024"] <- TRUE

comment(UNlocations_countries) <-
    "Source: World Population Prospects 2024. File 0-1: Location list with codes, description, SDGs region, subregion and UN development group, World Bank income groups, year of population peak and countries with explicit HIV/AIDS mortality modelling in WPP 2024 revision POP/DB/WPP/Rev.2024/F0-1. https://population.un.org/wpp/downloads?folder=Documentation&group=Documentation (accessed 2025-06-18)."

###-----------------------------------------------------------------------------
### ** Export

usethis::use_data(UNlocations_countries, internal = FALSE, overwrite = TRUE)
