################################################################################
###
### DATE CREATED: 2025-06-11
###
### AUTHOR: Mark Wheldon
###
### PROJECT: tfrSURFs
###
### DESCRIPTION: TFR stalls identified by Schoumaker, B. (2019), "Stalls in
### Fertility Transitions in sub-Saharan Africa: Revisiting the Evidence,"
### Studies in Family Planning, 50, 257–278. https://doi.org/10/gf5zpt.
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

library(tfrSURFs)

stopifnot(requireNamespace("FPPlateaus", quietly = TRUE))
stopifnot(requireNamespace("gdata", quietly = TRUE))
stopifnot(requireNamespace("here", quietly = TRUE))
stopifnot(requireNamespace("openxlsx", quietly = TRUE))
stopifnot(requireNamespace("withr", quietly = TRUE))

options("openxlsx.dateFormat" = "yyyy-mmm-dd")

setwd(here::here("data-raw"))

###-----------------------------------------------------------------------------
### * Inputs

###
### NOTE: File 'sifp12098-sup-0002-appendixb.xlsx' is available at
### https://onlinelibrary.wiley.com/doi/full/10.1111/sifp.12098
###

schoumaker_2019_tfr_stalls <-
    openxlsx::readWorkbook(here::here("data-raw",
                                      "sifp12098-sup-0002-appendixb.xlsx"),
                           startRow = 3)

row.names(schoumaker_2019_tfr_stalls) <- NULL

## The countries not in Schoumaker's dataset
UN_ssa <-
    UNlocations_countries[UNlocations_countries[["sub_saharan_africa"]],
                          c("country_code", "name")]

###-----------------------------------------------------------------------------
### ** Additions / Exclusions

### 'NA' rows

schoumaker_2019_tfr_stalls <-
    schoumaker_2019_tfr_stalls[!is.na(schoumaker_2019_tfr_stalls$Country), ]

### Pretransitional stalls

schoumaker_2019_tfr_stalls[which(schoumaker_2019_tfr_stalls[["No.transition"]] == 1),
                           "Category"] <- "Pretransitional"

### Match Country Names to UNlocations_countries

schoumaker_2019_tfr_stalls$Country[schoumaker_2019_tfr_stalls$Country == "Côte d'Ivoire"] <-
    UNlocations_countries[UNlocations_countries$country_code == 384, "name"]

schoumaker_2019_tfr_stalls$Country[schoumaker_2019_tfr_stalls$Country == "DR Congo"] <-
    UNlocations_countries[UNlocations_countries$country_code == 180, "name"]

schoumaker_2019_tfr_stalls$Country[schoumaker_2019_tfr_stalls$Country == "Tanzania"] <-
    UNlocations_countries[UNlocations_countries$country_code == 834, "name"]

### Add country codes. The input sheet only has country names.

data(UNlocations_countries)

cs <- unique(schoumaker_2019_tfr_stalls$Country)

cs_no_code <- cs[!cs %in% UNlocations_countries$name]
if (length(cs_no_code))
    stop("Some countries in 'schoumaker_2019_tfr_stalls' are not in UN Locations.")

schoumaker_2019_tfr_stalls <-
    base::merge(UNlocations_countries[, c("name", "country_code")],
                schoumaker_2019_tfr_stalls,
                by.x = "name", by.y = "Country",
                all.x = FALSE, all.y = TRUE)

### FIX Ghana

## The Ghana DHS was done in 1998, not 1999.
schoumaker_2019_tfr_stalls[schoumaker_2019_tfr_stalls$name == "Ghana", "Period"] <-
    gsub("1999", "1998",
         schoumaker_2019_tfr_stalls[schoumaker_2019_tfr_stalls$name == "Ghana",
                                    "Period"])

### Summarize the stall types

schoumaker_2019_tfr_stalls$Schoumaker_stall_type <-
    factor(schoumaker_2019_tfr_stalls$Category,
           levels = c("Moderate evidence", "No stall", "Strong evidence",
                      "Unambiguous evidence",  "Very strong evidence",
                      "Weak or conflicting evidence",
                      "Pretransitional"),
           labels = c("Moderate evidence", "No stall",
                      "Strong+ evidence", "Strong+ evidence", "Strong+ evidence",
                      "Weak or conflicting evidence",
                      "Pretransitional")) |>
    as.character()

### Keep only some columns

schoumaker_2019_tfr_stalls <-
    schoumaker_2019_tfr_stalls[, c("country_code", "name", "Period",
                                   "Schoumaker_stall_type", "Category")]

###-----------------------------------------------------------------------------
### * Computations

###-----------------------------------------------------------------------------
### ** Expand year ranges

## The input has a column "Period" which expresses the years as a range in the
## format 'yyyy-zzzz'. Expand these and create a row for each individual year.
##
## NOTE: The ranges are interpreted as closed on the left, open on the right,
## i.e., [yyyy, zzzz). This means, for example, that period "1998-2002" will be
## interpreted as [1998, 2002); 1998 will be a stall year, 2002 will not.

schoumaker_2019_tfr_stalls <-
    lapply(seq_len(nrow(schoumaker_2019_tfr_stalls)), function(i) {
        years <-
            strsplit(schoumaker_2019_tfr_stalls[i, "Period"], "-", fixed = TRUE)
        z <- schoumaker_2019_tfr_stalls[i, ]
        row.names(z) <- NULL
        data.frame(z,
                   year = seq(from = years[[1]][1],
                              to = as.numeric(years[[1]][2]) - 1, # interval is open on right
                              by = 1))
    })

schoumaker_2019_tfr_stalls <-
    do.call("rbind", args = schoumaker_2019_tfr_stalls)

if (any(xtabs(~ year + country_code, data = schoumaker_2019_tfr_stalls) > 1))
    stop("'schoumaker_2019_tfr_stalls' has duplicates in the 'year' column.")

###-----------------------------------------------------------------------------
### ** Columns

### Stall indicator

schoumaker_2019_tfr_stalls$Schoumaker_stall_any <-
    !is.na(schoumaker_2019_tfr_stalls$Schoumaker_stall_type) &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type != "No stall"

### Add T/F columns for each stall type

schoumaker_2019_tfr_stalls$Schoumaker_stall_strong <- FALSE
idx <- !is.na(schoumaker_2019_tfr_stalls$Schoumaker_stall_type) &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type != "No stall" &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type == "Strong+ evidence"
schoumaker_2019_tfr_stalls[idx, "Schoumaker_stall_strong"] <- TRUE

schoumaker_2019_tfr_stalls$Schoumaker_stall_moderate <- FALSE
idx <- !is.na(schoumaker_2019_tfr_stalls$Schoumaker_stall_type) &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type != "No stall" &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type == "Moderate evidence"
schoumaker_2019_tfr_stalls[idx, "Schoumaker_stall_moderate"] <- TRUE

schoumaker_2019_tfr_stalls$Schoumaker_stall_weak <- FALSE
idx <- !is.na(schoumaker_2019_tfr_stalls$Schoumaker_stall_type) &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type != "No stall" &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type == "Weak or conflicting evidence"
schoumaker_2019_tfr_stalls[idx, "Schoumaker_stall_weak"] <- TRUE

schoumaker_2019_tfr_stalls$Schoumaker_stall_pretransitional <- FALSE
idx <- !is.na(schoumaker_2019_tfr_stalls$Schoumaker_stall_type) &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type != "No stall" &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type == "Pretransitional"
schoumaker_2019_tfr_stalls[idx, "Schoumaker_stall_pretransitional"] <- TRUE

schoumaker_2019_tfr_stalls$Schoumaker_stall_none <- FALSE
idx <- !is.na(schoumaker_2019_tfr_stalls$Schoumaker_stall_type) &
    schoumaker_2019_tfr_stalls$Schoumaker_stall_type == "No stall"
schoumaker_2019_tfr_stalls[idx, "Schoumaker_stall_none"] <- TRUE

### Keep only some columns

schoumaker_2019_tfr_stalls <-
    schoumaker_2019_tfr_stalls[, c("year", "country_code", "name",
                                 "Schoumaker_stall_type",
                                 "Schoumaker_stall_strong", "Schoumaker_stall_moderate",
                                 "Schoumaker_stall_weak", "Schoumaker_stall_any",
                                 "Schoumaker_stall_pretransitional", "Schoumaker_stall_none")]

###-----------------------------------------------------------------------------
### ** Add countries not in Schoumaker's dataset

### DON'T do this; column 'Schoumaker_included' is created by 'add_schoumaker_tfr_stalls()'.

## schoumaker_2019_tfr_stalls$Schoumaker_included <- TRUE
## sch_cc <- schoumaker_2019_tfr_stalls[["country_code"]]
## schoumaker_2019_tfr_stalls <-
##     base::merge(schoumaker_2019_tfr_stalls,
##                 data.frame(UN_ssa[!UN_ssa[["country_code"]] %in% sch_cc, ],
##                            Schoumaker_included = FALSE),
##                 all = TRUE, sort = FALSE)

###-----------------------------------------------------------------------------
### ** Sort

schoumaker_2019_tfr_stalls <-
    schoumaker_2019_tfr_stalls[with(schoumaker_2019_tfr_stalls,
                                    order(country_code, year)), ]

###-----------------------------------------------------------------------------
### * Outputs

## openxlsx::write.xlsx(schoumaker_2019_tfr_stalls,
##                      file = here::here("data-raw", "schoumaker_2019_tfr_stalls.xlsx"),
##                      asTable = TRUE, tableStyle = "TableStyleMedium2")

comment(schoumaker_2019_tfr_stalls) <-
    "TFR stalls identified by Schoumaker, B. (2019), \"Stalls in Fertility Transitions in sub-Saharan Africa: Revisiting the Evidence,\" Studies in Family Planning, 50, 257–278, Appendix B. https://doi.org/10/gf5zpt."

usethis::use_data(schoumaker_2019_tfr_stalls, internal = FALSE, overwrite = TRUE)
