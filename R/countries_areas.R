################################################################################
###
### Functions related to countries, areas, country codes, etc.
###
################################################################################

###-----------------------------------------------------------------------------
### * Small Countries

##' Get and exclude countries with population less than 90000 in 2024
##'
##' @description
##'
##' \code{remove_small_countries} will remove countries with populations less
##' than 90000 in mid-2024 from the input. Demographic indicators for countries
##' with small populations often exhibit unique characteristics. It could be
##' useful to filter results for easier interpretation. Use
##' \code{get_small_countries} to see the country codes that will be excluded.
##'
##' There are methods for lists and vectors. In the former case, the list must
##' have country codes as names. Vectors must be numeric or character vectors of
##' three-digit numeric country codes.
##'
##' @param x A \code{\link{list}} or \code{\link{vector}}; see
##'     \dQuote{Description}.
##' @param ... Arguments passed to other methods.
##' @return \code{x} with small countries removed.
##' @author Mark C Wheldon
##'
##' @family Countries and areas
##'
##' @seealso UNlocations_countries
##'
##' @examples
##'
##' ## The country codes that will be excluded:
##' get_small_countries()
##'
##' @export
remove_small_countries <- function(x, ...) {
    UseMethod("remove_small_countries")
}

##' @rdname remove_small_countries
##' @export
remove_small_countries.list <- function(x) {
    keep <- which(names(x) %in% as.character(UNlocations_countries[!UNlocations_countries[["pop_lt_90k_2024"]], "country_code"]))
    NextMethod(keep = keep)
    ## if (!length(keep)) stop("'incl_small_countries' is 'TRUE'; no countries left.")
    ## return(x[keep]) # 'x' is a list with element names the 'country_code's, so subset on the codes.
}

##' @rdname remove_small_countries
##' @export
remove_small_countries.numeric <- function(x) {
    keep <- which(!(x %in% get_small_countries()))
    NextMethod(keep = keep)
    ## if (!length(keep)) stop("'incl_small_countries' is 'TRUE'; no countries to left.")
    ## return(x[keep])
}

##' @rdname remove_small_countries
##' @export
remove_small_countries.character <- function(x) {
    as.character(remove_small_countries(as.numeric(x)))
}

##' @rdname remove_small_countries
##' @export
remove_small_countries.default <- function(x, keep, ...) {
    if (!length(keep)) stop("'incl_small_countries' is 'FALSE'; no countries left.")
    return(x[keep])
}

##' @rdname remove_small_countries
##' @export
get_small_countries <- function() {
    UNlocations_countries[UNlocations_countries[["pop_lt_90k_2024"]], "country_code"]
}

##' @rdname remove_small_countries
##' @export
is_small_country <- function(code) {
    code %in% get_small_countries()
}

###-----------------------------------------------------------------------------
### * Conversions

##' Convert between country names and numeric codes
##'
##' Each country is assigned a three-digit numeric code by the M49 Standard
##' Country or Area Codes for Statistical Use (United Nations, 2018). These
##' functions provide for conversion between these codes and the country names
##' in English (see \dQuote{Note}). All names are the English names using only
##' ASCII characters. See the examples for two relevant cases.
##'
##' @param codes Numeric or character; three-digit numeric country codes.
##' @param names Character vector of official country names.
##' @return A vector of codes or names.
##' @author Mark C Wheldon
##'
##' @seealso UNlocations_countries
##'
##' @family Countries and areas
##'
##' @references
##' United Nations (2018) "Standard Country or Area Codes for Statistical Use (M49)", New York, N.Y.:
##' United Nations, Department of Economic and Social Affairs, Statistics
##' Division. \url{https://unstats.un.org/unsd/methodology/m49/} (accessed
##' 2025-06-18).
##'
##' @example
##'
##' ## Country names are in English, using only ASCII characters.
##' get_country_names(c(384, 638))
##'
##' ## Get all country names and codes
##' get_country_names()
##' get_country_codes()
##'
##' @export
get_country_names <- function(codes = NULL) {
    if (!is.null(codes)) return(UNlocations_countries[match(codes, UNlocations_countries[["country_code"]]), "name"])
    else return(UNlocations_countries[["name"]])
}

##' @rdname get_country_names
##' @export
get_country_codes <- function(names = NULL) {
    if (!is.null(names)) return(UNlocations_countries[match(tolower(names), tolower(UNlocations_countries[["name"]])), "country_code"])
    else return(UNlocations_countries[["country_code"]])
}

###-----------------------------------------------------------------------------
### * Validation

##' Validate numeric country codes
##'
##' Checks that numeric country codes are:
##'
##' 1. In the data set \code{\link{UNlocations_countries}}.
##' 2. In the \pkg{bayesTFR} output point to by \code{sim.dir}.
##'
##' @inheritParams make_tfr_surfs
##' @param country_codes Numeric vector of country codes to validate.
##' @return If valid, \code{country_codes} is returned; otherwise an error is signalled.
##' @author Mark C Wheldon
##'
##' @family Countries and areas
##'
##' @export
validate_country_codes <- function(country_codes, sim.dir) {

    ## Built-in table of countries and regions/subregions
    country_code_list <-
        UNlocations_countries[order(UNlocations_countries[["area_name"]],
                                    UNlocations_countries[["reg_name"]],
                                    UNlocations_countries[["name"]]), "country_code"]

    ## Check that 'country_codes' are in 'country_code_list'
    if (!is.null(country_codes)) {
        not_in <- country_codes[!country_codes %in% country_code_list]
        if (length(not_in))
            stop("The following country codes in 'country_codes' are not in the internal country code table: '",
                 toString(not_in), "'.")
    } else {
        ## 'Holy See' is in UNlocations_countries but not bayesTFR results
        country_codes <- country_code_list[country_code_list %in% bayesTFR::get.countries.table(bayesTFR::get.tfr.prediction(sim.dir = sim.dir))$code]
    }

    ## Check that all 'country_codes' are in the bayesTFR output
    not_in <- which(!country_codes %in% bayesTFR::get.countries.table(bayesTFR::get.tfr.prediction(sim.dir = sim.dir))$code)
    if (length(not_in)) {
        stop("The following country codes in 'country_codes' are not in the bayesTFR trajectories pointed to by 'sim.dir':\n",
             toString(country_codes[not_in]))
    }

    return(country_codes)
}
