.onAttach <- function(libname, pkgname) {
    desc <- utils::packageDescription(pkgname, fields = c("Package", "Version", "Packaged", "Built"))
    packageStartupMessage(format(paste(paste(names(desc), desc, sep = ": " ), collapse = "\n")))
    return(invisible())
}


## Form taken from 'R Packages' by Hadley Wickham, 'https://r-pkgs.org/r.html'
.onLoad <- function(libname, pkgname) {

    ## -------* OPTIONS

    op <- options()
    op.tfrSURFs <- list(
        tfrSURFs.message_about_unknown_aes = TRUE,
        tfrSURFs.sensitivity_analysis_output_dir_name = "sensitivity_analysis_outputs",
        tfrSURFs.sensitivity_analysis_overwrite = FALSE,
        tfrSURFs.sim.dir = NULL,
        tfrSURFs.verbose = getOption("verbose")
    )
    toset <- !(names(op.tfrSURFs) %in% names(op))
    if(any(toset)) options(op.tfrSURFs[toset])

    return(invisible())
}
