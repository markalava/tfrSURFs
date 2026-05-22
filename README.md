# tfrSURFs

This package produces estimates and projections of *S*talls, *U*pswings, and *R*eversals in *F*ertility transitions (SURFs) from probabilistic estimates and projections of total fertility rates (TFRs). It accompanies the manuscript "Caught in Transit: Identifying Stalls, Upswings and Reversals in Fertility Transitions For All Countries using a Probabilistic Approach", available [here](https://doi.org/10.31235/osf.io/u6r7n_v2 "Link to preprint of 'Caught in Transit: Identifying ...'"). 

*tfrSURFs* is a package for the *R Environment for Statistical Computing* (R Core Team 2025). If you do not have *R* on your system you will need to download and install it before attempting to use *tfrSURFs*. Information about *R*, including links to download it, can be found [here](https://www.r-project.org/ "Official homepage of the R Project"). *tfrSURFs* was developed using *R* v. 4.5.1. 


## Installation

Install the latest version directly from within *R* using the *devtools* package:

    ```
    install.packages("devtools")
    devtools::install_github("https://github.com/markalava/tfrSURFs", build_manual = TRUE, build_vignettes = TRUE)
    ```

Alternatively, visit the [Releases](https://github.com/markalava/tfrSURFs/releases "'Releases' page of the tfrSURFs package") page and download the source code of a specific version. See the *R* help page for `install.packages()` for instructions on how to install the package from source.
       

## Using the Package

A short user guide is available in the vignette *tfrSURFs*. To read it, install the package and issue the following command from within *R*:

```
vignette("tfrSURFs", package = "tfrSURFs")
```

*R* scripts to reproduce the main results and sensitivity analysis discussed in the manuscript ["Caught in Transit ... "](https://doi.org/10.31235/osf.io/u6r7n_v2 "Link to preprint of 'Caught in Transit: Identifying ...'") can be accessed with the following commands:

```{r}
system.file("manuscript_2026", "main_results.R", package = "tfrSURFs")
system.file("manuscript_2026", "sensitivity_analyses.R", package = "tfrSURFs")
```


## References

R Core Team. 2025. *R: A Language and Environment for Statistical Computing*. Vienna, Austria: R Foundation for Statistical Computing.


## Disclaimer

The views expressed herein are those of the authors and do not necessarily reflect the views of the United Nations. 
