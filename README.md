# tfrSURFs

This package produces estimates and projections of *S*talls, *U*pswings, and *R*eversals in *F*ertility trends (SURFs) from probabilistic estimates and projections of total fertility rates (TFRs). It accompanies the manuscript "Caught in Transit: Identifying Stalls, Upswings and Reversals in Fertility Transitions For All Countries using a Probabilistic Approach", available [here](https://doi.org/10.31235/osf.io/u6r7n_v2 "Link to preprint of 'Caught in Transit: Identifying ...'"). 

*tfrSURFs* is a package for the *R Environment for Statistical Computing* (R Core Team 2025). If you do not have *R* on your system you will need to download and install it before attempting to use *tfrSURFs*. Information about *R*, including links to download it, can be found [here](https://www.r-project.org/ "Official homepage of the R Project"). *tfrSURFs* was developed using *R* v. 4.5.1. 


## Installation

### Either ... Directly from GitHub using *devtools*

From within *R*:

1. Install the *devtools* package:

    ```
    install.packages("devtools")
    ```
    
2. Install *tfrSURFs*:

    ```
    devtools::install_github("https://github.com/markalava/tfrSURFs", build_manual = TRUE, build_vignettes = TRUE)
    ```

    
### Or ... From Source

1. Clone this repository to your local drive ([how?](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository "GitHub help on cloning repositories")) or download the Source code from the [Releases](https://github.com/markalava/tfrSURFs/releases "Releases page of the tfrSURFs package") page.

2. Open a command line terminal (sometimes called a console on Windows) and navigate to the directory containing the "tfrSURFs" directory (this might involve the use of the `cd` or `chdir` commands). Issue the following at the command line. You will need to modify the second command by replacing "\<version\>" to match the file produced by the first:
       
   ```
   R CMD build tfrSURFs
   R CMD INSTALL tfrSURFs_<version>.tar.gz # NOTE: Replace '<version>' with the actual version number
   ```
       

## Using the Package

A short user guide is available in the vignette *tfrSURFs*. To read it, install the package and issue the following command from within *R*:

```
vignette("tfrSURFs", package = "tfrSURFs")
```

Code to regenerate the data and results presented in "Caught in Transit: Identifying Stalls, Upswings and Reversals in Fertility Transitions For All Countries using a Probabilistic Approach" is bundled with the package. Once you have installed the package, you can request the path to the local copy on your system by issuing the following command from within *R*. Copy this file path from the *R* console and access the file as you would any other according to your operating system. 

```
system.file("manuscript/main_results.R", package = "tfrSURFs")
```


## References

R Core Team. 2025. *R: A Language and Environment for Statistical Computing*. Vienna, Austria: R Foundation for Statistical Computing.


## Disclaimer

The views expressed herein are those of the authors and do not necessarily reflect the views of the United Nations. 
