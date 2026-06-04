@echo off
ECHO.
ECHO.

FOR %%I in (.) DO SET CurrDirName=%%~nxI

ECHO. ********************************************************************************
ECHO.
ECHO. PACKAGE NAME:  %CurrDirName%
ECHO. 
ECHO. ********************************************************************************

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.

ECHO. ================================================================================
ECHO. DATA
ECHO. ================================================================================
ECHO.
Rscript --verbose --no-save --no-restore -e "setwd('data-raw'); sink(tempfile()); example(source, echo = FALSE, verbose = FALSE); sink(); sourceDir('.')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. DOCUMENT
ECHO. ================================================================================
ECHO.
Rscript --verbose --no-save --no-restore -e "suppressPackageStartupMessages(devtools::document())"
if %ERRORLEVEL% GEQ 1 PAUSE
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. INSTALL 
ECHO. ================================================================================
ECHO.
CHDIR .. && R CMD INSTALL --build --install-tests %CurrDirName% && CHDIR %CurrDirName%
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. TESTS - testthat
ECHO. ================================================================================
ECHO.

Rscript --verbose --no-save --no-restore -e "testthat::test_package('%CurrDirName%')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. TESTS - Compare with archived results
ECHO. ================================================================================
ECHO.

CD _no_install\compare_results
Rscript --verbose "compare_results.R"
if %ERRORLEVEL% GEQ 1 PAUSE
CD ..\..

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. TESTS - slowTests
ECHO. ================================================================================
ECHO.

rem Rscript --verbose --no-save --no-restore -e "setwd('inst/slowTests'); for (nm in sort(list.files('.', pattern = '[.][RrSsQq]$'))) { source('0_setup.R'); if (!identical(nm, '0_setup.R')) source(file.path('.', nm)) }"
rem if %ERRORLEVEL% GEQ 1 PAUSE

CD inst\slowTests
FOR %%F IN (*.R) DO (
ECHO Running %%F
Rscript --verbose "%%F"
IF %ERRORLEVEL% GEQ 1 (
ECHO Error running %%F
PAUSE 
)
)
CD ..\..


ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.





ECHO. ================================================================================
PAUSE
