# R package for Working with NACC T-Cog Neuropsychological Assessments
Ralph Møller Trane

## Get Started

Install this package from GitHub:

``` r
remotes::install_github("rmtrane/wlsRedcapApp")
```

The main Shiny application, which allows you to sift through summary
tables for all participants can be launched by first loaded the package,
then calling `wlsRedcapApp()`.

``` r
library(wlsRedcapApp)

wlsRedcapApp()
```

This package includes a demo data set (`demo_data`), which allows you to
explore the functionality of the package and in particular the Shiny
app. Not that these data might seem completely made up… because they
are. Visits might be out of order (as in, later visits have dates
earlier than first visits), and outcomes might seem contradictory. These
data were created by scrambling actual PHI data, and the sole purpose is
testing and showcasing this R package. If you have access to the WLS
REDCap project, you can download the full data from REDCap and choose to
use these data in the Shiny app. Simply follow the instructions in the
app.

For more information, see https://rmtrane.github.io/wlsRedcapApp

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmtrane/wlsRedcapApp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmtrane/wlsRedcapApp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
