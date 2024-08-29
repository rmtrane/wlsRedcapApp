# NACC T-Cog Neuropsychological Assessment
Ralph Møller Trane

This R package provides functions to work with outcomes from the NACC
T-Cog Neuropsychological Assessment. The main purpose is the creation of
a summary table used when dementia diagnoses are decided on through
consensus (see table below[^1])

``` r
library(wlsRedcapApp)

main_table(
  demo_data, 
  studyid = "326231g", 
  cur_date = "8/9/2021"
)
```

![](man/figures/home_page_table.png)

<!-- <div align="center"> -->
<!-- <img src="man/figures/home_page_table.png"> -->
<!-- </div> -->

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

[^1]: Note that Percentile numbers are rendered weird on this front
    page. For more accurate representation of the table, see the
    `main_table()` reference page
