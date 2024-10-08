% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longitudinal_table.R
\name{longitudinal_table}
\alias{longitudinal_table}
\title{Longitudinal Table of Standardized Scores}
\usage{
longitudinal_table(
  dat,
  fill_values = setNames(RColorBrewer::brewer.pal(n = 7, "RdYlGn"), nm = c("Impaired",
    "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"))
)
}
\arguments{
\item{dat}{Data to use. Should include one or more standardized cognitive variables.}

\item{fill_values}{a named vector of length seven with hex color values to use for categories
"Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"}
}
\description{
Create a table of standardized scores across multiple visits.
}
