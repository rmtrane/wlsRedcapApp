% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_to_z_scores.R
\name{standardize_to_z_scores}
\alias{standardize_to_z_scores}
\title{Standardize Raw Cognitive Scores to Z-Scores}
\usage{
standardize_to_z_scores(raw_scores, cog_var_name, education, age, sex)
}
\arguments{
\item{raw_scores}{Numeric vector of raw scores}

\item{cog_var_name}{String with name of the variable. Used to get correct means and standard deviations.}

\item{education}{Numeric vector with years of education. Used for subset of variables only.}

\item{age}{Numeric vector with ages in years}

\item{sex}{Character vector with sex of participants. Must be either "m" (for male) or "f" (for female). Used for subest of variables only}
}
\description{
Calculate z-scores as \verb{(raw score - average) / sd}. Available for a wide array
of cognitive scores.
}
