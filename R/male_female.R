#' Various Cognition Measures: means and standard deviations
#'
#' Averages and standard deviations for a long list of variables. These are
#' by gender, education level, and age group.
#'
#' @format ## `male_female`
#' \describe{
#'    \item{variable}{Variable name}
#'    \item{name}{Variable name in WLS REDCap database}
#'    \item{UDS_age_group}{Age groups. 1: <60, 2: 60-69, 3: 70-79, 4: 80-89, 5: >90}
#'    \item{edu_group}{Education group by years of education. A: <12, B: 13-15, C: 16, D: >16}
#'    \item{cog_sex}{Sex group}
#'    \item{m}{Mean of `name` for age group `UDS_age_group`, eduction group `education`, and gender `cog_sex`}
#'    \item{sd}{Standard deviation of `name` for age group `UDS_age_group`, eduction group `education`, and gender `cog_sex`}
#' }
#'
"male_female"
