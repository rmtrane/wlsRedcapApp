#' Means and SDs for calculating z-scores
#'
#' @format ## `for_zscores`
#' \describe{A list of tibbles. Each list entry correspond to a variable
#'   that we can standardize to z-scores. Each list entry holds a tibble organized
#'   as follows:
#'   \item{ravlt_trials_age_group/UDS_battery_age_group/otmt_age_group}{the appropriate age group. The function `get_age_group` can be used to convert
#'   an actual age to the corresponding age group.}
#'   \item{edu_group}{the education group based on years of education as follows: A if 0-12 years, B if 13-15, C if 16, and D if >= 17}
#'   \item{m}{mean for variable in group given by age group (and possible sex and education group)}
#'   \item{sd}{standard deviation}
#' }
#'
#'
"for_zscores"
