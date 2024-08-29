#' Get standardized F+L+C Words score
#'
#' This function provides standardized F+L+C scores given the raw score and age group.
#'
#' @param raw_score numeric vector
#' @param age numeric vector of same length as `raw_score` with ages
#'
#' @returns A numeric vector of same length as `raw_score` with the standardized scores.
#'
#' @examples
#' standardized_flc_flu(20, age = 70)
#'
#' @export

standardized_flc_flu <- function(
    raw_score,
    age
) {
  ## Checks
  stopifnot(length(raw_score) == length(age))
  stopifnot(is.numeric(raw_score))
  stopifnot(is.numeric(age))

  ## Get age groups
  age_groups <- get_age_group(age, "cowat")

  ## Get vector of age groups present (no need for calculating SS for age groups
  ## not in data anyway)
  age_groups_present <- na.omit(unique(age_groups))

  ## For each participant, find SS for each age group present, regardless of
  ## participants actual age group. (This is faster!)
  all_ss <- lapply(age_groups_present, \(y) {
    cowat_ranges$standardized_score[
      findInterval(
        raw_score,
        vec = cowat_ranges[[y]]
      )
    ]
  })

  ## Set names of list of all SS. Now, all_ss$A[i] is the SS of participant i
  ## had participant been in age group A. (Note: A will only be in all_ss if
  ## at least one participant is in age group A)
  all_ss <- setNames(all_ss, age_groups_present)

  ## Pull SS for each participant from the age group they belong to
  sapply(1:length(raw_score), \(i) ifelse(is.na(age_groups[i]), NA, all_ss[[age_groups[i]]][i]))
}
