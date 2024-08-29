#' Get standardized RAVLT Recognition Accuracy score
#'
#' @param raw_score Raw score as a percentage (0-100)
#' @param age numeric vector of same length as `raw_score`
#' @param sex vector of same length as `raw_score` containing the corresponding
#'  sex of the participants. Each entry must be either "m" (male) or "f" (female)
#'
#' @returns A numeric vector of same length as `raw_score` with the standardized scores.
#'
#' @examples
#' standardized_ravlt_recog_acc(raw_score = 75, age = 60, sex = "m")
#'
#' @export

standardized_ravlt_recog_acc <- function(
    raw_score,
    age,
    sex
) {

  stopifnot(length(raw_score) == length(age))
  stopifnot(length(raw_score) == length(sex))
  stopifnot(is.numeric(raw_score))
  stopifnot(is.numeric(age))

  stopifnot("Raw scores outside the range 0 to 100 not allowed" = all((raw_score <= 100 & raw_score >= 0) | is.na(raw_score)))
  stopifnot(length(setdiff(sex, c("m", "f"))) == 0)

  stopifnot("Missing values in 'age' not allowed" = sum(is.na(age)) == 0)

  ## First, get age groups
  ravlt_recog_age_groups <- get_age_group(age, "ravlt_recog")

  ## Get distinct age groups present
  age_groups_present <- na.omit(unique(ravlt_recog_age_groups))

  ## Second, get SS for all participants for each group, regardless of where they
  ## belong. We set up a 3-dimensional array to hold the results.
  age_sex_ss <- array(
    data = NA,
    dim = c(length(age), length(age_groups_present), 2),
    dimnames = list(
      part_index = 1:length(age),
      age_group = age_groups_present,
      sex = c("m", "f")
    )
  )

  ## For each possible sex and each possible age group, get standardized scores
  ## for all participants.
  for (s in c("m", "f")) {
    for (ag in age_groups_present) {
      age_sex_ss[,ag,s] <- ravlt_recog_ranges$standardized_score[
        findInterval(
          raw_score,
          vec = subset(ravlt_recog_ranges, sex == s)[[ag]]
        )
      ]
    }
  }
  ## Note, age_sex_ss[i, ag, s] is the SS for i'th entry if age group were ag
  ## and sex were s.

  ## Pull out the right scores for each participant.
  sapply(1:length(age), \(i) ifelse(is.na(ravlt_recog_age_groups[i]), NA, age_sex_ss[i, ravlt_recog_age_groups[i], sex[i]]))
}

