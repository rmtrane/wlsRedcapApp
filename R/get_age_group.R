#' Get Age Group for Standardized Scores
#'
#' Get various versions of age groups for finding standardized scores.
#'
#' @param age numeric vector with ages to be converted to groups
#' @param group_type string indicator which age group to return. Must be one of
#'   "ravlt_trials", "ravlt_recog", "UDS_battery", "otmt", "cowat", "digsym"
#'
#' @export
get_age_group <- function(
    age,
    group_type = c(
      "ravlt_trials",
      "ravlt_recog",
      "UDS_battery",
      "otmt",
      "cowat",
      "digsym"
    )
) {

  stopifnot("'age' must be numeric vector" = is.numeric(age))
  stopifnot("'group_type' must be one of 'ravlt_trials', 'ravlt_recog', 'UDS_battery', 'otmt', 'cowat', 'digsym'" =
              any(group_type == c(
                "ravlt_trials",
                "ravlt_recog",
                "UDS_battery",
                "otmt",
                "cowat",
                "digsym"
              )))
  stopifnot("'group_type' not of length 1" = length(group_type) == 1)

  ## From Excel sheet "Summary", cell BI25. Cutoffs in array in formula.
  if (group_type == "ravlt_trials") {
    out <- findInterval(age, c(16, 20, 30, 40, 50, 60, 70, 80))
    # out <- c(NA, LETTERS[1:9])[out + 1]

    return(out)
  }

  # From hidden column G of Excel sheet "RawScores"
  if (group_type == "ravlt_recog") {
    out <- findInterval(age, c(60,63,66,69,72,75,78,81,84,87))
    out <- c(NA, LETTERS[1:10])[out+1]

    return(out)
  }

  # From Excel sheets "male" and "female"
  if (group_type == "UDS_battery") {
    out <- findInterval(age, c(60, 70, 80, 90)) + 1

    return(out)
  }

  # From Mrazik et al. (2010). Note: original groups overlapped.
  if (group_type == "otmt") {
    out <- findInterval(age, c(20, 30, 40, 50, 60, 70))
    out <- c(NA, LETTERS[1:6])[out+1]

    return(out)
  }

  # From Excel sheet "COWAT"
  if (group_type == "cowat") {
    out <- findInterval(age, c(56, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 97))
    out <- c(NA, LETTERS[1:11], NA)[out+1]

    return(out)
  }

  # From Excel sheet "Dig-Symb"
  if (group_type == "digsym") {
    out <- findInterval(age, c(45, 55, 65, 70, 75, 80))
    out <- c(NA, LETTERS[1:6])[out+1]

    return(out)
  }

}
