#' Standardize Raw Cognitive Scores to Z-Scores
#'
#' Calculate z-scores as `(raw score - average) / sd`. Available for a wide array
#' of cognitive scores.
#'
#' @param raw_scores Numeric vector of raw scores
#' @param cog_var_name String with name of the variable. Used to get correct means and standard deviations.
#' @param education Numeric vector with years of education. Used for subset of variables only.
#' @param age Numeric vector with ages in years
#' @param sex Character vector with sex of participants. Must be either "m" (for male) or "f" (for female). Used for subest of variables only
#'
#' @export

standardize_to_z_scores <- function(
    raw_scores,
    cog_var_name,
    education,
    age,
    sex
) {

  ## Extract means and SDs for this particular variable
  m_sd <- for_zscores[[cog_var_name]]

  ## Find variables needed for matching
  # Age: always needed, but different group types

  # Get column name of age_group.
  age_group_type_needed <- colnames(m_sd)[grep("*age*", colnames(m_sd))]

  # Create match_to data.frame with two columns: raw_scores and age groups. (We
  # will later add means and SDs to this data.frame for standardizing raw scores.)
  match_to <- data.frame(
    raw_scores,
    age_group = get_age_group(age, group_type = stringr::str_remove(age_group_type_needed, "_age_group"))
  )

  # Change name of age_group variable to match the age group variable in m_sd
  colnames(match_to)[which(colnames(match_to) == "age_group")] <- age_group_type_needed

  # Add age group variable to vector of variables to join by later.
  join_by_vars <- age_group_type_needed

  # Sex:
  if (any(grepl("sex", colnames(m_sd)))) {
    # Some checks.
    stopifnot("'sex' is missing, but needed to standardize this variable" = !missingArg(sex))
    stopifnot("'sex' must be same length as 'raw_scores'" = length(sex) == length(raw_scores))
    stopifnot("'sex' must be vector of 'm' and 'f'" = length(setdiff(sex, c("f", "m"))) == 0)

    # Add sex to match_to data.frame
    match_to$sex <- sex

    # Add "sex" to variables to join by later
    join_by_vars <- c(join_by_vars, "sex")
  }

  # Education:
  if (any("edu_group" == colnames(m_sd))) {
    # Some checks
    stopifnot("'eduction' is missing, but needed to standardize this variable" = !missingArg(education))
    stopifnot("'eduction' must be same length as 'raw_scores'" = length(education) == length(raw_scores))
    stopifnot("'education' must be numeric vector" = is.numeric(education))

    # Add education groups to match_to data.frame. These are from "male" and "female"
    # sheets of Excel spreadsheet.
    match_to$edu_group <- c("A", "B", "C", "D")[findInterval(education, c(0, 13, 16, 17))]

    # Add "edu_group" to variables to join by momentarily
    join_by_vars <- c(join_by_vars, "edu_group")
  }

  # Add means and standard deviations to data.frame so we can standardize raw scores
  for_standardizing <- dplyr::left_join(
    match_to,
    m_sd,
    by = join_by_vars
  )

  # Return standardized scores
  with(for_standardizing, (raw_scores - m) / sd)

}



