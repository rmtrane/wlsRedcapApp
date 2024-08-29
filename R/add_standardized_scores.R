#' Add Standardized Scores to Data from REDCap
#'
#' This functions adds a plethora of standardized scores along with some grouping
#' variables needed to calculate the standardized scores.
#'
#' @param dat `tibble` or `data.frame` that includes, at the very least, the following:
#'    - `cog_studyid`
#'    - `cog_test_date`
#'    - `cog_sex`
#'    - `cog_education`
#'    - `cog_age`
#'    - any number of variables that are present in `male_female` or `otmt` or `ravlt_trials_m_sd`
#'
#' @export

add_standardized_scores <- function(dat) {
  out <- dat

  ## If any variables present for which we get z-scores, calculate z-scores
  if (any(colnames(dat) %in% names(for_zscores))){
    out <- out |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::any_of(names(for_zscores)),
          .fns = \(x) {
            standardize_to_z_scores(
              x,
              dplyr::cur_column(),
              age = .data$cog_age,
              sex = dplyr::case_match(.data$cog_sex, "Male" ~ "m", "Female" ~ "f", .default = NA),
              education = .data$cog_education
            )
          },
          .names = "standardized_{.col}"
        )
      )

    ## Add prefix "raw" to variable names
    wh <- which(colnames(out) %in% names(for_zscores))
    colnames(out)[wh] <- paste("raw", colnames(out)[wh], sep = "_")
  }

  ## If 'cog_ravlt_recog_acc' in data, get SS for this var
  if (any("cog_ravlt_recog_acc" == colnames(dat))) {
    out$ss_cog_ravlt_recog_acc <- standardized_ravlt_recog_acc(
      raw_score = dat$cog_ravlt_recog_acc,
      age = dat$cog_age,
      sex = dplyr::case_match(dat$cog_sex, "Male" ~ "m", "Female" ~ "f", .default = NA)
    )

    ## Add prefix "raw" to variable name
    colnames(out)[colnames(out) == "cog_ravlt_recog_acc"] <- "raw_cog_ravlt_recog_acc"
  }

  ## If 'cog_digsym' in data, get SS for this var
  if (any("cog_digsym" == colnames(dat))) {
    out$ss_cog_digsym <- standardized_digsym(
      raw_score = out$cog_digsym,
      age = out$cog_age
    )

    ## Add prefix "raw" to variable name
    colnames(out)[colnames(out) == "cog_digsym"] <- "raw_cog_digsym"
  }

  if (any("cog_flc_flu" == colnames(dat))) {
    out$ss_cog_flc_flu <- standardized_flc_flu(out$cog_flc_flu, out$cog_age)

    ## Add prefix "raw" to variable name
    colnames(out)[colnames(out) == "cog_flc_flu"] <- "raw_cog_flc_flu"
  }

  out
}

