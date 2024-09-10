#' Get Description
#'
#' @param raw Raw score of cognitive variable
#' @param z_score z-scores corresponding to raw scores
#' @param ss Standardized scores corresponding to raw scores
#' @param cog_var Name of cognitive variable
#'
#' @keywords internal
get_descriptions <- function(raw, z_score, ss, cog_var) {

  if (missingArg(raw)) {
    raw_length <- 0
  } else {
    raw_length <- length(raw)

    if (raw_length == 1 && is.na(raw))
      raw_length <- 0
  }

  if (missingArg(z_score)) {
    z_score_length <- 0
  } else {
    z_score_length <- length(z_score)

    if (z_score_length == 1 && is.na(z_score))
      z_score_length <- 0
  }

  if (missingArg(ss)) {
    ss_length <- 0
  } else {
    ss_length <- length(ss)

    if (ss_length == 1 && is.na(ss))
      ss_length <- 0
  }

  lns <- c(raw_length, z_score_length, ss_length)

  stopifnot("raw, z_score, and ss should either be NA or vectors of the same
            length" = setdiff(lns, 0) != 1)

  max_ln <- max(lns)

  if (raw_length == 0)
    raw <- rep(NA, max_ln)

  if (z_score_length == 0)
    z_score <- rep(NA, max_ln)

  if (ss_length == 0)
    ss <- rep(NA, max_ln)

  percentiles <- get_percentiles(z_score, ss, cog_var)

  dplyr::case_when(
    raw %in% c(995, 996, 997, 998, 999) ~ paste0("Unavailable (", raw, ")"),
    cog_var == "cog_cdr_global" ~ c("Normal", "Very Mild", "Mild" ,"Moderate", "Severe")[
      findInterval(raw, c(2.5, 4.5, 9.5, 16, 18.5)) + 1
    ],
    cog_var == "cog_gds15" ~ c("Minimal", "Mild", "Moderate", "Severe")[
      findInterval(raw, c(5, 9, 12)) + 1
    ],
    cog_var == "cog_moca_clock" ~ rep(c("Impaired", "Normal"), c(3,1))[raw + 1],
    .default = c("Impaired",
                 "Borderline",
                 "Low Average",
                 "Average",
                 "High Average",
                 "Superior")[findInterval(percentiles, c(0.03, 0.10, 0.25, 0.76, 0.92)) + 1]
  )

}
