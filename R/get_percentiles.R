#' Get Percentiles
#'
#' Get percentiles from z-scores, Standardized Scores and name of cognitive variable.
#'
#' @param z_score numeric vector with z-scores
#' @param ss numeric vector with standardized scores
#' @param cog_var character vector indicating what cognitive score each entry of
#'   `z_score` and `ss` correspond to.
#'
get_percentiles <- function(z_score, ss, cog_var) {
  stopifnot('"cog_var" must be given' = !missingArg(z_score))
  stopifnot('"cog_var" must either be length one or same length as "z_score"' = length(cog_var) %in% c(1, length(z_score)))


  if (missingArg(z_score))
    z_score <- NA

  if (missingArg(ss))
    ss <- NA

  dplyr::case_when(
    # For time and number of errors, we want right-tailed probabilities
    cog_var %in% c("cog_tmta_time", "cog_tmtb_time",
                      "cog_otmta_time", "cog_otmtb_time",
                      "cog_otmta_error", "cog_otmtb_error") ~ 1 - pnorm(z_score),
    # For other standardized scores, left tailed probabilities:
    cog_var %in% names(for_zscores) ~ pnorm(z_score),
    # Percentiles from SS
    cog_var %in% c("cog_flc_flu", "cog_digsym", "cog_ravlt_recog_acc") ~
      c(1,1,1,1,2,5,9,16,25,37,50,63,75,84,91,95,98,99,99,99)[pmax(1, ss + 1)]/100,
    .default = NA
  )
}
