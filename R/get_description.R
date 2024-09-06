#' Get Description
#'
#' @param raw Raw score of cognitive variable
#' @param z_score z-scores corresponding to raw scores
#' @param ss Standardized scores corresponding to raw scores
#' @param cog_var Name of cognitive variable
#'
get_descriptions <- function(raw, z_score, ss, cog_var) {
  percentiles <- get_percentiles(z_score, ss, cog_var)

  dplyr::case_when(
    raw %in% c(995, 996, 997, 998, 999) ~ paste0("Unavailable (", raw, ")"),
    cog_var == "cog_cdr_global" ~ c("Normal", "Very Mild", "Mild" ,"Moderate", "Severe")[
      findInterval(raw, c(0, 2.5, 4.5, 9.5, 16, 18.5, Inf))
    ],
    cog_var == "cog_gds15" ~ c("Minimal", "Mild", "Moderate", "Severe")[
      findInterval(raw, c(0, 5, 9, 12, 16, Inf))
    ],
    cog_var == "cog_moca_clock" ~ rep(c("Impaired", "Normal"), c(3,1))[raw + 1],
    .default = c("Impaired",
                 "Borderline",
                 "Low Average",
                 "Average",
                 "High Average",
                 "Superior")[findInterval(percentiles,
                                          c(0, 0.03, 0.10, 0.25, 0.76, 0.92, Inf))]
  )

}
