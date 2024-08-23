#' Get standardized WAIS-R Digit Symbol score
#'
#' @param cog_digsym Raw score
#' @param digsym_age_groups age group; a vector of same length as `cog_digsym`
#' with entries as one of the following
#'    - 'AAAA' for ages 45-54
#'    - 'BBBB' for ages 55-64
#'    - 'CCCC' for ages 65-69
#'    - 'DDDD' for ages 70-74
#'    - 'EEEE' for ages 75-79
#'    - 'FFFF' for ages 80+
#'
#' @returns A numeric vector of same length as `cog_digsym` with the standardized scores.
#'
#' @examples
#' standardized_cog_digsym(24, 'DDDD')
#'
#' @export

standardized_cog_digsym <- function(
    cog_digsym,
    digsym_age_groups
) {

  stopifnot(length(cog_digsym) == length(digsym_age_groups))
  stopifnot(is.numeric(cog_digsym))
  stopifnot(is.character(digsym_age_groups))

  purrr::map(
    c("AAAA", "BBBB", "CCCC", "DDDD", "EEEE", "FFFF"),
    \(y) {
      tibble::tibble(
        {{ y }} := dig_symb_ranges$standardized[
          findInterval(
            cog_digsym,
            vec = dig_symb_ranges[[y]]
          )
        ]
      )
    }
  ) |>
    purrr::list_cbind() |>
    dplyr::mutate(
      digsym_age_groups,
      standardized = dplyr::case_match(
        .data$digsym_age_groups,
        'AAAA' ~ AAAA,
        'BBBB' ~ BBBB,
        'CCCC' ~ CCCC,
        'DDDD' ~ DDDD,
        'EEEE' ~ EEEE,
        'FFFF' ~ FFFF,
        .default = NA
      )
    ) |>
    dplyr::pull(.data$standardized)
}
