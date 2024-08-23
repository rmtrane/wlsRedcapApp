#' Get standardized F+L+C Words score
#'
#' This function provides standardized F+L+C scores given the raw score and age group.
#'
#' @param cog_flc_flu Raw F+l+C score
#' @param cowat_age_groups COWAT age group; one of the following
#'    - 'AAA' for ages 56-62
#'    - 'BBB' for ages 63-65
#'    - 'CCC' for ages 66-68
#'    - 'DDD' for ages 69-71
#'    - 'EEE' for ages 72-74
#'    - 'FFF' for ages 75-77
#'    - 'GGG' for ages 78-80
#'    - 'HHH' for ages 81-83
#'    - 'III' for ages 84-86
#'    - 'JJJ' for ages 87-89
#'    - 'KKK' for ages 90-97
#'
#' @returns A numeric vector of same length as `cog_flc_flu` with the standardized scores.
#'
#' @examples
#' standardized_cog_flc_flu(20, 'HHH')
#'
#' @export

standardized_cog_flc_flu <- function(
    cog_flc_flu,
    cowat_age_groups
) {

  stopifnot(length(cog_flc_flu) == length(cowat_age_groups))
  stopifnot(is.numeric(cog_flc_flu))
  stopifnot(is.character(cowat_age_groups))

  purrr::map(
    c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG", "HHH", "III", "JJJ", "KKK"),
    \(y) {
      tibble::tibble(
        {{ y }} := cowat_ranges$standardized_score[
          findInterval(
            cog_flc_flu,
            vec = cowat_ranges[[y]]
          )
        ]
      )
    }
  ) |>
    purrr::list_cbind() |>
    dplyr::mutate(
      i = dplyr::row_number(),
      cowat_age_groups
    ) |>
    dplyr::mutate(
      standardized = dplyr::case_match(
        .data$cowat_age_groups,
        'AAA' ~ AAA,
        'BBB' ~ BBB,
        'CCC' ~ CCC,
        'DDD' ~ DDD,
        'EEE' ~ EEE,
        'FFF' ~ FFF,
        'GGG' ~ GGG,
        'HHH' ~ HHH,
        'III' ~ III,
        'JJJ' ~ JJJ,
        'KKK' ~ KKK,
        .default = NA
      )
    ) |>
    dplyr::pull(.data$standardized)
}
