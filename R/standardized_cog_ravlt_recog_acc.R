#' Get standardized RAVLT Recognition Accuracy score
#'
#' @param cog_ravlt_recog_acc Raw score as a percentage (0-100)
#' @param ravlt_recog_age_groups age group; a vector of same length as `cog_digsym`
#'   with entries as one of the following
#'    - 'AAAAA' for ages \[60,62\]
#'    - 'BBBBB' for ages \[63,65\]
#'    - 'CCCCC' for ages \[66,68\]
#'    - 'DDDDD' for ages \[69,71\]
#'    - 'EEEEE' for ages \[72,74\]
#'    - 'FFFFF' for ages \[75,77\]
#'    - 'GGGGG' for ages \[78,80\]
#'    - 'HHHHH' for ages \[81,83\]
#'    - 'IIIII' for ages \[84,86\]
#'    - 'JJJJJ' for ages >87
#' @param sex vector of same length as `cog_digsym` containing the corresponding
#'  sex of the participants. Each entry must be either "m" (male) or "f" (female)
#'
#' @returns A numeric vector of same length as `cog_ravlt_recog_acc` with the standardized scores.
#'
#' @examples
#' standardized_cog_ravlt_recog_acc(75, 'DDDDD', "m")
#'
#' @export

standardized_cog_ravlt_recog_acc <- function(
    cog_ravlt_recog_acc,
    ravlt_recog_age_groups,
    sex
) {

  stopifnot(length(cog_ravlt_recog_acc) == length(ravlt_recog_age_groups))
  stopifnot(is.numeric(cog_ravlt_recog_acc))
  stopifnot(is.character(ravlt_recog_age_groups))

  stopifnot(length(setdiff(sex, c("m", "f"))) == 0)

  ravlt_recog_ranges_wide <- ravlt_recog_ranges |>
    tidyr::pivot_wider(
      names_from = sex,
      values_from = -c(.data$standardized_score, .data$sex)
    )

  purrr::map(
    colnames(ravlt_recog_ranges_wide[,-1]),
    \(y) {
      tibble::tibble(
        {{ y }} := ravlt_recog_ranges_wide$standardized_score[
          findInterval(
            cog_ravlt_recog_acc,
            vec = ravlt_recog_ranges_wide[[y]]
          )
        ]
      )
    }
  ) |>
    purrr::list_cbind() |>
    dplyr::mutate(
      ravlt_recog_age_groups_sex = paste(ravlt_recog_age_groups, sex, sep = "_"),
      standardized = dplyr::case_match(
        .data$ravlt_recog_age_groups_sex,
        'AAAAA_m' ~ AAAAA_m,
        'BBBBB_m' ~ BBBBB_m,
        'CCCCC_m' ~ CCCCC_m,
        'DDDDD_m' ~ DDDDD_m,
        'EEEEE_m' ~ EEEEE_m,
        'FFFFF_m' ~ FFFFF_m,
        'GGGGG_m' ~ GGGGG_m,
        'HHHHH_m' ~ HHHHH_m,
        'IIIII_m' ~ IIIII_m,
        'JJJJJ_m' ~ JJJJJ_m,
        'AAAAA_f' ~ AAAAA_f,
        'BBBBB_f' ~ BBBBB_f,
        'CCCCC_f' ~ CCCCC_f,
        'DDDDD_f' ~ DDDDD_f,
        'EEEEE_f' ~ EEEEE_f,
        'FFFFF_f' ~ FFFFF_f,
        'GGGGG_f' ~ GGGGG_f,
        'HHHHH_f' ~ HHHHH_f,
        'IIIII_f' ~ IIIII_f,
        'JJJJJ_f' ~ JJJJJ_f,
        .default = NA
      )
    ) |>
    dplyr::pull(.data$standardized)
}


