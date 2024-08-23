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

  ## Start 'out' data
  out <- dat |>
    dplyr::mutate(
      edu_group = c("A", "B", "C", "D")[findInterval(.data$cog_education, c(0, 13, 16, 17, Inf))],
      UDS_age_group = findInterval(.data$cog_age, c(0, 60, 70, 80, 90, Inf))
    )

  ## Male/female
  if (any(colnames(out) %in% unique(male_female$name))) {
    from_male_female <- out |>
      tidyr::pivot_longer(
        cols = tidyselect::any_of(unique(male_female$name)),
        names_to = "name",
        values_to = "raw"
      ) |>
      dplyr::left_join(
        male_female
      ) |>
      dplyr::mutate(
        standardized = (.data$raw - .data$m) / .data$sd
      ) |>
      dplyr::select(
        -.data$variable, -.data$m, -.data$sd
      ) |>
      tidyr::pivot_wider(
        names_from = .data$name,
        values_from = c(.data$raw, .data$standardized)
      )

    out <- dplyr::left_join(
      out |> dplyr::select(-tidyselect::any_of(unique(male_female$name))),
      from_male_female
    )
  }

  ## OTMT
  if (any(colnames(out) %in% unique(otmt$name))) {
    from_otmt <- out |>
      dplyr::select(
        .data$cog_studyid,
        .data$cog_test_date,
        .data$cog_sex,
        .data$cog_age,
        tidyselect::any_of(unique(otmt$name))
      ) |>
      dplyr::mutate(
        otmt_age_group = c("AA", "BB", "CC", "DD", "EE", "FF")[
          findInterval(.data$cog_age, c(20, 30, 40, 50, 60, 70, Inf))
        ]
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::any_of(unique(otmt$name)),
        names_to = "name",
        values_to = "raw"
      ) |>
      dplyr::left_join(
        otmt
      ) |>
      dplyr::mutate(
        standardized = (.data$raw - .data$m) / .data$sd
      ) |>
      dplyr::select(
        -.data$m, -.data$sd
      ) |>
      tidyr::pivot_wider(
        names_from = .data$name,
        values_from = c(.data$raw, .data$standardized)
      )

    out <- dplyr::left_join(
      out |> dplyr::select(-tidyselect::any_of(unique(otmt$name))),
      from_otmt
    )
  }

  ## F+L+C Fluency
  if ("cog_flc_flu" %in% colnames(out)) {
    out <- out |>
      dplyr::rename(
        raw_cog_flc_flu = .data$cog_flc_flu
      ) |>
      dplyr::mutate(
        cowat_age_groups = c("XXX","AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG", "HHH", "III", "JJJ", "KKK", "YYY")[
          findInterval(.data$cog_age, c(0, 56, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 97, Inf))
        ],
        ss_cog_flc_flu = standardized_cog_flc_flu(.data$raw_cog_flc_flu, .data$cowat_age_groups)
      )
  }

  ## Digit Symbol
  if ("cog_digsym" %in% colnames(out)) {
    out <- out |>
      dplyr::rename(
        raw_cog_digsym = .data$cog_digsym
      ) |>
      dplyr::mutate(
        digsym_age_groups = c("XXXX", "AAAA", "BBBB", "CCCC", "DDDD", "EEEE", "FFFF")[
          findInterval(.data$cog_age, c(0, 45, 55, 65, 70, 75, 80, Inf))
        ],
        ss_cog_digsym = standardized_cog_digsym(.data$raw_cog_digsym, .data$digsym_age_groups)
      )
  }

  ## RAVLT
  if ("cog_ravlt_a1" %in% colnames(out)) {
    out <- out |>
      dplyr::rename(
        raw_cog_ravlt_recog_acc = .data$cog_ravlt_recog_acc
      ) |>
      dplyr::mutate(
        ravlt_age_groups = findInterval(.data$cog_age, c(16, 20, 30, 40, 50, 60, 70, 80)),
        ravlt_recog_age_groups = c("XXXXX",
                                   "AAAAA",
                                   "BBBBB",
                                   "CCCCC",
                                   "DDDDD",
                                   "EEEEE",
                                   "FFFFF",
                                   "GGGGG",
                                   "HHHHH",
                                   "IIIII",
                                   "JJJJJ")[findInterval(.data$cog_age, c(0,60,63,66,69,72,75,78,81,84,87,Inf))],
        ss_cog_ravlt_recog_acc = standardized_cog_ravlt_recog_acc(
          .data$raw_cog_ravlt_recog_acc,
          .data$ravlt_recog_age_groups,
          sex = dplyr::case_match(.data$cog_sex, "Male" ~ "m", "Female" ~ "f", .default = NA)
        )
      )

    ravlt_trials <- out |>
      dplyr::select(
        .data$cog_studyid,
        .data$cog_test_date,
        .data$ravlt_age_groups,
        tidyselect::any_of(unique(ravlt_trials_m_sd$name))
      ) |>
      tidyr::pivot_longer(
        tidyselect::any_of(unique(ravlt_trials_m_sd$name)),
        names_to = "name",
        values_to = "raw"
      ) |>
      dplyr::left_join(
        ravlt_trials_m_sd
      ) |>
      dplyr::mutate(
        standardized = (.data$raw - .data$m) / .data$sd
      ) |>
      dplyr::select(
        -.data$m, -.data$sd
      ) |>
      tidyr::pivot_wider(
        names_from = .data$name,
        values_from = c(.data$raw, .data$standardized)
      ) |>
      dplyr::filter(
        dplyr::if_any(
          tidyselect::everything(), \(x) length(x) > 1
        )
      )

    out <- dplyr::left_join(
      out |> dplyr::select(-tidyselect::any_of(unique(ravlt_trials_m_sd$name))),
      ravlt_trials
    )

  }

  out

}
