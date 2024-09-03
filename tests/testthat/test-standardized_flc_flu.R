test_that(
  "standardized_flc_flu",
  {
    expect_equal(standardized_flc_flu(raw_score = 86, age = 85), 18)
    expect_equal(standardized_flc_flu(raw_score = 41, age = 67), 12)

    # Age group A
    all_standardized_scores <- tibble::tibble(
      raw = 0:75
    ) |>
      dplyr::mutate(
        A = dplyr::case_when(
          raw < 13 ~ 2,
          raw < 17 ~ 3,
          raw < 19 ~ 4,
          raw < 21 ~ 5,
          raw < 26 ~ 6,
          raw < 29 ~ 7,
          raw < 32 ~ 8,
          raw < 34 ~ 9,
          raw < 40 ~ 10,
          raw < 43 ~ 11,
          raw < 46 ~ 12,
          raw < 52 ~ 13,
          raw < 57 ~ 14,
          raw < 63 ~ 15,
          raw < 68 ~ 16,
          raw < 72 ~ 17,
          raw >=72 ~ 18,
          .default = NA
        ),
        B = dplyr::case_when(
          raw < 13 ~ 2,
          raw < 17 ~ 3,
          raw < 19 ~ 4,
          raw < 21 ~ 5,
          raw < 25 ~ 6,
          raw < 27 ~ 7,
          raw < 30 ~ 8,
          raw < 33 ~ 9,
          raw < 38 ~ 10,
          raw < 41 ~ 11,
          raw < 45 ~ 12,
          raw < 50 ~ 13,
          raw < 57 ~ 14,
          raw < 63 ~ 15,
          raw < 68 ~ 16,
          raw < 72 ~ 17,
          raw >=72 ~ 18,
          .default = NA
        ),
        C = dplyr::case_when(
          raw < 13 ~ 2,
          raw < 17 ~ 3,
          raw < 18 ~ 4,
          raw < 21 ~ 5,
          raw < 24 ~ 6,
          raw < 26 ~ 7,
          raw < 30 ~ 8,
          raw < 33 ~ 9,
          raw < 38 ~ 10,
          raw < 41 ~ 11,
          raw < 45 ~ 12,
          raw < 50 ~ 13,
          raw < 57 ~ 14,
          raw < 63 ~ 15,
          raw < 68 ~ 16,
          raw < 72 ~ 17,
          raw >=72 ~ 18,
          .default = NA
        ),
        D = dplyr::case_when(
          raw < 13 ~ 2,
          raw < 15 ~ 3,
          raw < 16 ~ 4,
          raw < 20 ~ 5,
          raw < 23 ~ 6,
          raw < 26 ~ 7,
          raw < 29 ~ 8,
          raw < 32 ~ 9,
          raw < 37 ~ 10,
          raw < 40 ~ 11,
          raw < 44 ~ 12,
          raw < 50 ~ 13,
          raw < 57 ~ 14,
          raw < 62 ~ 15,
          raw < 68 ~ 16,
          raw < 72 ~ 17,
          raw >= 72 ~ 18,
          .default = NA
        ),
        E = dplyr::case_when(
          raw < 12 ~ 2,
          raw < 13 ~ 3,
          raw < 16 ~ 4,
          raw < 20 ~ 5,
          raw < 23 ~ 6,
          raw < 25 ~ 7,
          raw < 29 ~ 8,
          raw < 32 ~ 9,
          raw < 37 ~ 10,
          raw < 40 ~ 11,
          raw < 44 ~ 12,
          raw < 50 ~ 13,
          raw < 57 ~ 14,
          raw < 62 ~ 15,
          raw < 67 ~ 16,
          raw < 71 ~ 17,
          raw >= 71 ~ 18,
          .default = NA
        ),
        F = dplyr::case_when(
          raw < 11 ~ 2,
          raw < 12 ~ 3,
          raw < 14 ~ 4,
          raw < 20 ~ 5,
          raw < 23 ~ 6,
          raw < 25 ~ 7,
          raw < 29 ~ 8,
          raw < 32 ~ 9,
          raw < 37 ~ 10,
          raw < 40 ~ 11,
          raw < 44 ~ 12,
          raw < 50 ~ 13,
          raw < 57 ~ 14,
          raw < 62 ~ 15,
          raw < 65 ~ 16,
          raw < 71 ~ 17,
          raw >= 71 ~ 18,
          .default = NA
        ),
        G = dplyr::case_when(
          raw < 6 ~ 2,
          raw < 9 ~ 3,
          raw < 11 ~ 4,
          raw < 16 ~ 5,
          raw < 22 ~ 6,
          raw < 24 ~ 7,
          raw < 27 ~ 8,
          raw < 31 ~ 9,
          raw < 36 ~ 10,
          raw < 40 ~ 11,
          raw < 44 ~ 12,
          raw < 49 ~ 13,
          raw < 53 ~ 14,
          raw < 62 ~ 15,
          raw < 65 ~ 16,
          raw < 71 ~ 17,
          raw >= 71 ~ 18,
          .default = NA
        ),
        H = dplyr::case_when(
          raw < 6 ~ 2,
          raw < 9 ~ 3,
          raw < 11 ~ 4,
          raw < 16 ~ 5,
          raw < 20 ~ 6,
          raw < 24 ~ 7,
          raw < 26 ~ 8,
          raw < 31 ~ 9,
          raw < 36 ~ 10,
          raw < 40 ~ 11,
          raw < 44 ~ 12,
          raw < 49 ~ 13,
          raw < 53 ~ 14,
          raw < 61 ~ 15,
          raw < 63 ~ 16,
          raw < 71 ~ 17,
          raw >= 71 ~ 18,
          .default = NA
        ),
        I = dplyr::case_when(
          raw < 6 ~ 2,
          raw < 7 ~ 3,
          raw < 11 ~ 4,
          raw < 16 ~ 5,
          raw < 19 ~ 6,
          raw < 23 ~ 7,
          raw < 26 ~ 8,
          raw < 30 ~ 9,
          raw < 36 ~ 10,
          raw < 40 ~ 11,
          raw < 43 ~ 12,
          raw < 49 ~ 13,
          raw < 53 ~ 14,
          raw < 60 ~ 15,
          raw < 63 ~ 16,
          raw < 68 ~ 17,
          raw >= 68 ~ 18,
          .default = NA
        ),
        J = dplyr::case_when(
          raw < 5 ~ 2,
          raw < 6 ~ 3,
          raw < 11 ~ 4,
          raw < 16 ~ 5,
          raw < 19 ~ 6,
          raw < 23 ~ 7,
          raw < 25 ~ 8,
          raw < 29 ~ 9,
          raw < 36 ~ 10,
          raw < 40 ~ 11,
          raw < 43 ~ 12,
          raw < 49 ~ 13,
          raw < 53 ~ 14,
          raw < 59 ~ 15,
          raw < 62 ~ 16,
          raw < 64 ~ 17,
          raw >= 64 ~ 18,
          .default = NA
        ),
        K = dplyr::case_when(
          raw < 5 ~ 2,
          raw < 6 ~ 3,
          raw < 11 ~ 4,
          raw < 16 ~ 5,
          raw < 19 ~ 6,
          raw < 23 ~ 7,
          raw < 24 ~ 8,
          raw < 27 ~ 9,
          raw < 35 ~ 10,
          raw < 38 ~ 11,
          raw < 42 ~ 12,
          raw < 49 ~ 13,
          raw < 52 ~ 14,
          raw < 57 ~ 15,
          raw < 58 ~ 16,
          raw < 59 ~ 17,
          raw >= 59 ~ 18,
          .default = NA
        )
      )

    expect_equal(
      all_standardized_scores$A,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(60, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$B,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(65, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$C,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(68, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$D,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(71, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$E,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(74, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$F,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(77, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$G,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(80, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$H,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(83, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$I,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(86, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$J,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(89, nrow(all_standardized_scores)))
    )

    expect_equal(
      all_standardized_scores$K,
      standardized_flc_flu(all_standardized_scores$raw, age = rep(92, nrow(all_standardized_scores)))
    )

  }
)

# cat("dplyr::case_when(\n", paste(paste("raw <", cowat_ranges$D[-1], "~", cowat_ranges$standardized_score[-nrow(cowat_ranges)]), collapse = ",\n"), "\n),")


