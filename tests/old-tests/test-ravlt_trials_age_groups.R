test_that("ravlt_trials_age_groups", {
  test_tibble <- tibble::tibble(
    age = 5:85,
    age_groups = ravlt_trials_age_groups(age),
    age_groups_letters = ravlt_trials_age_groups(age, return_letter_group = T)
  ) |>
    dplyr::summarize(
      min = min(age),
      max = max(age),
      .by = tidyselect::starts_with("age_groups")
    ) |>
    dplyr::arrange(age_groups)

  result_tibble <- tibble::tibble(
    age = 5:85,
    age_groups = case_when(
      age < 16 ~ 0,
      age < 20 ~ 1,
      age < 30 ~ 2,
      age < 40 ~ 3,
      age < 50 ~ 4,
      age < 60 ~ 5,
      age < 70 ~ 6,
      age < 80 ~ 7,
      age >= 80 ~ 8,
      .default = NA
    ),
    age_groups_letters = case_when(
      age < 16 ~ NA,
      age < 20 ~ "A",
      age < 30 ~ "B",
      age < 40 ~ "C",
      age < 50 ~ "D",
      age < 60 ~ "E",
      age < 70 ~ "F",
      age < 80 ~ "G",
      age >= 80 ~ "H",
      .default = NA
    )
  ) |>
    dplyr::summarize(
      min = min(age),
      max = max(age),
      .by = tidyselect::starts_with("age_groups")
    ) |>
    dplyr::arrange(age_groups)

  expect_equal(test_tibble, result_tibble)

})
