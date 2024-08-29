test_that("Test get_age_group", {

  age_range_to_test <- 0:110

  test_tibble <- tibble::tibble(
    age = age_range_to_test,
    otmt_age_groups = get_age_group(age, group_type = "otmt"),
    ravlt_trials_age_groups = get_age_group(age, group_type = "ravlt_trials"),
    ravlt_recog_age_groups = get_age_group(age, group_type = "ravlt_recog"),
    UDS_battery_age_groups = get_age_group(age, group_type = "UDS_battery"),
    cowat_age_groups = get_age_group(age, group_type = "cowat"),
    digsym_age_groups = get_age_group(age, group_type = "digsym")
  ) |>
    dplyr::arrange("age")

  result_tibble <- tibble::tibble(
    age = age_range_to_test,
    otmt_age_groups = dplyr::case_when(
      age < 20 ~ NA,
      age < 30 ~ "A",
      age < 40 ~ "B",
      age < 50 ~ "C",
      age < 60 ~ "D",
      age < 70 ~ "E",
      age >= 70 ~ "F",
      .default = NA
    ),
    ravlt_trials_age_groups = dplyr::case_when(
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
    ravlt_recog_age_groups = dplyr::case_when(
      age < 60 ~ NA,
      age < 63 ~ "A",
      age < 66 ~ "B",
      age < 69 ~ "C",
      age < 72 ~ "D",
      age < 75 ~ "E",
      age < 78 ~ "F",
      age < 81 ~ "G",
      age < 84 ~ "H",
      age < 87 ~ "I",
      age >= 87 ~ "J",
      .default = NA
    ),
    UDS_battery_age_groups = dplyr::case_when(
      age < 60 ~ 1,
      age < 70 ~ 2,
      age < 80 ~ 3,
      age < 90 ~ 4,
      age >= 90 ~ 5,
      .default = NA
    ),
    cowat_age_groups = dplyr::case_when(
      age < 56 ~ NA,
      age < 63 ~ "A",
      age < 66 ~ "B",
      age < 69 ~ "C",
      age < 72 ~ "D",
      age < 75 ~ "E",
      age < 78 ~ "F",
      age < 81 ~ "G",
      age < 84 ~ "H",
      age < 87 ~ "I",
      age < 90 ~ "J",
      age < 97 ~ "K",
      age >= 97 ~ NA,
      .default = NA
    ),
    digsym_age_groups = dplyr::case_when(
      age < 45 ~ NA,
      age < 55 ~ "A",
      age < 65 ~ "B",
      age < 70 ~ "C",
      age < 75 ~ "D",
      age < 80 ~ "E",
      age >= 80 ~ "F",
      .default = NA
    )
  ) |>
    dplyr::arrange("age")

  expect_equal(test_tibble, result_tibble)

})
