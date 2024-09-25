test_that("functional_measures_table works", {
  expect_equal(
    functional_measures_table(dplyr::filter(demo_data, .data$cog_studyid == "326231g"))$`_data`,
    tibble::tribble(
      ~name, ~`Visit 2`, ~`Visit 1`,
      "FAS:", 0, 1,
      "Informant:", 3.44, 3.88,
      "Self:", 3.38, 3.38,
      "Global:", 0.5, 0.5,
      "SOB:", 1.5, 1.5
    )
  )
})
