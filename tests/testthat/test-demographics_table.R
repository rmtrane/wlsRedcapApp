test_that("Demographics table works", {
  demo_table <- demographics_table(demo_data, studyid = "326231g")

  expect_equal(
    demo_table$`_data`,
    tibble::tribble(
      ~name, ~value,
      "Study ID:", "326231g",
      "Education (years):", "12",
      "Age at Visits:", "79/82",
      "Gender:", "Male",
      "Handedness:", "RH",
      "Race:", "Caucasian"
    )
  )

})
