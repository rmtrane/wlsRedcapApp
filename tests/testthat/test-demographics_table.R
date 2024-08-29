test_that("Demographics table works", {

  expect_equal(
    demographics_table(demo_data, studyid = "326231g")$`_data`,
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



  expect_equal(
    demographics_table(demo_data, studyid = "100108g")$`_data`,
    tibble::tribble(
      ~name, ~value,
      "Study ID:", "100108g",
      "Education (years):", "12",
      "Age at Visit:", "83",
      "Gender:", "Female",
      "Handedness:", "RH",
      "Race:", "Caucasian"
    )
  )

})
