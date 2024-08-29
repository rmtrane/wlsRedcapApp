test_that("standardized_cog_ravlt_recog_acc", {
  expect_equal(standardized_ravlt_recog_acc(raw_score = 86, age = 85, sex = "m"), 10)
  expect_equal(standardized_ravlt_recog_acc(raw_score = 86, age = 85, sex = "f"), 9)

  expect_equal(standardized_ravlt_recog_acc(raw_score = 60, age = 84, sex = "m"), 4)
  expect_equal(standardized_ravlt_recog_acc(raw_score = 60, age = 84, sex = "f"), 2)

  # Check female
  expect_equal(
    standardized_ravlt_recog_acc(
      raw_score = c(62, 69, 75, 85, 89, 90, 93, 94, 98, 100),
      age = c(66, 68, 75, 76, 78, 84, 86, 89, 91, 93),
      sex = rep("f", 10) # c("f", "f", "f", "f", "m", "f", "m", "f", "m", "f")
    ),
    c(2, 3, 6, 8, 9, 10, 12, 12, 14, 16)
  )

  # Check male
  expect_equal(
    standardized_ravlt_recog_acc(
      raw_score = c(69, 70, 73, 79, 87, 89, 94, 97, 98, 100),
      age       = c(65, 68, 77, 81, 82, 88, 90, 91, 93, 97),
      sex       = rep("m", 10)
    ),
    c(3, 6, 7, 8, 10, 11, 14, 15, 15, 17)
  )
})
