test_that("standardized_digsym", {
  expect_equal(standardized_digsym(57, 72), 16)
  expect_equal(standardized_digsym(32, 65), 9)

  expect_equal(standardized_digsym(33, 72), 9)
  expect_equal(standardized_digsym(34, 72), 11)

  expect_equal(standardized_digsym(0, 85), 2)
})
