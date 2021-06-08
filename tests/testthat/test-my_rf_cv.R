test_that("my_rf_cv returns a numeric", {
  expect_type(my_rf_cv(5), "double")
})
