test_that("incorrect 'alternative' input throws error", {
  expect_error(my_t.test(x, alternative = "both.sided"))
})

test_that("my_t.test returns a list", {
  expect_type(my_t.test(1:10000, alternative = "two.sided", mu = 4900), "list")
})
