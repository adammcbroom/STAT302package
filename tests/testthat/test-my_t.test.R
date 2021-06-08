test_that("incorrect 'alternative' input throws error", {
  expect_error(my_t.test(1:10000, alternative = "both.sided", mu = 4900))
})

test_that("my_t.test returns a list", {
  expect_type(my_t.test(1:10000, alternative = "two.sided", mu = 4900), "list")
  expect_type(my_t.test(1:10000, alternative = "less", mu = 4900), "list")
  expect_type(my_t.test(1:10000, alternative = "greater", mu = 5100), "list")
})

