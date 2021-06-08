test_that("my_lm returns a list", {
  expect_type(my_lm(lifeExp ~ gdpPercap, my_gapminder), "list")
})

test_that("missing arguments return an error", {
  expect_error(my_lm(lifeExp ~ gdpPercap))
  expect_error(my_lm(gapminder))
})
