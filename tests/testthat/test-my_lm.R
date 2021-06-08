test_that("my_lm returns a list", {
  expect_type(my_lm(lifeExp ~ gdpPercap, my_gapminder), "list")
})
