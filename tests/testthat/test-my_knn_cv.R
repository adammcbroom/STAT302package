penguins <- tidyr::drop_na(my_penguins)

test_that("my_knn_cv returns a list", {
  expect_type(my_knn_cv(penguins[, 3:6], penguins$species, 1, 5), "list")
})
