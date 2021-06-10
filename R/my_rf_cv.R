#' Random forest cross-validation
#'
#' This function uses a random forest algorithm with 100 trees on the
#'   \code{my_penguins} data from the \code{palmerpenguins} package to predict
#'   output \code{body_mass_g} with covariates \code{bill_length_mm},
#'   \code{bill_depth_mm}, and \code{flipper_length_mm}. Also computes the
#'   cross-validation mean square error.
#'
#' @param k An integer representing the number of folds used in
#'   cross-validation.
#' @keywords prediction
#'
#' @return Numeric representing the cross-validation error, or the average MSE
#'   across all folds.
#'
#' @examples
#' my_rf_cv(10)
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # Remove NAs from data, subset to relevant covariates
  penguins <- NA
  penguins <- tidyr::drop_na(my_penguins)
  penguins_rf <- penguins[, c(3:6)]
  # Initialize empty vector to store MSEs
  mse_vec <- rep(NA, length = k)
  # Randomly split data into k_cv folds
  folds <- sample(rep(1:k, length = nrow(penguins_rf)))
  penguins_split <- cbind(penguins_rf, "Split" = folds)
  for(i in 1:k){
    # Assign observations to training and test data
    train <- penguins_split %>% dplyr::filter(Split != i)
    test <- penguins_split %>% dplyr::filter(Split == i)
    # Train random forest model with 100 trees
    rf_model <- randomForest::randomForest(body_mass_g ~
                                             bill_length_mm +
                                             bill_depth_mm +
                                             flipper_length_mm,
                                           data = train,
                                           ntree = 100)
    # Predict the body mass of ith fold
    rf_predictions <- predict(rf_model, test[, -4])
    # Calculate MSE (mean of the squares of predicted minus actual body mass)
    mse_vec[i] <- mean((rf_predictions - test[, 4])^2)
  }
  # Calculate and return average MSE across k folds
  cv_mse <- mean(mse_vec)
  return(cv_mse)
}

utils::globalVariables(c("bill_length_mm", "bill_depth_mm", "body_mass_g",
                         "flipper_length_mm", "my_penguins", "Split"))
