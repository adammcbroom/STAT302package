#' K-nearest neighbor cross-validation
#'
#' This function uses k-nearest neighbor analysis to generate predicted
#'   classifications for an input dataset based on input covariates. Also uses
#'   cross-validation to compute average misclassification rate in order to
#'   assist in the selection of an optimal value of k.
#'
#' @param train A data frame of training set cases. Include only covariates of
#'   interest to be used in predicting class.
#' @param cl A vector including the true class values of the training data
#' @param k_nn An integer representing the number of neighbors considered.
#' @param k_cv An integer representing the number of folds used in
#'   cross-validation.
#' @keywords prediction
#'
#' @return A list with the following elements:
#'   \code{"class"}: a vector of the predicted class for all observations,
#'   and \code{"cv_err"}: a numeric with the cross-validation misclassification
#'     error.
#'
#' @examples
#' penguins <- tidyr::drop_na(my_penguins)
#' my_knn_cv(penguins[, 3:6], penguins$species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Initialize output vectors
  class_preds <- vector()
  class <- vector()
  test_class <- vector()
  errors <- rep(NA, k_cv)
  # Randomly split data into k_cv folds
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  train_split <- cbind(train, "Split" = folds)
  cl_split <- cbind(data.frame(cl), "Split" = folds)
  for (i in 1:k_cv) {
    # Assign observations to training and test data
    x_train <- train_split %>% dplyr::filter(Split != i)
    x_test <- train_split %>% dplyr::filter(Split == i)
    y_train <- cl_split %>% dplyr::filter(Split != i)
    y_test <- cl_split %>% dplyr::filter(Split == i)
    # Predict the class of the ith fold
    class_preds <- c(class_preds, as.character(class::knn(train = x_train,
                                                          test = x_test,
                                                          cl = y_train[, 1],
                                                          k = k_nn)))
    # Store true class of ith fold in vector for comparison
    test_class <- c(test_class, as.character(y_test[, 1]))
    # Calculate misclassification rate
    errors[i] <- mean(class_preds != test_class)

  }
  # Calculate misclassification rate, return output
  class <- class::knn(train = train,
                      test = train,
                      cl = cl,
                      k = k_nn)
  cv_error <-  (sum(errors) / k_cv)
  output_list <- list("Predicted class" = class,
                      "Error rate" = cv_error)
  return(output_list)
}

utils::globalVariables("Split")
