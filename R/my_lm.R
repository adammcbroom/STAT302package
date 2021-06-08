#' Fitting linear models
#'
#' This function is used to fit linear models in order to carry out regressions.
#'
#' @param formula An object of class \code{"formula"} that describes the model
#'   to be fitted.
#' @param data An input data frame containing the variables to be used in the
#'   model.
#' @keywords inference
#'
#' @return A table with rows for each estimated coefficient and columns
#'   including the estimate, standard error, t-statistic, and p-value.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' @importFrom stats model.frame model.matrix model.response predict pt sd
#'   na.omit
#'
#' @export

my_lm <- function(formula, data) {
  # Extract and store model matrix X
  mod_mat_X <- stats::model.matrix(formula, data)
  # Extract and store model response Y
  mod_frame_1 <- stats::model.frame(formula, data)
  mod_resp_Y <- stats::model.response(mod_frame_1)
  # Calculate and store degrees of freedom
  df_2 <- nrow(data) - ncol(mod_mat_X)
  # Calculate the coefficients
  estimate <- (solve(t(mod_mat_X) %*% mod_mat_X) %*%
                 (t(mod_mat_X) %*% mod_resp_Y))
  # Calculate and store the estimated variance of the error term
  variance <- sum((((mod_resp_Y - (mod_mat_X %*% estimate)) ^2) / df_2))
  # Calculate and store the standard errors
  std_errors  <- (sqrt(variance) *
                    sqrt(diag(solve((t(mod_mat_X) %*% mod_mat_X)))))
  # Calculate and store the t values
  t_val <- (estimate / std_errors)
  # Calculate and store the p values
  prob <- 2 * stats::pt(abs(t_val), df_2, lower.tail = FALSE)
  # Return table with labeled columns and rows
  output_mat <- matrix(NA, nrow = nrow(estimate), ncol = 4)
  output_mat[ , 1] <- estimate
  output_mat[ , 2] <- std_errors
  output_mat[ , 3] <- t_val
  output_mat[ , 4] <- prob
  output <- as.data.frame(output_mat)
  rownames(output) <- c(colnames(mod_mat_X))
  colnames(output) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(output)
}
