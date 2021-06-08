#' T-test
#'
#' This function performs a one-sample t-test on a numeric vector of input data.
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis.
#'   Only accepts input values of \code{"two.sided"}, \code{"less"},
#'   or\code{"greater"}.
#' @param mu Numeric input indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return A list with the following elements:
#'   \code{"test_stat"}: the numeric test statistic,
#'   \code{"df"}: the degrees of freedom,
#'   \code{"alternative"}: the value of the parameter \code{"alternative"},
#'   and \code{"p_val"}: the numeric p-value.
#'
#' @examples
#' my_t.test(1:10000, alternative = "two.sided", mu = 4900)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # Calculate and store sample mean
  sample_mean <- mean(x)
  # Calculate and store test statistic
  test_stat <- (sample_mean - mu) / (sd(x) / sqrt(length(x)))
  # Calculate and store degrees of freedom
  df <- length(x) - 1
  # Calculate p value for test with alternative hypothesis as "less"
  if (alternative == "less") {
    p_val <- stats::pt(test_stat, df, lower.tail = TRUE)
    # Store and print output
    output <- list("test_stat" = test_stat,
                   "df" = df,
                   "alternative" = alternative,
                   "p_val" = p_val)
    return(output)
    # Calculate p value for test with alternative hypothesis as "greater"
  } else if (alternative == "greater") {
    p_val <- stats::pt(test_stat, df, lower.tail = FALSE)
    # Store and print output
    output <- list("test_stat" = test_stat,
                   "df" = df,
                   "alternative" = alternative,
                   "p_val" = p_val)
    return(output)
    # Calculate p value for test with alternative hypothesis as "two sided"
  } else if (alternative == "two.sided") {
    p_val <- 2 * stats::pt(abs(test_stat), df, lower.tail = FALSE)
    # Store and print output
    output <- list("test_stat" = test_stat,
                   "df" = df,
                   "alternative" = alternative,
                   "p_val" = p_val)
    return(output)
    # Return error message if "alternative" argument is incorrect
  } else {
    stop("'Alternative' must be one of 'two.sided', 'less,' or 'greater'.")
  }
}
