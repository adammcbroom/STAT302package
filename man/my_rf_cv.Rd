% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_rf_cv.R
\name{my_rf_cv}
\alias{my_rf_cv}
\title{Random forest cross-validation}
\usage{
my_rf_cv(k)
}
\arguments{
\item{k}{An integer representing the number of folds used in
cross-validation.}
}
\value{
Numeric representing the cross-validation error, or the average MSE
across all folds.
}
\description{
This function uses a random forest algorithm with 100 trees on the
\code{my_penguins} data from the \code{palmerpenguins} package to predict
output \code{body_mass_g} with covariates \code{bill_length_mm},
\code{bill_depth_mm}, and \code{flipper_length_mm}. Also computes the
cross-validation mean square error.
}
\examples{
my_rf_cv(10)
my_rf_cv(5)

}
\keyword{prediction}
