% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_assumptions.R
\name{check_assumptions}
\alias{check_assumptions}
\title{Checking the Gauss-Markov Assumptions}
\usage{
check_assumptions(model_name)
}
\arguments{
\item{model_name}{An OLS model object.}
}
\value{
Each row represents one assumption check. The three columns
show the test, the relevant value (depends on the exact test), and
an evaluation of whether further exploration is necessary. Normality,
constant variance, and autocorrelation are all significance tests.
Multicollinearity uses VIF scores (relevant only for multiple regression).
Conditional mean looks at the correlation between the residuals and the numeric
variable.  Currently, it is unable to properly evaluate the influence of a
categorical variable.
}
\description{
This function taks in an OLS model and returns a data frame of the
most common assumptions used to test a model. Used as a starting
point for testing the assumptions.  The results offer suggestions
about which assumptions deserve a closer examination.
}
\examples{
model <- lm(dist ~ speed , data = cars) # running a model
check_assumptions(model)

model2 <- lm(mpg ~ cyl + wt, data = mtcars)
check_assumptions(model2)
}
