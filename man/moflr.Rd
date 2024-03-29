% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moflr.R
\name{moflr}
\alias{moflr}
\title{Fuzzy Linear Regression Using the Multi-Objective Fuzzy Linear Regression Method}
\usage{
moflr(x, y, omega = 0.5, sc = 1e-06)
}
\arguments{
\item{x}{matrix of \emph{n} independent variable values, followed by \emph{n} spreads.
First column is exptected to consist of ones, representing intercept. Missing values
not allowed.}

\item{y}{two column matrix of dependent variable values and the respective spread.
Method assumes symmetric triangular fuzzy input, so the second spread (if present) is
ignored. Missing values not allowed.}

\item{omega}{a scalar that specifies weight that determines trade-off of
between outliers penalization and data fitting in interval \code{[0,1]}, where high
values of \code{omega} decrease the penalization of outliers.}

\item{sc}{scaling constant used to input random spreads for the intercept, necessary for
computational stability.}
}
\value{
Returns a \code{fuzzylm} object that includes the model coefficients, limits
for data predictions from the model and the input data.
}
\description{
This function calculates fuzzy regression coeficients using the multi-objective fuzzy
linear regression (MOFLR) method developed by Nasrabadi et al. (2005)
that combines the least squares approach (fitting of a central tendency) with the
possibilistic approach (fitting of spreads) when approximating an observed linear
dependence by a fuzzy linear model.
}
\details{
The function input expects both the response and the predictors in form of
symmetric fuzzy numbers. The
prediction returns symmetric triangular fuzzy number coefficients.
The Nasrabadi et al.'s method can process datasets with multiple outliers. Values
\code{omega>0.5} decrease weight of outliers on the solution.
}
\note{
Preferred use is through the \code{\link{fuzzylm}} wrapper function with argument
\code{method = "moflr"}.
}
\examples{
data(fuzzydat)
fuzzylm(y~x, fuzzydat$nas, "moflr", "xl", , "yl")
}
\references{
Nasrabadi, M. M., Nasrabadi, E. and Nasrabady, A. R. (2005) Fuzzy linear
regression analysis: a multi-objective programming approach. \emph{Applied Mathematics
and Computation} 163: 245-251.
}
\seealso{
\code{\link{fuzzylm}}
}
\keyword{fuzzy}
