% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TEF.r
\name{TEF}
\alias{TEF}
\title{Total Error of Fit of Fuzzy Regression Model}
\usage{
TEF(object, sc = 1e-06, ...)
}
\arguments{
\item{object}{a \code{fuzzylm} object.}

\item{sc}{scaling constant used for numerical stability when spreads are equal to zero.}

\item{...}{additional arguments passed to the \code{integrate} function.}
}
\value{
A numeric with sum of pairwise differences between the triangular fuzzy
numbers.
}
\description{
Calculates total error of fit of a fuzzy regression model based on the
concept of difference in membership functions of triangular fuzzy numbers
between the estimated and observed fuzzy dependent variables.
}
\details{
Calculates \eqn{\sum{E}}{sum(E)}, where \eqn{E}{E} is the difference in
membership functions between two triangular fuzzy numbers. Here, between the
observation and the prediction from a fuzzy regression model \code{fuzzylm}.
}
\note{
\code{TEF} is not suitable for assessing fuzzy linear regression models that were
fitted from crisp input data. Such data will result in division by zero. The scaling
constant \code{sc} numerically allows the calculation to proceed, but it is not
advisable. Use \code{\link{GOF}} instead.
}
\examples{
data(fuzzydat)
f <- fuzzylm(y ~ x, fuzzydat$lee)
TEF(f)
}
\references{
Kim B. and Bishu R. R. (1998) Evaluation of fuzzy linear regression
models by comparing membership functions. \emph{Fuzzy Sets and Systems}
100: 343-352.
}
\seealso{
\code{\link{fuzzylm}}, \code{\link{GOF}}
}
