% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.fuzzylm.R
\name{plot.fuzzylm}
\alias{plot.fuzzylm}
\title{Plot Fuzzy Linear Regression}
\usage{
\method{plot}{fuzzylm}(
  x,
  y = NULL,
  choice = 1,
  res = 2,
  col.fuzzy = NA,
  length = 0.05,
  angle = 90,
  main = "method",
  xlab = NULL,
  ylab = NULL,
  ...
)
}
\arguments{
\item{x}{a \code{fuzzylm} object.}

\item{y}{NULL for plotting a \code{fuzzylm} object.}

\item{choice}{an integer or character string specifying which explanatory variable to
plot in a partial fit of a multiple regression.}

\item{res}{an integer \code{>= 2} specifying resolution of shading for the regression
plot. Minimum resolution for shading the plot is 3.}

\item{col.fuzzy}{color for shading of the regression plot.}

\item{length}{length of the edges of the arrow head (in inches).}

\item{angle}{angle from the shaft of the arrow to the edge of the arrow head.}

\item{main}{a main title for the plot. Default title specifies method used to fit
the model.}

\item{xlab}{a label for the x axis, defaults to a description of x.}

\item{ylab}{a label for the y axis, defaults to a description of y.}

\item{...}{additional graphical parameters.}
}
\value{
No return value, called for side effects.
}
\description{
Plots the data and the central tendency with spreads of a fuzzy linear regression.
For multiple regression, allows choice of which variable to display. Optionally colors
the polygon for the regression.
}
\details{
Silently plots the data. Fuzzy numbers are plotted with points for the central
value and arrows specifying spreads.

In case the \code{x} object contains a multiple fuzzy regression, the function plots
a partial fit for one explanatory variable.
}
\examples{
data(fuzzydat)
f = fuzzylm(y ~ x, fuzzydat$lee)
plot(f)
plot(f, res = 20, col.fuzzy = "red")
}
\keyword{fuzzy}
