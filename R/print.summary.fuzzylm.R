#' Prints Fuzzy Linear Regression Summary
#'
#' Prints the models for the central tendency and spreads from the \code{fuzzylm} object. 
#' @param x a summary of a \code{fuzzylm} object.
#' @param ... further arguments passed to or from other methods.
#' @keywords fuzzy
#' @export
#' @examples
#' x <- rep(1:3, each = 5)
#' y <- c(rnorm(5, 1), rnorm(5, 2), rnorm(5, 3))
#' dat <- data.frame(x = x, y = y)
#' f <- fuzzylm(y ~ x, dat)
#' sum.f <- summary(f)
#' sum.f


print.summary.fuzzylm = function(x, ...){
	n = length(x$c)
	if(x$method %in% c("nasrabadi", "tanaka")) n = (length(x$c) + 1) / 2
	cat("\nModel for the central tendency:\n")
	cat(x$yvars[1], "=", x$c[1], "+", 
		paste(x$c[2:n], x$xvars[1:(n-1)], sep=" * ", collapse = " + "), sep = " ")
	cat("\n\nModel for the left spread:\n")
	cat(x$yvars[1], "=", x$c[1] - x$l[1], "+", 
		paste(x$c[2:n] - x$l[2:n], x$xvars[1:(n-1)], sep=" * ", collapse = " + "), sep = " ")
	cat("\n\nModel for the right spread:\n")
	cat(x$yvars[1], "=", x$c[1] + x$l[1], "+", 
		paste(x$c[2:n] + x$l[2:n], x$xvars[1:(n-1)], sep=" * ", collapse = " + "), sep = " ")
}