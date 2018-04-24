#' Fuzzy Linear Regression
#'
#' A wrapper function that calculates fuzzy regression coeficients using a chosen method.
#' @param formula a model formula.
#' @param data a data.frame, containing the variables in formula.
#' @param method method for fitting of the fuzzy linear model. 
#'     Accepts any non-redundant abbreviation.
#' @param fuzzy.left.x character string vector specifying column name(s) with the left 
#'   spread of the fuzzy independent variable(s).
#' @param fuzzy.right.x character string vector specifying column name(s) with the right 
#'   spread of the fuzzy independent variable(s).
#' @param fuzzy.left.y character string vector specifying column name(s) with the left 
#'   spread of the fuzzy dependent variable(s).
#' @param fuzzy.right.y character string vector specifying column name(s) with the right 
#'   spread of the fuzzy dependent variable(s).
#' @param ... additional parameters used by specific methods.
#' @details The implemented methods include \code{\link{lee}} for fitting the fuzzy linear
#'   regression from the crisp input data (Lee and Tanaka 1999), and \code{\link{diamond}} 
#'   (Diamond 1988), \code{\link{hung}} (Hung and Yang 2006), \code{\link{nasrabadi}}
#'   (Nasrabadi et al. 2005) and \code{\link{tanaka}} (Tanaka et al. 1989) methods for
#'   triangular fuzzy numbers. 
#' @return Returns a \code{fuzzylm} object that includes the model coefficients, limits
#'   for data predictions from the model and the input data.
#' @seealso \code{\link{plot.fuzzylm}}, \code{\link{predict.fuzzylm}}, 
#'   \code{\link{summary.fuzzylm}}
#' @references
#'   Diamond, P. (1988) Fuzzy least squares. \emph{Information Sciences}
#'   46(3): 141-157.
#'
#'   Hung, W.-L. and Yang, M.-S. (2006) An omission approach for detecting 
#'   outliers in fuzzy regression models. \emph{Fuzzy Sets and Systems} 157: 3109-3122.
#'
#'   Lee, H. and Tanaka, H. (1999) Fuzzy approximations with non-symmetric fuzzy
#'   parameters in fuzzy regression analysis. \emph{Journal of the Operations Research
#'   Society Japan} 42: 98-112.
#'
#'   Nasrabadi, M. M., Nasrabadi, E. and Nasrabady, A. R. (2005) Fuzzy linear 
#'   regression analysis: a multi-objective programming approach. \emph{Applied Mathematics
#'   and Computation} 163: 245-251.
#'
#'   Tanaka H., Hayashi I. and Watada J. (1989) Possibilistic linear 
#'   regression analysis for fuzzy data. \emph{European Journal of Operational 
#'   Research} 40: 389-396.
#' @export
#' @examples
#' data(fuzzydat)
#' fuzzylm(y ~ x, data = fuzzydat$lee, method = "lee")
#' \dontrun{
#' # returns error due to incorrect number of spreads
#' fuzzylm(y ~ x, data = fuzzydat$dia, method = "diamond", fuzzy.left.y = "yl")}
#' # use the same column name for left and right spread, when the method requests 
#' # non-symmetric fuzzy numbers
#' fuzzylm(y ~ x, data = fuzzydat$dia, method = "diamond", fuzzy.left.y = "yl", fuzzy.right.y = "yl")


fuzzylm = function(formula, data, method = "lee", fuzzy.left.x = NULL, fuzzy.right.x = NULL, fuzzy.left.y = NULL, fuzzy.right.y = NULL, ...){
	# initiate model.frame
	cl <- match.call()
	mf <- match.call(expand.dots = FALSE)
    xy = all.vars(mf)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
	x <- attr(mt, "term.labels")
	y <- xy[attr(mt, "response")]
	# check spreads
	if(any(!is.null(c(fuzzy.left.x, fuzzy.right.x, fuzzy.left.y, fuzzy.right.y)))){
		warning("fuzzy spreads detected - assuming same variable order as in formula")
		lhs <- c(y, fuzzy.left.y, fuzzy.right.y)
		rhs <- c(x, fuzzy.left.x, fuzzy.right.x)
		formula <- ifelse(length(lhs) > 1, 
						  sprintf("cbind(%s) ~ %s", toString(lhs), paste(rhs, collapse = " + ")),
						  sprintf("%s ~ %s", toString(lhs), paste(rhs, collapse = " + ")))
		mf <- stats::model.frame(formula, data = data)
  	    if(attr(stats::terms(mf), "intercept") == 0) stop("fuzzy regression through origin is not supported. Use intercept")
	}
	y <- stats::model.response(mf, "numeric")
	x <- stats::model.matrix(stats::as.formula(formula), data = mf)
	# check method
	methods <- c("lee", "diamond", "hung", "nasrabadi", "tanaka")
	if(!any(grepl(method, methods)))
		stop(gettextf("method '%s' is not supported.", method))
	if(sum(grepl(paste0("^", method), methods)) > 1)
		stop(gettextf("method '%s' matches more than one available method.", method))
	method <- methods[grepl(paste0("^", method), methods)]
	coefs <- switch(method, lee = lee(x = x, y = y, ...),
							diamond = diamond(x = x, y = y, ...),
							hung = hung(x = x, y = y, ...),
							nasrabadi = nasrabadi(x = x, y = y, ...),
							tanaka = tanaka(x = x, y = y, ...))
	fuzzy <- list(call = cl, method = method, fuzzynum = coefs$fuzzynum, coef = coefs$coef, lims = coefs$lims, x = x, y = y)
	class(fuzzy) <- "fuzzylm"
	fuzzy
}

