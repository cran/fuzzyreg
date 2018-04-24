#' Data For Fuzzy Linear Regression
#'
#' Example data reported by the authors of the respective fuzzy linear regression methods
#' for testing model fit performance.
#' 
#' @docType data
#' @usage data(fuzzydat)
#' @format A list of data.frames.
#' @keywords datasets
#' @source Diamond, P. (1988) Fuzzy least squares. \emph{Information Sciences}
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
#' @examples
#' data(fuzzydat)
#' fuzzylm(y ~ x, data = fuzzydat$lee)
#' fuzzylm(y ~ x, data = fuzzydat$dia, method = "dia", fuzzy.left.y = "yl", fuzzy.right.y = "yl")
"fuzzydat"