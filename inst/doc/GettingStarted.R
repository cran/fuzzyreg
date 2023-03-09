## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#",
  fig.path = "figures/",
  fig.height = 4.5, 
  fig.width = 6 
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("fuzzyreg", dependencies = TRUE)
#  require(fuzzyreg)
#  help(package = "fuzzyreg")

## ---- echo=FALSE--------------------------------------------------------------
if(!require(fuzzyreg)) install.packages("fuzzyreg", dependencies = TRUE)

## -----------------------------------------------------------------------------
data(fuzzydat)
f <- fuzzylm(formula = y ~ x, data = fuzzydat$lee)

## -----------------------------------------------------------------------------
print(f)

## ----plrls, fig.width = 6, fig.height = 4.5, fig.align = 'center', fig.cap = 'Fuzzy linear regression using the PLRLS method'----
plot(f, res = 20, col = "lightblue", main = "PLRLS")

## -----------------------------------------------------------------------------
summary(f)

## ----TNF, echo=FALSE, eval=TRUE, fig.width = 6.5, fig.height = 3.5, fig.align = 'center', fig.cap = 'Triangular fuzzy numbers.'----
plot(1, type = "n", xlim = c(0,3), ylim=c(0,1.1), xlab = expression(italic("x")),
ylab = "degree of membership", las = 1)
points(matrix(c(.5,1.5,2.5,1,1,1), ncol=2), pch=19)
text(c(.5,1.5,2.5),1, labels=as.expression(lapply(LETTERS[1:3], function(x) bquote(italic(.(x))))),pos=2)
segments(.5,0,.5,1,lty=2)
segments(1.5,0,1.5,1,lty=2)
segments(2.5,0,2.5,1, lty=2)
segments(.7,0,1.5,1)
segments(1.9,0,1.5,1)
segments(2,0,2.5,1)
segments(3,0,2.5,1)

## ---- eval=FALSE--------------------------------------------------------------
#  A = c(0.5, 0, 0)

## ---- eval = FALSE------------------------------------------------------------
#  B = c(1.5, 0.8, 0.4)
#  C = c(2.5, 0.5, 0.5)

## -----------------------------------------------------------------------------
A <- rnorm(3)
fuzzify(x = A, method = "zero")

## -----------------------------------------------------------------------------
fuzzify(x = A, method = "err", err = abs(runif(2 * 3) * 1e-6))

## -----------------------------------------------------------------------------
fuzzify(x = A, method = "err", err = 0.2)

## -----------------------------------------------------------------------------
fuzzify(x = A, method = "mean")
fuzzify(x = A, method = "median")

## -----------------------------------------------------------------------------
fuzzify(x = A, y = c(1, 2, 2), method = "median")
fuzzify(x = A, y = c(1, 1, 2), method = "median")

## -----------------------------------------------------------------------------
require(FuzzyNumbers)
B1 = FuzzyNumber(0.7, 1.5, 1.5, 1.9,
		left = function(x) x,
		right = function(x) 1 - x,
		lower = function(a) a,
		upper = function(a) 1 - a)
B1

## -----------------------------------------------------------------------------
xc = core(B1)[1]
l = xc - supp(B1)[1]
r = supp(B1)[2] - xc
c(xc, l, r)

## ---- echo = FALSE------------------------------------------------------------
detach("package:FuzzyNumbers", unload = TRUE)
require(fuzzyreg)

## -----------------------------------------------------------------------------
fuzzydat$nas

## -----------------------------------------------------------------------------
f2 <- fuzzylm(formula = y ~ x, data = fuzzydat$nas,
             fuzzy.left.x = "xl",
             fuzzy.left.y = "yl", method = "moflr")

## -----------------------------------------------------------------------------
f3 <- fuzzylm(y ~ x, data = fuzzydat$nas,
             fuzzy.left.y = "yl",
             fuzzy.right.y = "yl", method = "fls")

## -----------------------------------------------------------------------------
pred2 = predict(f2)
pred2$y

## ----figTEF, echo=FALSE, eval=TRUE, fig.cap = 'Comparison of MOFLR and FLS model fits to the same data.', fig.width = 8.5, fig.height = 3.5, fig.align = 'center'----
oldpar <- par()
par(mfrow=c(1,2))
plot(f2, res=20, col.fuzzy="lightblue", main = "f2 - MOFLR")
plot(f3, res=20, col.fuzzy = "lightblue", main = "f3 - FLS")

## -----------------------------------------------------------------------------
TEF(f2)
TEF(f3)

## ----regfig, eval=TRUE, echo=FALSE, fig.cap = 'Fuzzy and statistical linear regression. Fuzzy regression displays the central value and the support boundaries determined by the left and right spread (PLRLS - left panel). Statistical least-squares regressionshows the  confidence interval for the regression fit (LS - right panel).', fig.width = 8.5, fig.height = 3.5, fig.align = 'center'----
par(mfrow=c(1,2))
f4 = fuzzylm(y~x, fuzzydat$lee, method="plrls")
plot(f4, res=20, col.fuzzy="lightblue", main="PLRLS")
f1 = lm(y~x, fuzzydat$lee)
newx = seq(min(fuzzydat$lee$x), max(fuzzydat$lee$y), length.out = 500)
conf1 = predict(f1, newdata=data.frame(x=newx),
                interval="confidence")
plot(fuzzydat$lee$x, fuzzydat$lee$y, xlab = "x", ylab = "y", main="LS")
abline(f1)
lines(newx, conf1[,2], lty=2)
lines(newx, conf1[,3], lty=2)
par(oldpar)

## ---- eval=FALSE--------------------------------------------------------------
#  ?plrls

