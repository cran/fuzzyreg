context("data handling with fuzzylm")


test_that("warnings for spreads", {
	dat <- fuzzydat$nas
	
	expect_warning(fuzzylm(y~x,dat, method="fls", fuzzy.left.y = "yl", fuzzy.right.y = "yl"),
				   "spreads detected")

})


test_that("error for incorrect number of spreads for nsTFN", {
  
   expect_error(fuzzylm(y ~ x, data = fuzzydat$dia, method = "fls", fuzzy.left.y = "yl"),
                "two spreads")
})



test_that("warning on outlier in OPLR method", {

   expect_warning(fuzzylm(y ~ x, fuzzydat$hun, "oplr", , , "yl", "yl"),
                  "one outlier")
})


test_that("error on multiple outliers in OPLR method", {

  dat = fuzzydat$hun
  dat[1,3] = 4.0
  
  expect_error(fuzzylm(y ~ x, dat, "oplr", , , "yl", "yl"),
               "Multiple outliers")
})

