context("correct numerical solution")

test_that("correct PLR-LS", {
  
  data(fuzzydat)
  expect_equivalent(fuzzylm(y ~ x, data = fuzzydat$lee, method = "plrls")$coef,  
               matrix(c(17.761911, 2.746428, .7619108, 1.2464280, 2.7380892, .4202387), ncol = 3),
               tolerance = 1e-6)
  
})

test_that("correct PLR", {
  
  data(fuzzydat)
  expect_equivalent(fuzzylm(y ~ x, fuzzydat$tan, "plr", , , "yl", "yr")$coef,  
                    matrix(c(3.85, 2.1, 3.85, 0, 3.85, 0), ncol = 3),
                    tolerance = 1e-3)
  
})

test_that("correct FLS", {

  data(fuzzydat)
  expect_equivalent(fuzzylm(y ~ x, fuzzydat$dia, "fls", , , "yl", "yl")$coef,
                    matrix(c(1.375, .1475, .1475, .1204, .025, .025), ncol = 3, byrow = TRUE),
                    tolerance = 1e-4)
})

test_that("correct MOFLR", {
  
  data(fuzzydat)
  expect_equivalent(fuzzylm(y ~ x, fuzzydat$nas, "moflr", "xl", , "yl")$coef,
                    matrix(c(6.07, 7.84, 7.84, 1.67, .48, .48), ncol = 3, byrow = TRUE),
                    tolerance = 1e-3)
})


test_that("correct OPLR", {
  
  data(fuzzydat)
  expect_equivalent(fuzzylm(y ~ x, fuzzydat$hun, "oplr", , , "yl", "yl")$coef,
                    matrix(c(4.15, 3.95, 3.95, 2, 0, 0), ncol = 3, byrow = TRUE),
                    tolerance = 1e-3)
})


test_that("correct TEF for crisp numbers", {

  f = fuzzylm(y ~ x, data = data.frame(x = 1:3, y = 2:4))
  expect_equal(0, 
               TEF(f),
               tolerance = 1e-6)
})

test_that("correct TEF for TFN using PLR by Tanaka et al.", {

  data(fuzzydat)
  f = fuzzylm(y ~ x, fuzzydat$tan, "plr", , , "yl", "yr")
  expect_equal(TEF(f),
               5.6,
               tolerance = 1e-2)
})