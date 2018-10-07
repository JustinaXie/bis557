library(testthat)

context("Test the output of homework 2.")

test_that("Ridge regression works.", {
  data(ridge_train)
  data(ridge_test)
  fit_ridge <- ridge_reg(y ~., ridge_train)
  fit_lm.ridge <- MASS::lm.ridge(y ~., ridge_train, lambda = 1.2121212)
  fit_linear_model <- linear_model(y ~., lm_patho)
  expect_equivalent(fit_ridge$coef, fit_lm.ridge$coef, tolerance = 1e-5)
})
