test_that("tsCV works with h = 1", {
  e <- tsCV(lynx, rwf, h = 1)
  expect_s3_class(e, "ts")
  expect_null(dim(e))
  expect_length(e, length(lynx))
})

test_that("tsCV works with h > 1", {
  e <- tsCV(lynx, rwf, h = 3)
  expect_s3_class(e, "ts")
  expect_shape(e, dim = c(length(lynx), 3))
  expect_equal(colnames(e), paste0("h=", 1:3))
})

test_that("tsCV works with initial", {
  e1 <- tsCV(lynx, rwf, h = 1)
  e2 <- tsCV(lynx, rwf, h = 1, initial = 10)
  expect_true(sum(!is.na(e2)) < sum(!is.na(e1)))
})

test_that("tsCV errors on bad initial", {
  expect_error(
    tsCV(lynx, rwf, h = 1, initial = length(lynx)),
    "initial period too long"
  )
})

test_that("tsCV works with xreg", {
  y <- ts(rnorm(50))
  xreg <- matrix(rnorm(100), ncol = 2)
  fn <- function(x, h, xreg, newxreg) {
    forecast(Arima(x, order = c(1, 0, 0), xreg = xreg), xreg = newxreg)
  }
  e <- tsCV(y, fn, h = 1, xreg = xreg)
  expect_s3_class(e, "ts")
  expect_length(e, length(y))
})

test_that("CVar works", {
  skip_on_cran()
  cv <- CVar(lynx, k = 2, lambda = 0.15)
  expect_s3_class(cv, "CVar")
  expect_equal(cv$k, 2)
  expect_shape(cv$CVsummary, dim = c(7, 2))
  expect_true(cv$LBpvalue >= 0 && cv$LBpvalue <= 1)
  # residuals should equal testfit - y
  expect_equal(
    as.numeric(cv$residuals),
    as.numeric(cv$testfit) - as.numeric(lynx)
  )
  expect_output(print(cv), "cross-validation")
})
