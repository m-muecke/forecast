# A unit test for bats function
test_that("tests for a non-ts object", {
  set.seed(123)
  abc <- rnorm(50, 5, 1)
  fit <- bats(abc, use.box.cox = TRUE, use.parallel = FALSE)
  expect_false(fit$lambda == 0)
  expect_output(print(fit), "Seed States")
  expect_length(residuals(fit), 50L)
  plot(fit)
  expect_equal(bats(1, use.box.cox = TRUE, use.parallel = FALSE)$AIC, -Inf)
  expect_equal(bats(-1, use.box.cox = TRUE, use.parallel = FALSE)$AIC, -Inf)
})

test_that("updateGMatrix updates gamma.bold for all seasonal periods", {
  # two seasonal periods c(2, 3): tau = 5, gamma.bold layout (g1, 0, g2, 0, 0)
  g <- matrix(0, nrow = 6, ncol = 1)
  gamma.bold <- matrix(0, nrow = 1, ncol = 5)
  forecast:::updateGMatrix(
    g = g,
    gammaBold = gamma.bold,
    alpha = 0.1,
    beta = NULL,
    gammaVector = c(0.2, 0.3),
    seasonalPeriods = c(2L, 3L)
  )
  expect_identical(g[, 1], c(0.1, 0.2, 0, 0.3, 0, 0))
  expect_identical(gamma.bold[1, ], c(0.2, 0, 0.3, 0, 0))
})

test_that("Test parallel of bats", {
  abc <- rnorm(50, 5, 1)
  skip_on_cran()
  skip_on_ci()
  expect_gt(bats(abc, use.box.cox = TRUE, use.parallel = TRUE)$lambda, 0.999)
})
