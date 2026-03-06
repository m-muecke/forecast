test_that("ndiffs works with kpss", {
  expect_equal(ndiffs(WWWusage), 1)
})

test_that("ndiffs works with adf", {
  expect_equal(ndiffs(WWWusage, test = "adf"), 1)
})

test_that("ndiffs works with pp", {
  expect_equal(ndiffs(WWWusage, test = "pp"), 1)
})

test_that("ndiffs returns 0 for constant series", {
  expect_equal(ndiffs(ts(rep(5, 100))), 0)
})

test_that("ndiffs warns on out-of-range alpha", {
  expect_warning(ndiffs(WWWusage, alpha = 0.001), "less than the minimum")
  expect_warning(ndiffs(WWWusage, alpha = 0.5), "larger than the maximum")
})

test_that("ndiffs respects max.d", {
  result <- ndiffs(WWWusage, max.d = 1)
  expect_true(result <= 1)
})

test_that("nsdiffs works with default test", {
  expect_equal(nsdiffs(AirPassengers), 1)
})

test_that("nsdiffs works with ocsb test", {
  expect_equal(nsdiffs(AirPassengers, test = "ocsb"), 1)
})

test_that("ndiffs on seasonally differenced AirPassengers", {
  expect_equal(ndiffs(diff(log(AirPassengers), 12)), 1)
})

test_that("nsdiffs returns 0 for non-seasonal data", {
  expect_error(nsdiffs(WWWusage), "Non seasonal data")
})

test_that("nsdiffs returns 0 for constant series", {
  expect_equal(nsdiffs(ts(rep(5, 100), frequency = 12)), 0)
})

test_that("nsdiffs returns 0 when frequency >= length", {
  expect_equal(nsdiffs(ts(1:10, frequency = 12)), 0)
})

test_that("ocsb.test works", {
  result <- ocsb.test(AirPassengers)
  expect_s3_class(result, "OCSBtest")
  expect_true(!is.null(result$statistics))
  expect_true(!is.null(result$critical))
  expect_output(print(result), "OCSB test")
})

test_that("ocsb.test errors on non-seasonal data", {
  expect_error(ocsb.test(WWWusage), "Data must be seasonal")
})
