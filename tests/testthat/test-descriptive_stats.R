test_that("Mean works correctly", {
  expect_equal(calc_mean(c(1,2,3)), 2)
  expect_equal(calc_mean(c(1:10)), 5.5)
  # expect_true(is.na(calc_mean(numeric(0))))
})

test_that("Median works correctly", {
  expect_equal(calc_median(c(1,2,3)), 2)
  expect_equal(calc_median(c(1,2,3,4)), 2.5)
  expect_equal(calc_median(c(1:9)), 5)
  expect_equal(calc_median(c(1:10)), 5.5)
})

test_that("Mode works correctly", {
  expect_equal(calc_mode(c(1,2,2,3)), 2)
  expect_equal(sort(calc_mode(c(1,1,2,2))), c(1,2))
  expect_equal(calc_mode(c(1,2,3)), c(1,2,3))
})

test_that("Quartiles and IQR work", {
  x1 <- c(1,2,3,4,5)
  x2 <- c(1:10)
  expect_equal(calc_q1(x1), 2)
  expect_equal(calc_q3(x1), 4)
  expect_equal(calc_iqr(x1), 2)
  expect_equal(calc_q1(x2), 3.25)
  expect_equal(calc_q3(x2), 7.75)
  expect_equal(calc_iqr(x2), 4.5)
})

test_that("Handles NA values", {
  expect_equal(calc_mean(c(1,2,3,4,5,NA)), 3)
  expect_equal(calc_median(c(1,2,3,4,5,NA)), 3)
  expect_equal(calc_mode(c(1,2,2,3,NA)), 2)
  expect_equal(calc_q1(c(1,2,3,4,5,NA)), 2)
  expect_equal(calc_q3(c(1,2,3,4,5,NA)), 4)
  expect_equal(calc_iqr(c(1,2,3,4,5,NA)), 2)
})

test_that("Errors are thrown for invalid inputs", {
  expect_error(calc_mean(), class = "descriptiveStats_missing_input")
  expect_error(calc_mean("a"), class = "descriptiveStats_invalid_input")
  expect_error(calc_mean(numeric(0)), class = "descriptiveStats_empty_input")
  expect_error(calc_mean(c(NA_real_, NA_real_, NA_real_)), class = "descriptiveStats_all_na")
})
