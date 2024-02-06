test_that("00: Error when vectors dont have same length", {
  expect_error(.check_params(c(1,2), c('y'), 3L, 3L, "", NULL, 10))
})

test_that("01: Error when vectors dont have same length", {
  expect_error(.check_params(c(1), c('y', 'k'), 3L, 3L, "", NULL, 10))
})

test_that("02: Error when vectors dont have same length", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3L, "", c('#FFFFFF'), 10))
})

test_that("03: Error when cp_max not is integer", {
  expect_error(.check_params(c(1,2), c('y','k'), 3, 3L, "", NULL, 10))
})

test_that("04: Error when cp_line_width is not integer", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3, "", NULL, 10))
})

test_that("05: Error when cp_title_size is not numeric / integer", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3L, "", NULL, 's'))
})

test_that("06: Error when cp_title_size is < 1", {
  expect_error(.check_params(c(1,2), c('y','k'), 3L, 3L, "", NULL, 0))
})

test_that("07: Error when cp_max is <= 0", {
  expect_error(.check_params(c(1,2), c('y','k'), 0L, 3L, "", NULL, 1))
})

test_that("08: Error when cp_vals is null", {
  expect_error(.check_params(NULL, c('y','k'), 1L, 3L, "", NULL, 1))
})

test_that("09: Error when cp_text is null", {
  expect_error(.check_params(c(1,2), NULL, 1L, 3L, "", NULL, 1))
})

test_that("10: Error when cp_vals is not numeric", {
  expect_error(.check_params(c('s','s'), c('y','k'), 1L, 3L, "", NULL, 1))
})

test_that("11: Error when cp_text is not character", {
  expect_error(.check_params(c(1,2), c(1,2), 1L, 3L, "", NULL, 1))
})
