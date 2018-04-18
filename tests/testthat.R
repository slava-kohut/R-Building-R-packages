library(testthat)
library(farsr)

test_that("correct filename", {
  expect_that(make_filename(2012), is_identical_to("accident_2012.csv.bz2"))
  expect_that(make_filename(2013), is_identical_to("accident_2013.csv.bz2"))
  expect_that(make_filename(2014), is_identical_to("accident_2014.csv.bz2"))
})
