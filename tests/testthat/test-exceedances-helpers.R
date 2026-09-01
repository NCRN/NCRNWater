# tests/testthat/test-exceedances-helpers.R
#
# Example usage
#   testthat::test_file("tests/testthat/test-exceedances-helpers.R")
#   devtools::test(filter = "exceedances")

library(testthat)
library(NCRNWater)

test_that("vec_format formats lists of years nicely", {
  expect_equal(NCRNWater:::vec_format(integer(0)), "")
  expect_equal(NCRNWater:::vec_format(2024), "2024")
  expect_equal(NCRNWater:::vec_format(c(2023, 2024)), "2023 and 2024")
  expect_equal(NCRNWater:::vec_format(c(2021, 2022, 2023)), "2021, 2022, and 2023")
})
