# tests/testthat/test-example-data.R
# 
# Example usage:
# testthat::test_file("tests/testthat/test-example-data.R")
# 

library(NCRNWater)
library(testthat)

testthat::test_that("example paths exist and load",{
  paths <- NCRNWater::example_paths()
  testthat::expect_true(file.exists(paths$data_fp))
  testthat::expect_true(file.exists(paths$metadata_fp))
})

testthat::test_that("example dataframes load", {
  
  ex <- NCRNWater::example_data(assign = FALSE, reader = "utils")
  testthat::expect_true(is.data.frame(ex$wqp))
  testthat::expect_true(is.data.frame(ex$wqp_metadata))
})

test_that("example_ncrnwater builds an NCRNWater object", {
  # Build object from package-shipped example files
  # wd <- NCRNWater::example_ncrnwater() # loud, shows warnings that come from metadata IsActive filtering
  wd <- quiet_example_ncrnwater() # quiet, hides warnings from metadata IsActive filtering
  
  # Top-level structure: list of Park S4 objects
  testthat::expect_true(is.list(wd))
  testthat::expect_true(length(wd) >= 1L)
  
  # Defensive: at least one element is an S4 'Park'
  testthat::expect_true(any(vapply(wd, function(p) methods::is(p, "Park"), logical(1))))
})
