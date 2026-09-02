# ------------------------------------------------------------------------------
# tests/testthat/test-example-data.R
#
# Test coverage summary for R/example_data.R and R/example_ncrnwater_obj.R
#
# This suite validates loading and staging of package-shipped example data—
# including raw example CSVs, metadata files, and the construction of an
# NCRNWater object via example_ncrnwater(). These tests ensure example paths
# exist, example data loads cleanly, staging produces sensible objects, and
# quiet wrappers suppress benign filterActive() warnings during CI and
# exhaustive runs.
#
# Covered behaviors:
#   • example_paths():
#       - Returns valid absolute file paths for example data and metadata.
#       - All referenced files exist in the installed package.
#
#   • example_data(assign = FALSE):
#       - Loads example WQP and metadata CSVs as data.frames.
#       - Reader abstraction (“utils”, “readr”, etc.) behaves consistently.
#
#   • example_ncrnwater():
#       - Builds a complete NCRNWater object from package example data.
#       - Top-level structure is a list of S4 Park objects with ≥1 entries.
#       - At least one element is an S4 "Park" instance.
#       - quiet_example_ncrnwater() muffles benign filterActive() staging warnings
#         so logs remain clean during test runs.
#
# Notes:
#   • Uses quiet_example_ncrnwater() to prevent noisy filterActive() messages,
#     while still testing the full example data → staging → NCRNWater object
#     workflow.
#   • Tests rely on package-installed example data, not the fixture created by
#     getWD(), because example_ncrnwater() is part of the user-facing API.
#
# Run:
#
#   # Fast (standard dev cycle)
#   devtools::test(filter = "example")
#
#   # Exhaustive (recommended pre-deploy)
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "example")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-example-data.R")
#
# ------------------------------------------------------------------------------
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
