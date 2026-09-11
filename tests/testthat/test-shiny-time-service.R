# tests/testthat/test-shiny-time-service.R
#
# ------------------------------------------------------------------------------
# Test module: test-shiny-time-service.R
#
# Test coverage summary for R/shinyTimeService.R (resolve_year_bounds)
#
# This suite validates late-stage resolution of global year bounds when the
# app configuration specifies "auto" for time.min_year/time.max_year. It asserts:
#   • Years are derived from hydrated data (wd) when WQP path is unavailable.
#   • WQP-disabled "auto" still derives from hydrated data.
#   • Fallback behavior returns numeric config values when wd is not supplied.
#
# Notes:
#   • Uses getWD() from helper-fixtures.R to hydrate a fixture NCRNWater object
#     without touching user-local Data/ paths.
#   • Known benign filterActive() staging warnings are muffled by getWD().
#   • We do not assert exact min/max values (dataset-dependent); instead we assert
#     type correctness, monotonicity, and the 'source' flag semantics.
#
# Run:
#   devtools::test(filter = "shiny-time-service")
#   testthat::test_file("tests/testthat/test-shiny-time-service.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

# Pull in test helpers (getWD(), etc.)
testthat::source_test_helpers()

make_cfg_auto <- function(network = "NCRN", wqx = TRUE) {
  list(
    app   = list(network_code = network),
    files = list(dataname = "wqp.csv", metadataname = "wqp_ncrnwater_metadata.csv"),
    wqx   = list(enabled = wqx),
    time  = list(min_year = "auto", max_year = "auto")
  )
}

test_that("[resolve_year_bounds] derives years from hydrated data when 'auto' and WQP enabled", {
  wd  <- getWD()  # fixture hydration; benign warnings muffled
  cfg <- make_cfg_auto(network = names(wd)[1], wqx = TRUE)
  
  yrs <- NCRNWater:::resolve_year_bounds(cfg, wd = wd)
  
  expect_true(is.list(yrs))
  expect_true(is.numeric(yrs$min_year))
  expect_true(is.numeric(yrs$max_year))
  expect_true(yrs$min_year <= yrs$max_year)
  expect_identical(yrs$source, "derived")
})

test_that("[resolve_year_bounds] derives years from hydrated data when 'auto' and WQP disabled", {
  wd  <- getWD()
  cfg <- make_cfg_auto(network = names(wd)[1], wqx = FALSE)
  
  yrs <- NCRNWater:::resolve_year_bounds(cfg, wd = wd)
  
  expect_true(is.numeric(yrs$min_year))
  expect_true(is.numeric(yrs$max_year))
  expect_true(yrs$min_year <= yrs$max_year)
  expect_identical(yrs$source, "derived")
})

test_that("[resolve_year_bounds] falls back to numeric config values when wd is NULL", {
  cfg <- list(
    app   = list(network_code = "NCRN"),
    files = list(dataname = "wqp.csv", metadataname = "wqp_ncrnwater_metadata.csv"),
    wqx   = list(enabled = TRUE),
    time  = list(min_year = 2005, max_year = 2025)
  )
  
  yrs <- NCRNWater:::resolve_year_bounds(cfg, wd = NULL)
  
  expect_identical(yrs$min_year, 2005)
  expect_identical(yrs$max_year, 2025)
  expect_identical(yrs$source,  "fallback")
})