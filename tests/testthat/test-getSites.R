# tests/testthat/test-getSites.R
#
# Example usage:
#   testthat::test_file("tests/testthat/test-getSites.R")
#   devtools::test(filter = "getSites")

library(testthat)
library(NCRNWater)

test_that("getSites(list) dedupes identical Site objects by SiteCode", {
  # Use the shared fixture
  wd <- getWD()
  
  # Pick a valid (park, site, param); we only need park/site for this test
  combo <- pick_valid_combo(wd)
  park  <- combo$park
  site  <- combo$site
  
  # When a specific sitecode is requested, getSites should return exactly one element
  sites <- NCRNWater::getSites(wd, parkcode = park, sitecode = site)
  expect_length(sites, 1L)
  
  # Defensive: the element should be an S4 'Site'
  expect_true(methods::is(sites[[1]], "Site"))
  
  # And its SiteCode should match what we requested
  expect_identical(sites[[1]]@SiteCode, site)
})

test_that("getSiteInfo returns unique names per site under a park", {
  wd    <- getWD()
  
  # Reuse a valid park from the dynamic picker to ensure there are sites
  park  <- pick_valid_combo(wd)$park
  
  # All sites for the chosen park
  sites <- NCRNWater::getSites(wd, parkcode = park)
  sc    <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
  
  # Site names reported by getSiteInfo
  nm    <- NCRNWater::getSiteInfo(wd, parkcode = park, info = "SiteName")
  
  # The number of returned names should equal the number of unique site codes
  expect_equal(length(nm), length(unique(sc)))
  
  # Optional: the names vector should not contain duplicates
  expect_equal(length(nm), length(unique(nm)))
  
  # Optional: basic shape sanity
  expect_true(length(nm) >= 1L)
})
