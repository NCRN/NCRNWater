# tests/testthat/test-shiny-photo-service.R
#
# ------------------------------------------------------------------------------
# Test module: test-shiny-photo-service.R
#
# Test coverage summary for R/shinyPhotoService.R
#
# This suite validates the Shiny-specific photo parsing service `parsePhotos()`.
# It stages a small set of JPG filenames in a temporary directory, covering the
# three supported naming conventions:
#   1) WATER_<PARK>_<SITE>_<YYYYMMDD> (<index>).JPG
#   2) dwq|cwq_NCRN_<PARK>_<SITE>_<YYYY-MM-DD>_<YYYYMMDD-HHMMSS>.jpg
#   3) <PARK>_<SITE>_<YYYYMMDD> (<index>).JPG
# The test asserts the nested structure (park -> site -> year -> sitevisit ->
# filename), verifies `rel_fpath` and `sortorder`, and checks that non-JPG files
# are ignored by the service.
#
# Covered behaviors:
#   • Structure: nested list keys (park, normalized site, year, sitevisit, filename).
#   • Values: `rel_fpath` (full path) and `sortorder` (index/timestamp) are present.
#   • Site normalization: site key is `"NCRN_<PARK>_<SITE>"`.
#   • Site visit label uses resolved site name via getSiteInfo(..., info="SiteName").
#   • Non-JPG files are ignored.
#
# Notes:
#   • Uses `getWD()` + `enumerate_combos()` from helper-fixtures.R, ensuring park/site
#     combinations exist in the fixture.
#   • All files are written to `tempdir()`; no package-installed assets are modified.
#
# Run:
#
#   # Fast (sampled)
#   devtools::test(filter = "shiny-photo-service")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-shiny-photo-service.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

# Ensure helpers (getWD(), enumerate_combos(), etc.) are available
testthat::source_test_helpers()

test_that("[parsePhotos] builds nested index for the three naming conventions and ignores non-JPG", {
  # Hydrate fixture and choose one valid (park, site) combo
  wd <- getWD()
  cases <- sample_n_valid_combos(wd, n_cases = 1L)
  case  <- cases[[1]]
  park  <- case$park       # e.g., "ANTI"
  site_full <- case$site   # e.g., "NCRN_ANTI_SHCK"
  
  # Derive the short site token expected by filenames (i.e., strip "NCRN_<PARK>_")
  site_short <- sub(paste0("^NCRN_", park, "_"), "", site_full)
  
  # Build a tiny photo set in tempdir()
  img_dir <- file.path(tempdir(), "photos_service_test")
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Fixed date components (YYYYMMDD, YYYY-MM-DD, timestamp)
  ymd       <- "20240201"
  ymd_dash  <- "2024-02-01"
  timestamp <- "20240201-084406"
  
  # 1) WATER_<PARK>_<SITE>_<YYYYMMDD> (<index>).JPG
  f1 <- sprintf("WATER_%s_%s_%s (1).JPG", park, site_short, ymd)
  
  # 2) dwq_NCRN_<PARK>_<SITE>_<YYYY-MM-DD>_<YYYYMMDD-HHMMSS>.jpg
  f2 <- sprintf("dwq_NCRN_%s_%s_%s_%s.jpg", park, site_short, ymd_dash, timestamp)
  
  # 3) <PARK>_<SITE>_<YYYYMMDD> (<index>).JPG
  f3 <- sprintf("%s_%s_%s (8).JPG", park, site_short, ymd)
  
  # Non-JPG file should be ignored
  f4 <- "not_a_photo.txt"
  
  # Write files
  writeLines("stub", file.path(img_dir, f1))
  writeLines("stub", file.path(img_dir, f2))
  writeLines("stub", file.path(img_dir, f3))
  writeLines("stub", file.path(img_dir, f4))
  
  # Parse
  imgs <- NCRNWater::parsePhotos(directory = img_dir, object = wd)
  
  # Expected keys
  expect_true(is.list(imgs))
  expect_true(park %in% names(imgs))
  
  site_norm <- paste0("NCRN_", park, "_", site_short)
  expect_true(site_norm %in% names(imgs[[park]]))
  
  # Year node expected (from ymd)
  year_key <- substr(ymd, 1, 4)
  expect_true(year_key %in% names(imgs[[park]][[site_norm]]))
  
  # Site visit label should be "<SiteName> YYYY-MM-DD"
  site_name <- NCRNWater::getSiteInfo(wd, parkcode = park, sitecode = site_full, info = "SiteName")
  visit_key <- paste0(site_name, " ", substr(ymd, 1, 4), "-", substr(ymd, 5, 6), "-", substr(ymd, 7, 8))
  expect_true(visit_key %in% names(imgs[[park]][[site_norm]][[year_key]]))
  
  # Check each filename node for presence and fields
  node <- imgs[[park]][[site_norm]][[year_key]][[visit_key]]
  
  # WATER_... (index in parentheses)
  expect_true(f1 %in% names(node))
  expect_true(is.list(node[[f1]]))
  expect_true(file.exists(node[[f1]]$rel_fpath))
  expect_equal(node[[f1]]$sortorder, "1")
  
  # dwq_... (timestamp sortorder)
  expect_true(f2 %in% names(node))
  expect_true(is.list(node[[f2]]))
  expect_true(file.exists(node[[f2]]$rel_fpath))
  expect_equal(node[[f2]]$sortorder, timestamp)
  
  # PARK_SITE_... (index in parentheses)
  expect_true(f3 %in% names(node))
  expect_true(is.list(node[[f3]]))
  expect_true(file.exists(node[[f3]]$rel_fpath))
  expect_equal(node[[f3]]$sortorder, "8")
  
  # Non-JPG should be ignored
  expect_false(f4 %in% names(node))
})
