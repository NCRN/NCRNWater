# tests/testthat/test-shiny-hydration-service.R
#
# ------------------------------------------------------------------------------
# Test module: test-shiny-hydration-service.R
#
# Test coverage summary for R/hydration_service.R
#
# This suite validates the network hydration service that prepares the Shiny app’s
# data layer: it loads the NCRNWater object (wd) from *raw* example CSVs and reads
# the “active” metadata created by filterActive(). Tests assert shape/types and
# non-error behavior using package-shipped assets under inst/extdata/NCRN.
#
# Covered behaviors:
#   • hydrate_network():
#       - Accepts raw filenames (e.g., wqp.csv, wqp_ncrnwater_metadata.csv).
#       - Internally relies on importNCRNWater() → filterActive() to generate
#         *_active.csv files in the staging directory.
#       - Returns a list with wd, metadata_active, and root fields.
#       - wd is a list of Park S4 objects; metadata_active is a data.frame.
#   • Robust I/O:
#       - Works against a temp staging layout: file.path(tempdir(), "NCRN").
#       - Does not touch app-side Data/ folders or modify package-installed files.
#
# Notes:
#   • We muffle only the known benign filterActive() warning; other warnings must
#     surface. The test is side-effect free (writes to tempdir()) and portable.
#
# Run:
#
#   # Fast (sampled)
#   devtools::test(filter = "hydration-service")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-shiny-hydration-service.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

test_that("[hydrate_network] builds wd + active metadata from raw example CSVs (inst/extdata/NCRN)", {
  # Resolve package-shipped raw example files
  base_ext <- system.file("extdata", package = "NCRNWater")
  network  <- "NCRN"
  
  raw_wqp   <- file.path(base_ext, network, "wqp.csv")
  raw_meta  <- file.path(base_ext, network, "wqp_ncrnwater_metadata.csv")
  
  expect_true(nzchar(base_ext) && file.exists(base_ext))
  expect_true(file.exists(raw_wqp))
  expect_true(file.exists(raw_meta))
  
  # Stage into a temp "Data/NCRN" layout (side-effect free)
  td        <- tempdir()
  stage_dir <- file.path(td, network)
  dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
  
  file.copy(raw_wqp,  file.path(stage_dir, "wqp.csv"),                   overwrite = TRUE)
  file.copy(raw_meta, file.path(stage_dir, "wqp_ncrnwater_metadata.csv"), overwrite = TRUE)
  
  # Call hydration service against temp staging dir, muffling ONLY the known benign warning
  res <- withCallingHandlers(
    hydrate_network(
      network              = network,
      base_dir             = td,   # raw inputs at tempdir()/NCRN
      dataname             = "wqp.csv",
      metadataname         = "wqp_ncrnwater_metadata.csv",
      wqx                  = TRUE
      # NOTE: We DO NOT pass *_active names; importNCRNWater() → filterActive() must create them.
    ),
    warning = function(w) {
      if (grepl("^filterActive\\(\\): dropped \\d+ inactive metadata rows", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  
  # Structure & types
  expect_true(is.list(res))
  expect_true(all(c("wd", "metadata_active", "root") %in% names(res)))
  
  expect_true(is.list(res$wd))
  expect_true(length(res$wd) >= 1L)
  expect_true(any(vapply(res$wd, function(p) methods::is(p, "Park"), logical(1))))
  
  expect_s3_class(res$metadata_active, "data.frame")
  expect_identical(res$root, file.path(td, network))
  
  # Optional: verify *_active.csv files were produced in the staging dir
  # (This asserts importNCRNWater() invoked filterActive() under the hood.)
  active_wqp  <- file.path(stage_dir, "wqp_active.csv")
  active_meta <- file.path(stage_dir, "wqp_ncrnwater_metadata_active.csv")
  expect_true(file.exists(active_wqp))
  expect_true(file.exists(active_meta))
})
