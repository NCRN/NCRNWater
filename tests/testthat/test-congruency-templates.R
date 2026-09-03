# tests/testthat/test-congruency-templates.R
#
# ------------------------------------------------------------------------------
# Test module: test-congruency-templates.R
#
# Test coverage summary for template file resolution used in R/congruency.R
#
# This lightweight module validates that the NCRNWater template CSV files
# (wqp.csv and wqp_ncrnwater_metadata.csv) are correctly installed under
# inst/extdata/templates/ and discoverable via system.file(). These templates
# are required for congruency() to perform structure and semantic checks on
# user-supplied WQP data files and metadata files.
#
# Covered behaviors:
#   • system.file() returns non-empty strings for both template files.
#   • Template filepaths resolve inside the installed NCRNWater package.
#   • Template files physically exist on disk under inst/extdata/templates/.
#
# Notes:
#   • This module does not test congruency() logic itself; those are covered in
#     test-congruency.R. This file tests ONLY that the template assets can be
#     located reliably by the package code.
#   • These tests are fast, side-effect-free, and do not load or parse the CSVs.
#
# Run:
#
#   # Fast / sampled
#   devtools::test(filter = "congruency-templates")
#
#   # Exhaustive mode (no distinction; same behavior)
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "congruency-templates")
#
#   # Run this file directly
#   testthat::test_file("tests/testthat/test-congruency-templates.R")
#
# ------------------------------------------------------------------------------

library(testthat)
library(NCRNWater)

test_that("[congruency] template files resolve under inst/extdata/templates", {
  wqp_tmpl  <- system.file("extdata", "templates/wqp.csv", package = "NCRNWater")
  meta_tmpl <- system.file("extdata", "templates/wqp_ncrnwater_metadata.csv", package = "NCRNWater")
  expect_true(nzchar(wqp_tmpl)  && file.exists(wqp_tmpl))
  expect_true(nzchar(meta_tmpl) && file.exists(meta_tmpl))
})
