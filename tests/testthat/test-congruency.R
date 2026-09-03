# tests/testthat/test-congruency.R
#
# ------------------------------------------------------------------------------
# Test module: test-congruency.R
#
# Test coverage summary for R/congruency.R
#
# This suite validates the congruency() helper that checks structural and semantic
# alignment between a user-provided WQP data CSV and NCRNWater metadata CSV,
# relative to shipped templates in inst/extdata/templates/.
#
# Covered behaviors:
#   • File existence and CSV extension checks (pass/fail)
#   • Column-name congruency (user vs template) for data and metadata
#   • Value congruency for key pairs:
#       MonitoringLocationIdentifier ~ SiteCode, SiteCodeWQX
#       MonitoringLocationName ~ SiteName
#       CharacteristicName ~ DataName
#   • Success path prints "OK to proceed!" and does not error
#   • Failure paths raise stop() with human-readable messages
#
# Notes:
#   • Tests use tempdir() and write small ephemeral CSVs—no modification to package files.
#   • Output is captured via expect_output() for the success case; failure cases use expect_error().
#   • These tests assume templates are installed under inst/extdata/templates/ and
#     congruency() resolves them via system.file().
#
# Run:
#
#   # Fast (sampled)
#   devtools::test(filter = "congruency")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-congruency.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

# ---- Helpers -----------------------------------------------------------------

# Write a data.frame to tempdir() as CSV; return its filepath
.write_csv <- function(df, fname) {
  fp <- file.path(tempdir(), fname)
  utils::write.csv(df, fp, row.names = FALSE)
  fp
}

# Build congruent pair (columns & values align)
.build_congruent_pair <- function() {
  # Values align across the pairs used in congruency_helper_values()
  data_df <- data.frame(
    MonitoringLocationIdentifier = c("NCRN_ABC", "NCRN_DEF"),
    MonitoringLocationName       = c("ABC Site", "DEF Site"),
    CharacteristicName           = c("ANC", "ANC"),
    ResultMeasureValue           = c(6.2, 5.0),
    stringsAsFactors = FALSE
  )
  
  meta_df <- data.frame(
    SiteCode    = c("NCRN_ABC", "NCRN_DEF"),
    SiteCodeWQX = c("NCRN_ABC", "NCRN_DEF"),
    SiteName    = c("ABC Site", "DEF Site"),
    DataName    = c("ANC", "ANC"),
    LowerPoint  = c(5, 5),
    UpperPoint  = c(8, 8),
    stringsAsFactors = FALSE
  )
  
  list(
    data_fp = .write_csv(data_df, "user_wqp.csv"),
    meta_fp = .write_csv(meta_df, "user_meta.csv")
  )
}

# Build incongruent values pair (e.g., site code mismatch)
.build_incongruent_values_pair <- function() {
  data_df <- data.frame(
    MonitoringLocationIdentifier = c("NCRN_ABC", "NCRN_XYZ"), # XYZ not in metadata
    MonitoringLocationName       = c("ABC Site", "XYZ Site"),
    CharacteristicName           = c("ANC", "ANC"),
    ResultMeasureValue           = c(6.2, 5.0),
    stringsAsFactors = FALSE
  )
  meta_df <- data.frame(
    SiteCode    = c("NCRN_ABC", "NCRN_DEF"),
    SiteCodeWQX = c("NCRN_ABC", "NCRN_DEF"),
    SiteName    = c("ABC Site", "DEF Site"),
    DataName    = c("ANC", "ANC"),
    LowerPoint  = c(5, 5),
    UpperPoint  = c(8, 8),
    stringsAsFactors = FALSE
  )
  list(
    data_fp = .write_csv(data_df, "bad_user_wqp.csv"),
    meta_fp = .write_csv(meta_df, "bad_user_meta.csv")
  )
}

# Build incongruent columns pair (user data missing a required column, + extra col)
.build_incongruent_columns_pair <- function() {
  # Remove MonitoringLocationName; add an extra unexpected column
  data_df <- data.frame(
    MonitoringLocationIdentifier = c("NCRN_ABC"),
    CharacteristicName           = c("ANC"),
    ResultMeasureValue           = c(6.2),
    ExtraCol                     = c("unused"),
    stringsAsFactors = FALSE
  )
  meta_df <- data.frame(
    SiteCode    = c("NCRN_ABC"),
    SiteCodeWQX = c("NCRN_ABC"),
    SiteName    = c("ABC Site"),
    DataName    = c("ANC"),
    LowerPoint  = c(5),
    UpperPoint  = c(8),
    stringsAsFactors = FALSE
  )
  list(
    data_fp = .write_csv(data_df, "bad_cols_user_wqp.csv"),
    meta_fp = .write_csv(meta_df, "bad_cols_user_meta.csv")
  )
}

# ---- Tests -------------------------------------------------------------------

test_that("[congruency] templates resolve under inst/extdata/templates", {
  wqp_tmpl  <- system.file("extdata", "templates/wqp.csv", package = "NCRNWater")
  meta_tmpl <- system.file("extdata", "templates/wqp_ncrnwater_metadata.csv", package = "NCRNWater")
  expect_true(nzchar(wqp_tmpl)  && file.exists(wqp_tmpl))
  expect_true(nzchar(meta_tmpl) && file.exists(meta_tmpl))
})

# Error: nonexistent files → problems reported and stop()
test_that("[congruency] nonexistent files cause error", {
  expect_error(
    NCRNWater::congruency("no_such_wqp.csv", "no_such_meta.csv"),
    regexp = "does not exist|congruency problem\\(s\\)",
    ignore.case = TRUE
  )
})

# Error: not CSV → problems reported and stop()
test_that("[congruency] non-CSV inputs cause error", {
  bad_wqp  <- file.path(tempdir(), "user_wqp.txt")
  bad_meta <- file.path(tempdir(), "user_meta.txt")
  file.create(bad_wqp)
  file.create(bad_meta)

  expect_error(
    NCRNWater::congruency(bad_wqp, bad_meta),
    regexp = "is not a CSV file|congruency problem\\(s\\)",
    ignore.case = TRUE
  )
})

# Error: mismatched columns → problems reported and stop()
test_that("[congruency] column name mismatch causes error", {
  pair <- .build_incongruent_columns_pair()
  expect_error(
    NCRNWater::congruency(pair$data_fp, pair$meta_fp),
    regexp = "column names .* do not match|congruency problem\\(s\\)",
    ignore.case = TRUE
  )
})

# Error: mismatched values across key pairs → problems reported and stop()
test_that("[congruency] value mismatch causes error", {
  pair <- .build_incongruent_values_pair()
  expect_error(
    NCRNWater::congruency(pair$data_fp, pair$meta_fp),
    regexp = "values .* do not match|congruency problem\\(s\\)",
    ignore.case = TRUE
  )
})

# Success path: congruent files → prints OK and no error (against official templates)
test_that("[congruency] congruent user files pass and print OK (against official templates)", {
  # Resolve official templates shipped with the package
  wqp_tmpl  <- system.file("extdata", "templates/wqp.csv", package = "NCRNWater")
  meta_tmpl <- system.file("extdata", "templates/wqp_ncrnwater_metadata.csv", package = "NCRNWater")
  expect_true(nzchar(wqp_tmpl)  && file.exists(wqp_tmpl))
  expect_true(nzchar(meta_tmpl) && file.exists(meta_tmpl))
  
  # Read templates to obtain canonical column sets
  wqp_tmpl_df  <- utils::read.csv(wqp_tmpl,  stringsAsFactors = FALSE)
  meta_tmpl_df <- utils::read.csv(meta_tmpl, stringsAsFactors = FALSE)
  
  # Build a 1-row user WQP data frame with EXACT template columns
  user_wqp_df <- as.data.frame(
    matrix(nrow = 1, ncol = ncol(wqp_tmpl_df)),
    stringsAsFactors = FALSE
  )
  names(user_wqp_df) <- names(wqp_tmpl_df)
  user_wqp_df[] <- NA
  
  # Build a 1-row user metadata data frame with EXACT template columns
  user_meta_df <- as.data.frame(
    matrix(nrow = 1, ncol = ncol(meta_tmpl_df)),
    stringsAsFactors = FALSE
  )
  names(user_meta_df) <- names(meta_tmpl_df)
  user_meta_df[] <- NA
  
  # Choose a single consistent site and parameter value
  site_code   <- "NCRN_ABC"
  site_name   <- "ABC Site"
  param_name  <- "Specific conductance"  # any string; must match in both files
  
  # Populate the key value pairs that congruency() checks
  # WQP (data)
  if ("MonitoringLocationIdentifier" %in% names(user_wqp_df)) user_wqp_df$MonitoringLocationIdentifier <- site_code
  if ("MonitoringLocationName"       %in% names(user_wqp_df)) user_wqp_df$MonitoringLocationName       <- site_name
  if ("CharacteristicName"           %in% names(user_wqp_df)) user_wqp_df$CharacteristicName           <- param_name
  if ("ResultMeasureValue"           %in% names(user_wqp_df)) user_wqp_df$ResultMeasureValue           <- 1.23
  
  # Metadata (NCRNWater)
  if ("SiteCode"    %in% names(user_meta_df)) user_meta_df$SiteCode    <- site_code
  if ("SiteCodeWQX" %in% names(user_meta_df)) user_meta_df$SiteCodeWQX <- site_code
  if ("SiteName"    %in% names(user_meta_df)) user_meta_df$SiteName    <- site_name
  if ("DataName"    %in% names(user_meta_df)) user_meta_df$DataName    <- param_name
  if ("LowerPoint"  %in% names(user_meta_df)) user_meta_df$LowerPoint  <- 0
  if ("UpperPoint"  %in% names(user_meta_df)) user_meta_df$UpperPoint  <- 999
  
  # Write user files to tempdir
  user_wqp  <- file.path(tempdir(), "user_wqp.csv")
  user_meta <- file.path(tempdir(), "user_meta.csv")
  utils::write.csv(user_wqp_df,  user_wqp,  row.names = FALSE)
  utils::write.csv(user_meta_df, user_meta, row.names = FALSE)
  
  # Now check user files (1-row, congruent values) against official templates (schema)
  expect_output(
    expect_no_error(
      NCRNWater::congruency(
        data_filename     = user_wqp,
        metadata_filename = user_meta,
        data_template     = wqp_tmpl,
        metadata_template = meta_tmpl
      )
    ),
    regexp = "OK to proceed!"
  )
})
