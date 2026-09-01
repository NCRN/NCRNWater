# tests/testthat/test-examples.R
#
# Test all functionality related to example data, paths, and convenience object
#
# ---------------------------------------------------------------------------
# Example usage (devtools / testthat):
#   devtools::test(filter = "examples")
#   testthat::test_file("tests/testthat/test-examples.R")
#   testthat::test_file("tests/testthat/test-examples.R", filter = "\\[examples\\]\\s*paths valid$")
#
# Changelog
# 2026-08-28: cw, initial version
# ---------------------------------------------------------------------------

library(testthat)

# ---------------------------
# example_paths() tests
# ---------------------------

test_that("examples paths valid", {
  paths <- NCRNWater::example_paths("NCRN")
  
  # Directory exists
  expect_true(dir.exists(paths$dir))
  
  # Files exist
  expect_true(file.exists(paths$data_fp))
  expect_true(file.exists(paths$metadata_fp))
  
  # Basenames parsed correctly (no hard-coded names)
  expect_identical(basename(paths$data_fp),     paths$data)
  expect_identical(basename(paths$metadata_fp), paths$metadata)
  
  # Directory alignment: file paths live under the returned dir
  expect_identical(dirname(paths$data_fp),     paths$dir)
  expect_identical(dirname(paths$metadata_fp), paths$dir)
  
})

test_that("examples paths unknown network errors cleanly", {
  
  expect_error(
    NCRNWater::example_paths("NO_SUCH_NETWORK"),
    regexp = "not found",
    ignore.case = TRUE
  )

})

# ---------------------------
# example_data() tests
# ---------------------------

test_that("examples data via utils returns data.frames", {
  
  ex <- NCRNWater::example_data(assign = FALSE, reader = "utils")
  expect_true(is.data.frame(ex$wqp))
  expect_true(is.data.frame(ex$wqp_metadata))
  
  # basic shape sanity
  expect_true(nrow(ex$wqp) >= 0L)
  expect_true(nrow(ex$wqp_metadata) >= 1L)
  
})

test_that("examples data via readr returns tibbles then data.frames", {
  
  skip_if_not_installed("readr")
  
  # as tibble (default)
  ex_tb <- NCRNWater::example_data(assign = FALSE, reader = "readr", as_tibble = TRUE)
  expect_s3_class(ex_tb$wqp,          "tbl_df")
  expect_s3_class(ex_tb$wqp_metadata, "tbl_df")
  
  # as data.frame
  ex_df <- NCRNWater::example_data(assign = FALSE, reader = "readr", as_tibble = FALSE)
  expect_true(is.data.frame(ex_df$wqp))
  expect_true(is.data.frame(ex_df$wqp_metadata))
  expect_false(inherits(ex_df$wqp, "tbl_df"))
  expect_false(inherits(ex_df$wqp_metadata, "tbl_df"))

})

test_that("examples data assigns into provided environment with custom names", {
  
  # Use a private environment rather than .GlobalEnv
  target_env <- new.env(parent = emptyenv())
  
  ex <- NCRNWater::example_data(
    assign        = TRUE,
    reader        = "utils",
    envir         = target_env,
    data_name     = "wqpX",
    metadata_name = "wqp_mdX"
  )
  
  # Objects were created in the target environment
  expect_true(exists("wqpX",    envir = target_env))
  expect_true(exists("wqp_mdX", envir = target_env))
  
  # Assigned values match the returned objects
  expect_equal(get("wqpX",    envir = target_env), ex$wqp)
  expect_equal(get("wqp_mdX", envir = target_env), ex$wqp_metadata)
  
  # Nothing leaked into the global environment inadvertently
  expect_false(exists("wqpX",    envir = .GlobalEnv))
  expect_false(exists("wqp_mdX", envir = .GlobalEnv))

})

# ---------------------------
# example_ncrnwater() tests
# ---------------------------

test_that("examples example_ncrnwater builds object and exceed works (summary)", {
  
  # If your example data is large or slow, uncomment the next line:
  # skip_on_cran()
  
  wd <- NCRNWater::example_ncrnwater()
  # Top-level type: list of Park objects
  expect_true(is.list(wd))
  expect_true(length(wd) >= 1L)
  
  # Optional: at least one element is an S4 Park (defensive check)
  expect_true(any(vapply(wd, function(p) methods::is(p, "Park"), logical(1))))
  
  # exceed() summary-mode returns expected columns
  df <- NCRNWater::exceed(wd)
  expect_true(is.data.frame(df))
  expect_true(all(c("Park", "Site", "Characteristic", "Category",
                    "Total", "Acceptable", "TooLow", "TooHigh", "AllExceed")
                  %in% names(df)))

})
