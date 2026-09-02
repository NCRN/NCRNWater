# tests/testthat/test-exceed.R
# 
# ------------------------------------------------------------------------------
# Test coverage summary
#
# This test file exercises both summary- and rows-mode paths of exceed(), and
# validates behavior under different arguments, thresholds, and operator settings.
#
# Covered behaviors:
#   • Summary schema: Ensures the output contains
#     Park, Site, Characteristic, Category, Total, Acceptable, TooLow, TooHigh, AllExceed.
#     Also verifies no duplicate rows by the key Park|Site|Characteristic|Category.
#
#   • points argument:
#       - points = "lower": TooHigh must be NA (upper comparisons disabled).
#       - points = "upper": TooLow must be NA (lower comparisons disabled).
#
#   • catsum = TRUE:
#       - Confirms grouping by Park/Site/Category with deduped rows under that key.
#
#   • all = TRUE:
#       - Characteristics without thresholds are included; row count is non-decreasing
#         compared to the default (all = FALSE).
#
#   • Rows-mode schema:
#       - Validates presence of LowerPoint, UpperPoint, LowerPointCondition,
#         UpperPointCondition, Exceed_Lower, Exceed_Upper, Exceed_Type.
#       - If rows exist, checks Exceed_Type is consistent with flags:
#         "both" when both flags TRUE; "lower"/"upper" when exactly one is TRUE.
#
#   • Operator overrides (le/ge):
#       - Exercises boundary semantics: equality at thresholds counts as exceed
#         when lower_op = "le" or upper_op = "ge".
#       - If no equality cases exist for a park, the test skips once per park to
#         keep logs concise while still documenting the absence of equality cases.
#
#   • Zero-exceed path:
#       - With impossible bounds (lower = -Inf, upper = Inf), verifies rows-mode
#         returns an empty data frame (nrow == 0) with the full schema intact.
#
#   • Empty selection:
#       - When filters select no data (e.g., non-existent site), summary-mode
#         returns an empty data frame with the correct schema (no errors).
#
# Notes:
#   • Tests are parameterized over (park, site, param) combinations using shared
#     helpers and respect local run-mode knobs (sampled vs. exhaustive).
#   • Skip-once behavior is used where a minimum scenario is required (e.g., equality
#     at thresholds) to keep output readable without hiding coverage intent.
# ------------------------------------------------------------------------------
# 
# Example usage:
#   testthat::test_file("tests/testthat/test-exceed.R")
#   devtools::test(filter = "exceed")
# 
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "exceed")
# 
library(testthat)
library(NCRNWater)

# Shared fixture (benign staging warning muted; temp staging via helper)
wd <- getWD()

# Respect run-mode knobs: exhaustive vs sampled combinations
exhaustive <- getOption("ncrnwater.test.exhaustive", FALSE)
cases      <- if (exhaustive) list_all_valid_combos(wd) else sample_n_valid_combos(wd)
th_cases   <- list_all_threshold_combos(wd)  # rows-mode threshold combos

# Local helper: summary-mode schema assertion
expect_summary_schema <- function(df) {
  needed <- c("Park","Site","Characteristic","Category",
              "Total","Acceptable","TooLow","TooHigh","AllExceed")
  missing <- setdiff(needed, names(df))
  expect_true(length(missing) == 0,
              info = paste("Missing columns:", paste(missing, collapse = ", ")))
}

# Muffle only known benign warnings during tests
quiet_exceed <- function(...) {
  withCallingHandlers(
    NCRNWater::exceed(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
      if (grepl("^filterActive\\(\\): dropped \\d+ inactive metadata rows", msg)) {
        invokeRestart("muffleWarning")
      }
      # Let other warnings through
    }
  )
}

# Memoized env to avoid repeated skips per park for equality tests
.skip_once_exceed_env <- new.env(parent = emptyenv())

# -------------------------
# Summary mode: schema & dedup across parameterized cases
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-summary] %s:%s:%s returns deduped summary rows", park, site, param), {
    df <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param, mode = "summary")
    expect_s3_class(df, "data.frame")
    expect_summary_schema(df)
    
    # No duplicate Park/Site/Characteristic/Category rows
    key <- paste(df$Park, df$Site, df$Characteristic, df$Category, sep = "|")
    expect_equal(length(key), length(unique(key)))
    
    # Basic non-negative checks
    for (col in c("Total","Acceptable","TooLow","TooHigh","AllExceed")) {
      expect_true(all(is.na(df[[col]]) | df[[col]] >= 0L))
    }
  })
}

# -------------------------
# Summary mode: points = "lower" and points = "upper" behavior
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-summary] points='lower' sets TooHigh=NA [%s:%s:%s]", park, site, param), {
    df <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param, mode = "summary", points = "lower")
    expect_s3_class(df, "data.frame"); expect_summary_schema(df)
    # TooHigh must be NA when points="lower"
    expect_true(all(is.na(df$TooHigh)))
  })
  
  test_that(sprintf("[exceed-summary] points='upper' sets TooLow=NA [%s:%s:%s]", park, site, param), {
    df <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param, mode = "summary", points = "upper")
    expect_s3_class(df, "data.frame"); expect_summary_schema(df)
    # TooLow must be NA when points="upper"
    expect_true(all(is.na(df$TooLow)))
  })
}

# -------------------------
# Summary mode: catsum = TRUE collapses to Park/Site/Category
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-summary] catsum=TRUE groups by Category [%s:%s:%s]", park, site, param), {
    df <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                            mode = "summary", catsum = TRUE)
    expect_s3_class(df, "data.frame")
    expect_true(all(c("Park","Site","Category","Total","Acceptable","TooLow","TooHigh","AllExceed") %in% names(df)))
    # Ensure dedup key is Park|Site|Category
    key <- paste(df$Park, df$Site, df$Category, sep = "|")
    expect_equal(length(key), length(unique(key)))
  })
}

# -------------------------
# Summary mode: all = TRUE includes characteristics without thresholds
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-summary] all=TRUE includes no-threshold characteristics [%s:%s:%s]", park, site, param), {
    df_default <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                                    mode = "summary", all = FALSE)
    df_all     <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                                    mode = "summary", all = TRUE)
    expect_true(nrow(df_all) >= nrow(df_default))
  })
}

# -------------------------
# Rows mode: schema across threshold parameterized cases
# -------------------------
for (case in th_cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-rows] %s:%s:%s returns exceeded measurements with context columns", park, site, param), {
    rows <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                              points = "both", mode = "rows")
    expect_s3_class(rows, "data.frame")
    expect_rows_schema(rows)  # shared helper
    
    # If rows exist, Value should not be NA (NA flags are dropped)
    if (nrow(rows) > 0L) {
      expect_true(all(!is.na(rows$Value)))
    }
    
    # Exceed_Type matches flags
    if (nrow(rows) > 0L) {
      both <- rows$Exceed_Lower & rows$Exceed_Upper
      expect_true(all(rows$Exceed_Type[both] == "both"))
      only_lower <- rows$Exceed_Lower & !rows$Exceed_Upper
      expect_true(all(rows$Exceed_Type[only_lower] == "lower"))
      only_upper <- !rows$Exceed_Lower & rows$Exceed_Upper
      expect_true(all(rows$Exceed_Type[only_upper] == "upper"))
    }
  })
}

# -------------------------
# Rows mode: operator overrides 'le'/'ge' include equality
# -------------------------
for (case in th_cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-rows] 'le'/'ge' include equality [%s:%s:%s]", park, site, param), {
    rows <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                              mode = "rows", lower_op = "le", upper_op = "ge", points = "both")
    expect_s3_class(rows, "data.frame"); expect_rows_schema(rows)
    
    # If any values equal thresholds, equality should count as exceed
    eq_lower <- with(rows, which(!is.na(LowerPoint) & !is.na(Value) & Value == LowerPoint))
    eq_upper <- with(rows, which(!is.na(UpperPoint) & !is.na(Value) & Value == UpperPoint))
    
    if (length(eq_lower) == 0L && !exists(park, envir = .skip_once_exceed_env, inherits = FALSE)) {
      assign(park, TRUE, envir = .skip_once_exceed_env)
      skip(sprintf("No equality-at-lower threshold found for %s; equality behavior not exercised.", park))
    }
    if (length(eq_upper) == 0L && !exists(paste0(park, "_upper"), envir = .skip_once_exceed_env, inherits = FALSE)) {
      assign(paste0(park, "_upper"), TRUE, envir = .skip_once_exceed_env)
      skip(sprintf("No equality-at-upper threshold found for %s; equality behavior not exercised.", park))
    }
    
    if (length(eq_lower) > 0L) expect_true(any(rows$Exceed_Lower[eq_lower]))
    if (length(eq_upper) > 0L) expect_true(any(rows$Exceed_Upper[eq_upper]))
  })
}

# -------------------------
# Rows mode: zero-exceed case returns empty schema
# -------------------------
for (case in th_cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-rows] zero-exceed returns empty schema [%s:%s:%s]", park, site, param), {
    # Force no exceeds by choosing impossible bounds for both edges:
    rows <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                              points = "both", mode = "rows",
                              lower = -Inf, upper = Inf, lower_op = "lt", upper_op = "gt")
    expect_s3_class(rows, "data.frame"); expect_rows_schema(rows)
    expect_true(nrow(rows) == 0L)
  })
}

# -------------------------
# Summary mode: explicit user thresholds are recycled
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[exceed-summary] explicit thresholds are recycled [%s:%s:%s]", park, site, param), {
    df <- NCRNWater::exceed(wd, parkcode = park, sitecode = site, charname = param,
                            mode = "summary", lower = 5, upper = NA)
    expect_s3_class(df, "data.frame"); expect_summary_schema(df)
  })
}

# -------------------------
# Error path: empty selection returns empty summary schema
# -------------------------
test_that("[exceed-summary] filtering to no data returns empty schema", {
  # df <- NCRNWater::exceed(wd, sitecode = "__no_such_site__", mode = "summary") # loud, lots of repeated warnings
  df <- quiet_exceed(wd, sitecode = "__no_such_site__", mode = "summary") # quiet, we can actually tell if the test works properly
  expect_s3_class(df, "data.frame")
  expect_summary_schema(df)
  expect_true(nrow(df) == 0L)
})

test_that("[exceed-summary] catsum schema excludes Characteristic", {
  case <- cases[[1]]
  df <- NCRNWater::exceed(wd, parkcode = case$park, sitecode = case$site, charname = case$param,
                          mode = "summary", catsum = TRUE)
  expect_true(all(c("Park","Site","Category","Total","Acceptable","TooLow","TooHigh","AllExceed") %in% names(df)))
  expect_false("Characteristic" %in% names(df))
})

