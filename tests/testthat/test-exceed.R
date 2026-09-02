# ------------------------------------------------------------------------------
# Test module: test-exceed.R
#
# Test coverage summary for R/exceed.R
#
# This suite exercises both summary- and rows-mode paths of exceed(), validating
# schema, deduplication, points behavior, operator overrides, zero-exceed, and
# empty-selection handling. Parameterized tests run on sampled or exhaustive
# (park, site, param) combinations.
#
# Covered behaviors:
#   • Summary schema:
#       - Park, Site, Characteristic, Category, Total, Acceptable, TooLow, TooHigh, AllExceed.
#       - Key dedupe: Park|Site|Characteristic|Category (or Park|Site|Category when catsum=TRUE).
#   • points argument:
#       - points="lower" → TooHigh is NA; points="upper" → TooLow is NA.
#   • catsum=TRUE:
#       - Groups by Park/Site/Category; distinct on those columns.
#   • all=TRUE:
#       - Includes characteristics without thresholds; row count non-decreasing vs. default.
#   • Rows-mode schema:
#       - LowerPoint, UpperPoint, LowerPointCondition, UpperPointCondition,
#         Exceed_Lower, Exceed_Upper, Exceed_Type; Type consistent with flags.
#   • Operator overrides:
#       - lower_op="le"/upper_op="ge" treat equality as exceed; skipped once per park if no equality occurs.
#   • Zero-exceed:
#       - Impossible bounds (-Inf/Inf) produce empty data frame with full rows-mode schema.
#   • Empty selection:
#       - Non-existent filters yield empty summary schema without errors; warning muffled.
#
# Notes:
#   • Uses getWD() and sample_n_valid_combos()/list_all_valid_combos() and
#     list_all_threshold_combos() (for rows mode).
#   • Equality checks employ skip-once semantics to avoid chatty logs.
#   • Known benign warnings (staging, “No sites match…”) can be muffled via a quiet wrapper.
#
# Run:
#
#   # Fast (sampled cases)
#   devtools::test(filter = "exceed")
#
#   # Exhaustive (all combinations)
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "exceed")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-exceed.R")
#
# ------------------------------------------------------------------------------
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

