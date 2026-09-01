# tests/testthat/test-exceed.R
#
# Example usage:
#   testthat::test_file("tests/testthat/test-exceed.R")
#   devtools::test(filter = "exceed")
#
# This file relies on the shared fixture defined in helper-fixtures.R:
#   WaterData <- get_waterdata_fixture()

library(testthat)
library(NCRNWater)

test_that("exceed returns per-site rows aligned with thresholds and is deduped", {
  # Use the shared fixture (memoized across tests)
  WaterData <- get_waterdata_fixture()
  
  # Global example across the network (summary mode)
  df <- NCRNWater::exceed(WaterData, charname = "TotalN")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 1L)
  
  expect_true(
    all(c("Park","Site","Characteristic","Category",
          "Total","Acceptable","TooLow","TooHigh","AllExceed") %in% names(df))
  )
  
  # No duplicate Park/Site/Characteristic/Category rows
  key <- paste(df$Park, df$Site, df$Characteristic, df$Category, sep = "|")
  expect_identical(length(key), length(unique(key)))
  
  # Filtered example (single park)
  df2 <- NCRNWater::exceed(WaterData, parkcode = "NACE", charname = "ANC")
  expect_true(nrow(df2) >= 1L)
  
  key2 <- paste(df2$Park, df2$Site, df2$Characteristic, df2$Category, sep = "|")
  expect_identical(length(key2), length(unique(key2)))
  
  # Custom thresholds: scalar recycled; should run without error
  df3 <- NCRNWater::exceed(WaterData, charname = "ANC", lower = 5, upper = NA)
  expect_s3_class(df3, "data.frame")
})



test_that("exceed rows mode returns exceeded measurements with context columns", {
  wd <- get_waterdata_fixture()
  
  rows <- NCRNWater::exceed(wd, charname = "ANC", points = "both", mode = "rows")
  expect_s3_class(rows, "data.frame")
  expect_rows_schema(rows)  # from helper-fixtures.R
})
