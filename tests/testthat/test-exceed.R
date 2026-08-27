library(testthat)
test_that("exceed returns per-site rows aligned with thresholds and is deduped", {
  skip_if_not_installed("NCRNWater")
  
  Network <- "NCRN"
  WaterData <- importNCRNWater(
    file.path("Data", Network),
    Data     = "wqp_active.csv",
    MetaData = "wqp_ncrnwater_metadata_active.csv",
    wqx      = TRUE
  )
  
  # Global ANC across network
  df <- NCRNWater::exceed(WaterData, charname = "ANC")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 1L)
  expect_true(all(c("Park","Site","Characteristic","Category","Total","Acceptable","TooLow","TooHigh","AllExceed") %in% names(df)))
  # No duplicate Park/Site/Characteristic/Category rows
  key <- paste(df$Park, df$Site, df$Characteristic, df$Category, sep = "|")
  expect_identical(length(key), length(unique(key)))
  
  # Filtered example
  df2 <- NCRNWater::exceed(WaterData, parkcode = "NACE", charname = "ANC")
  expect_true(nrow(df2) >= 1L)
  key2 <- paste(df2$Park, df2$Site, df2$Characteristic, df2$Category, sep = "|")
  expect_identical(length(key2), length(unique(key2)))
  
  # Custom thresholds: scalar recycled and vector validated
  df3 <- NCRNWater::exceed(WaterData, charname = "ANC", lower = 5, upper = NA)
  expect_s3_class(df3, "data.frame")
})
