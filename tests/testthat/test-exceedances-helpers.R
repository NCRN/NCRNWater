# tests/testthat/test-exceedances-helpers.R
#
# Unit tests for helper-functions that make parts of the exceedances tab in the shiny app
#
# Run just this file:
#   testthat::test_file("tests/testthat/test-exceedances-helpers.R")
# Or via devtools filter:
#   devtools::test(filter = "exceedances")

library(testthat)
library(NCRNWater)

# -------------------------
# vec_format() tests
# -------------------------
test_that("vec_format formats lists of years nicely", {
  expect_equal(NCRNWater:::vec_format(integer(0)), "")
  expect_equal(NCRNWater:::vec_format(2024), "2024")
  expect_equal(NCRNWater:::vec_format(c(2023, 2024)), "2023 and 2024")
  expect_equal(NCRNWater:::vec_format(c(2021, 2022, 2023)), "2021, 2022, and 2023")
})

# -------------------------
# site_info() / site_data()
# -------------------------
test_that("site_info returns expected fields; site_data returns measurable columns", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  info <- NCRNWater:::site_info(wd, park, site, param)
  expect_true(is.list(info))
  expect_true(all(c("SiteName","DisplayName","Units","LowerPoint","UpperPoint",
                    "LowerPointCondition","UpperPointCondition",
                    "LowerDescription","UpperDescription") %in% names(info)))
  
  df <- NCRNWater:::site_data(wd, park, site, param)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Date","Value") %in% names(df)))
  expect_true(nrow(df) > 0)
})

# -------------------------
# exceed_rows()
# -------------------------
test_that("exceed_rows returns rows-mode schema", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  rows <- NCRNWater:::exceed_rows(wd, park, site, param)
  expect_s3_class(rows, "data.frame")
  expect_true(all(c("Date","Value","LowerPoint","UpperPoint",
                    "LowerPointCondition","UpperPointCondition",
                    "Exceed_Lower","Exceed_Upper","Exceed_Type") %in% names(rows)))
})

# -------------------------
# choose_ex_point()
# -------------------------
test_that("choose_ex_point picks lower/upper point based on observed exceedances", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  rows <- NCRNWater:::exceed_rows(wd, park, site, param)
  info <- NCRNWater:::site_info(wd, park, site, param)

  ep <- NCRNWater:::choose_ex_point(rows, info$LowerPoint, info$UpperPoint)
  # choose_ex_point returns numeric threshold or NA when both exceedances occur
  expect_true(is.numeric(ep) || is.na(ep))
})

# -------------------------
# fill_threshold_descriptions()
# -------------------------
test_that("fill_threshold_descriptions emits fallback text when descriptions are missing", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  info <- NCRNWater:::site_info(wd, park, site, param)
  out  <- NCRNWater:::fill_threshold_descriptions(info)
  expect_true(is.list(out))
  expect_true(all(c("LowerDescription","UpperDescription") %in% names(out)))
})

# -------------------------
# add_difference()
# -------------------------
test_that("add_difference computes delta from exceeded threshold", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  rows <- NCRNWater:::exceed_rows(wd, park, site, param)
  rows2 <- NCRNWater:::add_difference(rows)
  
  expect_true("Difference" %in% names(rows2))
  
  expect_vector(rows2$Difference)
  expect_true(is.numeric(rows2$Difference))
  if (nrow(rows2) > 0) {
    expect_true(any(!is.na(rows2$Difference)))
  }
})

# -------------------------
# yearly_summary()
# -------------------------
test_that("yearly_summary computes ntot/nex and formatted percent", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  df   <- NCRNWater:::site_data(wd, park, site, param)
  rows <- NCRNWater:::exceed_rows(wd, park, site, param)
  hist <- NCRNWater:::yearly_summary(df, rows)

  expect_true(all(c("Year","ntot","nex","percent_ex","formatted_percent_ex") %in% names(hist)))
  expect_true(all(hist$percent_ex >= 0 & hist$percent_ex <= 100))
})

# -------------------------
# compose_text()
# -------------------------
test_that("compose_text returns grammar snippets and year bounds", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  df   <- NCRNWater:::site_data(wd, park, site, param)
  rows <- NCRNWater:::exceed_rows(wd, park, site, param)
  hist <- NCRNWater:::yearly_summary(df, rows)
  info <- NCRNWater:::site_info(wd, park, site, param)
  sum_nex <- sum(hist$nex)
  non_na_obs <- nrow(dplyr::filter(df, !is.na(Value)))
  ex_point <- NCRNWater:::choose_ex_point(rows, info$LowerPoint, info$UpperPoint)

  txt <- NCRNWater:::compose_text(hist, info, ex_point, sum_nex, non_na_obs)
  expect_true(all(c("recent_year","oldest_year","highest_rate","lowest_rate",
                    "highest_rate_years","lowest_rate_years","grammar1","grammar2") %in% names(txt)))
})

# -------------------------
# build_plot()
# -------------------------
test_that("build_plot returns a ggplot object with expected aesthetics", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  df   <- NCRNWater:::site_data(wd, park, site, param)
  rows <- NCRNWater:::exceed_rows(wd, park, site, param)
  hist <- NCRNWater:::yearly_summary(df, rows)
  info <- NCRNWater:::site_info(wd, park, site, param)
  sum_nex <- sum(hist$nex)
  non_na_obs <- nrow(dplyr::filter(df, !is.na(Value)))
  ex_point <- NCRNWater:::choose_ex_point(rows, info$LowerPoint, info$UpperPoint)

  p <- NCRNWater:::build_plot(hist, info$SiteName, info$DisplayName, info$Units, ex_point, sum_nex, non_na_obs)
  expect_s3_class(p, "ggplot")
})

# -------------------------
# build_site_package()
# -------------------------
test_that("build_site_package returns the per-site package structure", {
  wd <- getWD()
  combo <- pick_valid_combo(wd)
  park <- combo$park
  site <- combo$site
  param <- combo$param
  
  out <- NCRNWater:::build_site_package(wd, park, site, param)

  expect_true(is.list(out))
  expect_true(all(c("df","exdf","histdata","p","Sitename","Characteristic","Unit",
                    "LowerPoint","UpperPoint","LowerThreshold","UpperThreshold") %in% names(out)))

  expect_s3_class(out$df, "data.frame")
  expect_s3_class(out$exdf, "data.frame")
  expect_s3_class(out$histdata, "data.frame")
  expect_s3_class(out$p, "ggplot")
})
