# tests/testthat/test-exceedances-helpers.R
#
# Run just this file:
#   testthat::test_file("tests/testthat/test-exceedances-helpers.R")
# Or via devtools filter:
#   devtools::test(filter = "exceedances")

library(testthat)
library(NCRNWater)

# Use shared fixture; suppress benign filterActive warnings during staging
getWD <- function() {
  suppressWarnings(get_waterdata_fixture())
}

# Helper: find a valid (park, site, param) that produces non-empty data
pick_valid_combo <- function(wd) {
  # Iterate parks -> sites -> characteristics until we find a data.frame with Date & Value
  parks <- names(wd)
  for (pk in parks) {
    sites <- names(wd[[pk]]@Sites)
    for (st in sites) {
      # Pull characteristics available at this site
      chars <- names(wd[[pk]]@Sites[[st]]@Characteristics)
      for (ch in chars) {
        df <- NCRNWater::getWData(wd, parkcode = pk, sitecode = st, charname = ch, output = "data.frame")
        if (is.data.frame(df) && all(c("Date","Value") %in% names(df)) && nrow(df) > 0) {
          return(list(park = pk, site = st, param = ch))
        }
      }
    }
  }
  stop("Could not find a valid (park, site, param) with non-empty Date/Value data in the fixture.")
}

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

# # -------------------------
# # exceed_rows()
# # -------------------------
# test_that("exceed_rows returns rows-mode schema", {
#   wd <- get_waterdata_fixture()
#   rows <- NCRNWater:::exceed_rows(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   expect_s3_class(rows, "data.frame")
#   expect_true(all(c("Date","Value","LowerPoint","UpperPoint",
#                     "LowerPointCondition","UpperPointCondition",
#                     "Exceed_Lower","Exceed_Upper","Exceed_Type") %in% names(rows)))
# })
# 
# # -------------------------
# # choose_ex_point()
# # -------------------------
# test_that("choose_ex_point picks lower/upper point based on observed exceedances", {
#   wd <- get_waterdata_fixture()
#   rows <- NCRNWater:::exceed_rows(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   info <- NCRNWater:::site_info(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   
#   ep <- NCRNWater:::choose_ex_point(rows, info$LowerPoint, info$UpperPoint)
#   # choose_ex_point returns numeric threshold or NA when both exceedances occur
#   expect_true(is.numeric(ep) || is.na(ep))
# })
# 
# # -------------------------
# # fill_threshold_descriptions()
# # -------------------------
# test_that("fill_threshold_descriptions emits fallback text when descriptions are missing", {
#   wd <- get_waterdata_fixture()
#   info <- NCRNWater:::site_info(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   out  <- NCRNWater:::fill_threshold_descriptions(info)
#   expect_true(is.list(out))
#   expect_true(all(c("LowerDescription","UpperDescription") %in% names(out)))
# })
# 
# # -------------------------
# # add_difference()
# # -------------------------
# test_that("add_difference computes delta from exceeded threshold", {
#   wd <- get_waterdata_fixture()
#   rows <- NCRNWater:::exceed_rows(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   rows2 <- NCRNWater:::add_difference(rows)
#   expect_true("Difference" %in% names(rows2))
#   expect_s3_class(rows2$Difference, "numeric")
# })
# 
# # -------------------------
# # yearly_summary()
# # -------------------------
# test_that("yearly_summary computes ntot/nex and formatted percent", {
#   wd <- get_waterdata_fixture()
#   df   <- NCRNWater:::site_data(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   rows <- NCRNWater:::exceed_rows(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   hist <- NCRNWater:::yearly_summary(df, rows)
#   
#   expect_true(all(c("Year","ntot","nex","percent_ex","formatted_percent_ex") %in% names(hist)))
#   expect_true(all(hist$percent_ex >= 0 & hist$percent_ex <= 100))
# })
# 
# # -------------------------
# # compose_text()
# # -------------------------
# test_that("compose_text returns grammar snippets and year bounds", {
#   wd <- get_waterdata_fixture()
#   df   <- NCRNWater:::site_data(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   rows <- NCRNWater:::exceed_rows(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   hist <- NCRNWater:::yearly_summary(df, rows)
#   info <- NCRNWater:::site_info(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   sum_nex <- sum(hist$nex)
#   non_na_obs <- nrow(dplyr::filter(df, !is.na(Value)))
#   ex_point <- NCRNWater:::choose_ex_point(rows, info$LowerPoint, info$UpperPoint)
#   
#   txt <- NCRNWater:::compose_text(hist, info, ex_point, sum_nex, non_na_obs)
#   expect_true(all(c("recent_year","oldest_year","highest_rate","lowest_rate",
#                     "highest_rate_years","lowest_rate_years","grammar1","grammar2") %in% names(txt)))
# })
# 
# # -------------------------
# # build_plot()
# # -------------------------
# test_that("build_plot returns a ggplot object with expected aesthetics", {
#   wd <- get_waterdata_fixture()
#   df   <- NCRNWater:::site_data(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   rows <- NCRNWater:::exceed_rows(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   hist <- NCRNWater:::yearly_summary(df, rows)
#   info <- NCRNWater:::site_info(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   sum_nex <- sum(hist$nex)
#   non_na_obs <- nrow(dplyr::filter(df, !is.na(Value)))
#   ex_point <- NCRNWater:::choose_ex_point(rows, info$LowerPoint, info$UpperPoint)
#   
#   p <- NCRNWater:::build_plot(hist, info$SiteName, info$DisplayName, info$Units, ex_point, sum_nex, non_na_obs)
#   expect_s3_class(p, "ggplot")
# })
# 
# # -------------------------
# # build_site_package()
# # -------------------------
# test_that("build_site_package returns the per-site package structure", {
#   wd <- get_waterdata_fixture()
#   out <- NCRNWater:::build_site_package(wd, "NACE", "NCRN_NACE_FTDU", "ANC")
#   
#   expect_true(is.list(out))
#   expect_true(all(c("df","exdf","histdata","p","Sitename","Characteristic","Unit",
#                     "LowerPoint","UpperPoint","LowerThreshold","UpperThreshold") %in% names(out)))
#   
#   expect_s3_class(out$df, "data.frame")
#   expect_s3_class(out$exdf, "data.frame")
#   expect_s3_class(out$histdata, "data.frame")
#   expect_s3_class(out$p, "ggplot")
# })