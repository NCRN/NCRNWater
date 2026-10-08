# tests/testthat/test-shiny-exceed-service.R
#
# ------------------------------------------------------------------------------
# Test module: test-shiny-exceed-service.R
#
# Test coverage summary for R/shinyExceedService.R (build_exceed_site_package)
#
# This suite validates the Shiny exceedance service that powers the Exceedances
# tab. It asserts the API returns a stable shape, uses metadata comparators
# (lt/le/gt/ge) in narrative text, conditionally includes threshold description
# columns, computes per-row Difference correctly, and handles plot toggling.
#
# Covered behaviors:
#   • Structure & types: df/exdf/desc_exdf/histdata/p/html_extext/flags.
#   • Comparator-aware narrative: BOTH case (lt/gt) includes correct symbols;
#     lower-only and upper-only branches handled by other tests.
#   • Threshold description columns: include only those present.
#   • Difference calculation: row-wise, using Exceed_Upper / Exceed_Lower.
#   • Plot: ggplot object present when show_plot = TRUE; empty ggplot otherwise.
#
# Notes:
#   • Uses helper-fixtures.R (getWD(), sample_n_valid_combos(), etc.).
#   • Calls internal function via NCRNWater:::build_exceed_site_package.
#   • The pH Big Hunting Creek test is guarded and will skip if fixtures
#     lack the expected site/characteristic; adjust as needed for your CI.
#
# Run:
#   devtools::test(filter = "shiny-exceed-service")
#   testthat::test_file("tests/testthat/test-shiny-exceed-service.R")
#
# ------------------------------------------------------------------------------

library(testthat)
library(NCRNWater)

# Ensure test helpers (getWD(), enumerate_combos(), etc.) are available
testthat::source_test_helpers()

# Utility: build a package safely
.build_pkg <- function(wd, park, site, char, show_plot = TRUE) {
  NCRNWater:::build_exceed_site_package(wd, park, site, char, show_plot = show_plot)
}

test_that("[build_exceed_site_package] returns stable shape & types for a typical exceedance case", {
  wd <- getWD()
  # Pick a combo with thresholds and (likely) data; if exceedances are zero that's OK—shape check only
  combos <- sample_n_valid_combos(wd, n_cases = 1L)
  c <- combos[[1]]
  
  pkg <- .build_pkg(wd, c$park, c$site, c$param, show_plot = TRUE)
  
  expect_true(is.list(pkg))
  expect_true(all(c(
    "df","exdf","desc_exdf","histdata","Sitename","Characteristic","Unit",
    "p","html_extext","has_exceedances"
  ) %in% names(pkg)))
  
  expect_s3_class(pkg$df, "data.frame")
  expect_s3_class(pkg$exdf, "data.frame")
  expect_s3_class(pkg$desc_exdf, "data.frame")
  expect_s3_class(pkg$histdata, "data.frame")
  expect_s3_class(pkg$p, "ggplot")
  expect_type(pkg$html_extext, "character")
  expect_true(is.logical(pkg$has_exceedances))
})

test_that("[desc_exdf] Difference matches row-wise exceed-side (Upper vs Lower)", {
  wd <- getWD()
  # Find a combo that actually has at least one exceed row; fall back gracefully if none found
  combos <- sample_n_valid_combos(wd, n_cases = 5L)
  pkg <- NULL
  for (c in combos) {
    tmp <- .build_pkg(wd, c$park, c$site, c$param, show_plot = FALSE)
    if (nrow(tmp$exdf) > 0) { pkg <- tmp; break }
  }
  skip_if(is.null(pkg), "No exceedance rows found in sampled fixtures.")
  
  ex <- pkg$exdf
  # Expected per-row difference, aligned to exceed flags and threshold presence
  expected <- dplyr::case_when(
    !is.na(ex$UpperPoint) &  is.na(ex$LowerPoint)               ~ ex$Value - ex$UpperPoint,
    is.na(ex$UpperPoint) & !is.na(ex$LowerPoint)               ~ ex$Value - ex$LowerPoint,
    !is.na(ex$UpperPoint) & !is.na(ex$LowerPoint) & ex$Exceed_Upper ~ ex$Value - ex$UpperPoint,
    !is.na(ex$UpperPoint) & !is.na(ex$LowerPoint) & ex$Exceed_Lower ~ ex$Value - ex$LowerPoint,
    TRUE ~ as.numeric(NA)
  )
  expected <- round(expected, 4)
  
  # Desc table is ordered by SampleDate desc; align expected accordingly
  ex_ordered <- ex[order(ex$Date, decreasing = TRUE), ]
  expected_ordered <- expected[order(ex$Date, decreasing = TRUE)]
  
  # Subset desc_exdf to the same number of rows (it should correspond to exdf exceed rows)
  desc <- pkg$desc_exdf
  expect_equal(nrow(desc), nrow(ex_ordered))
  expect_equal(desc$Difference, expected_ordered)
})

test_that("[html_extext] BOTH thresholds (lt/gt) use metadata comparators in narrative (pH at BGHC)", {
  # Guards
  wd <- tryCatch(getWD(), error = function(e) NULL)
  skip_if(is.null(wd), "WD fixture unavailable.")
  skip_if(!("CATO" %in% names(wd)), "Park CATO missing in fixture.")
  cato_sites <- names(wd[["CATO"]]@Sites)
  skip_if(!("NCRN_CATO_BGHC" %in% cato_sites), "Site NCRN_CATO_BGHC missing in fixture.")
  
  # Build for pH; skip if not present
  pkg <- tryCatch(
    NCRNWater:::build_exceed_site_package(wd, park = "CATO", site = "NCRN_CATO_BGHC", char = "pH", show_plot = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(pkg), "pH characteristic not available at BGHC in fixture.")
  
  txt <- pkg$html_extext
  
  # Parse the HTML safely
  doc <- xml2::read_html(paste0("<div>", txt, "</div>"))  # wrap to ensure a single root
  
  # Find ARIA spans for comparators
  lt_nodes <- xml2::xml_find_all(doc, ".//span[@aria-label='less than']")
  gt_nodes <- xml2::xml_find_all(doc, ".//span[@aria-label='greater than']")
  
  # Expect both spans present
  expect_true(length(lt_nodes) >= 1, info = "Missing <span aria-label='less than'>")
  expect_true(length(gt_nodes) >= 1, info = "Missing <span aria-label='greater than'>")
  
  # xml2 decodes entities; node text should be the actual symbol
  lt_texts <- xml2::xml_text(lt_nodes)
  gt_texts <- xml2::xml_text(gt_nodes)
  
  # For a BOTH case with lt/gt metadata, symbols must be "<" and ">"
  expect_true(any(lt_texts == "<"), info = paste("Expected '<' inside less-than span; saw:", paste(lt_texts, collapse = ", ")))
  expect_true(any(gt_texts == ">"), info = paste("Expected '>' inside greater-than span; saw:", paste(gt_texts, collapse = ", ")))
  
  # Basic content checks (points and leading sentence)
  full_text <- xml2::xml_text(doc)
  expect_match(full_text, "\\bNo\\s+pH\\s+measurements\\s+were\\b", perl = TRUE)
  expect_match(full_text, "\\b6\\b")
  expect_match(full_text, "\\b9\\b")
})

test_that("[build_exceed_site_package] plot toggles with show_plot flag", {
  wd <- getWD()
  c <- sample_n_valid_combos(wd, n_cases = 1L)[[1]]
  
  pkg_true  <- .build_pkg(wd, c$park, c$site, c$param, show_plot = TRUE)
  pkg_false <- .build_pkg(wd, c$park, c$site, c$param, show_plot = FALSE)
  
  expect_s3_class(pkg_true$p,  "ggplot")
  expect_s3_class(pkg_false$p, "ggplot")  # empty ggplot still a ggplot object
  # Optional: detect empty plot by layers count = 0
  expect_true(length(pkg_false$p$layers) == 0)
})

test_that("[build_exceed_site_package] has_exceedances flag is consistent with exdf rows", {
  wd <- getWD()
  c <- sample_n_valid_combos(wd, n_cases = 3L)[[1]]
  
  pkg <- .build_pkg(wd, c$park, c$site, c$param, show_plot = FALSE)
  expect_identical(pkg$has_exceedances, nrow(pkg$exdf) > 0)
})
