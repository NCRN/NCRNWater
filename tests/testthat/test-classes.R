# tests/testthat/test-classes.R
#
# ------------------------------------------------------------------------------
# Test coverage summary for class definitions:
#   NCRNWaterObj, Park, Site, Characteristic
#
# This suite validates basic construction, slot structure, and accessor
# consistency for core S4 classes. Behavioral testing of these classes is
# covered extensively by higher-level modules (getChars, getCharInfo, getSites,
# getParkInfo, exceed, diagnoseWaterData). This file establishes explicit
# invariants that class definitions must uphold.
#
# Run:
#   devtools::test(filter = "classes")
#   testthat::test_file("tests/testthat/test-classes.R")
#
# ------------------------------------------------------------------------------

library(testthat)
library(NCRNWater)

test_that("Characteristic class constructs with expected slots", {
  ch <- methods::new("Characteristic",
                     CharacteristicName    = "ANC",
                     DisplayName           = "ANC",
                     Units                 = "mg/L",
                     Category              = "Chem",
                     CategoryDisplay       = "Chem",
                     LowerPoint            = 5,
                     UpperPoint            = 8,
                     LowerPointCondition   = "le",
                     UpperPointCondition   = "ge",
                     Data = data.frame(Date = as.Date("2020-01-01"), Value = 6)
  )
  
  expect_s4_class(ch, "Characteristic")
  
  # Basic slot type invariants
  expect_true(is.character(ch@CharacteristicName))
  expect_true(is.character(ch@Units))
  expect_true(is.numeric(ch@LowerPoint) || is.na(ch@LowerPoint))
  expect_s3_class(ch@Data, "data.frame")
  
  # Accessor consistency (use named info=)
  cn <- NCRNWater::getCharInfo(ch, info = "CharName")
  un <- NCRNWater::getCharInfo(ch, info = "Units")
  
  expect_true(is.character(cn) && length(cn) == 1L && cn == ch@CharacteristicName)
  expect_true(is.character(un) && length(un) == 1L && un == ch@Units)
})

test_that("Site class constructs with expected slots", {
  ch <- methods::new("Characteristic",
                     CharacteristicName    = "ANC",
                     DisplayName           = "ANC",
                     Units                 = "mg/L",
                     Category              = "Chem",
                     CategoryDisplay       = "Chem",
                     LowerPoint            = 5,
                     UpperPoint            = 8,
                     LowerPointCondition   = "le",
                     UpperPointCondition   = "ge",
                     Data = data.frame(Date = as.Date("2020-01-01"), Value = 6))
  
  st <- methods::new("Site",
                     SiteCode       = "TEST_SITE",
                     SiteName       = "Test Site",
                     Lat            = 39.0,
                     Long           = -77.0,
                     Type           = "River",
                     Characteristics = list(ANC = ch))
  
  expect_s4_class(st, "Site")
  expect_true(is.character(st@SiteCode))
  expect_true(is.list(st@Characteristics))
  
  # Accessor consistency (use named info=)
  sc <- NCRNWater::getSiteInfo(st, info = "SiteCode")
  expect_true(is.character(sc) && length(sc) == 1L && sc == "TEST_SITE")
})

test_that("Park class constructs with expected slots", {
  ch <- methods::new("Characteristic",
                     CharacteristicName    = "ANC",
                     DisplayName           = "ANC",
                     Units                 = "mg/L",
                     Category              = "Chem",
                     CategoryDisplay       = "Chem",
                     LowerPoint            = 5,
                     UpperPoint            = 8,
                     LowerPointCondition   = "le",
                     UpperPointCondition   = "ge",
                     Data = data.frame(Date = as.Date("2020-01-01"), Value = 6))
  
  st <- methods::new("Site",
                     SiteCode       = "TEST_SITE",
                     SiteName       = "Test Site",
                     Lat            = 39.0,
                     Long           = -77.0,
                     Type           = "River",
                     Characteristics = list(ANC = ch))
  
  pk <- methods::new("Park",
                     ParkCode = "TEST",
                     ShortName = "Short",
                     LongName  = "Long",
                     Network   = "NCRN",
                     Sites     = list(TEST_SITE = st))
  
  expect_s4_class(pk, "Park")
  expect_true(is.character(pk@ParkCode))
  
  # Accessor consistency (use named info=)
  pc <- NCRNWater::getParkInfo(pk, info = "ParkCode")
  expect_true(is.character(pc) && length(pc) >= 1L)
  # Park method may replicate per characteristic count; ensure at least one equals ParkCode
  expect_true(any(pc == "TEST"))
})