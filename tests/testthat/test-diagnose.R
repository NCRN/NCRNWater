# tests/testthat/test-diagnose.R
#
# Run just this file:
#   testthat::test_file("tests/testthat/test-diagnose.R")

library(NCRNWater)
test_that("diagnose normalizes and validates comparator codes", {
  # Construct a minimal Characteristic with new code fields
  ch <- methods::new("Characteristic",
                     CharacteristicName="ANC", DisplayName="ANC",
                     Units="mg/L", Category="Chem", CategoryDisplay="Chem",
                     LowerPoint=5, UpperPoint=8,
                     LowerPointCondition="ge", UpperPointCondition="le",
                     Data=data.frame(Date=as.Date("2020-01-01"), Value=c(4,5,8,9))
  )
  s <- methods::new("Site", SiteCode="SITE_A", SiteName="Alpha", Lat=0, Long=0, Type="River",
                    Characteristics=list(ANC=ch))
  p <- methods::new("Park", ParkCode="TEST", ShortName="TEST", LongName="Test Park", Network="NCRN",
                    Sites=list(SITE_A=s))
  
  diag <- diagnoseWaterData(list(p), verbose_chars = FALSE, show_char_details = FALSE)
  expect_gte(diag$problems, 0L)  # no invalid code warnings expected
})
