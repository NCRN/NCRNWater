
test_that("getChars(list/Park) dedupes Characteristics by identity", {
  skip_if_not_installed("NCRNWater")

  Network <- "NCRN"
  WaterData<-NCRNWater::importNCRNWater(file.path("Data", Network), Data='wqp_activeonly.csv', MetaData = 'wqp_ncrnwater_metadata_activeonly.csv', wqx=T)

  park <- "NACE"
  site <- "NCRN_NACE_OXRU"
  param <- "pH"

  chars <- NCRNWater::getChars(WaterData, parkcode = park, sitecode = site, charname = param)
  flat  <- unlist(chars, recursive = FALSE, use.names = FALSE)
  expect_length(flat, 1L)
  expect_identical(flat[[1]]@CharacteristicName, "pH")
})

test_that("getCharInfo returns scalar LowerDescription per Site/Char", {
  skip_if_not_installed("NCRNWater")

  Network <- "NCRN"
  WaterData<-NCRNWater::importNCRNWater(file.path("Data", Network), Data='wqp_activeonly.csv', MetaData = 'wqp_ncrnwater_metadata_activeonly.csv', wqx=T)

  # Problem case (now fixed)
  park <- "NACE"
  site <- "NCRN_NACE_OXRU"
  param <- "pH"
  x <- NCRNWater::getCharInfo(WaterData, sitecode = site, parkcode = park,
                              charname = param, info = "LowerDescription")
  expect_length(x, 1L)

  # Previously working case
  park <- "GWMP"
  site <- "NCRN_GWMP_MICR"
  y <- NCRNWater::getCharInfo(WaterData, sitecode = site, parkcode = park,
                              charname = param, info = "LowerDescription")
  expect_length(y, 1L)
})
