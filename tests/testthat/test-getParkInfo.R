test_that("getParkInfo(list) dedupes Park objects by ParkCode for unfiltered and filtered calls", {
    skip_if_not_installed("NCRNWater")
    
    Network <- "NCRN"
    WaterData <- importNCRNWater(file.path("Data", Network), Data = "wqp_activeonly.csv")
    
    # Unfiltered: expect NO duplicates
    short_names <- NCRNWater::getParkInfo(WaterData, info = "ParkShortName")
    expect_true(length(short_names) == length(unique(short_names)))
    expect_true(length(short_names) > 0L)
    
    # Also check ParkCode uniqueness
    park_codes <- NCRNWater::getParkInfo(WaterData, info = "ParkCode")
    expect_true(length(park_codes) == length(unique(park_codes)))
    
    # Filtered: specific park returns a single name (deduped)
    park <- "NACE"
    x <- NCRNWater::getParkInfo(WaterData, parkcode = park, info = "ParkShortName")
    expect_length(x, 1L)
    expect_match(x, "Nat. Cap. Parks - East")
    
    park <- "GWMP"
    y <- NCRNWater::getParkInfo(WaterData, parkcode = park, info = "ParkShortName")
    expect_length(y, 1L)
    expect_match(y, "GW Parkway")
})

