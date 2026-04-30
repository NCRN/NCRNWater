
library(testthat)
test_that("getSites(list) dedupes identical Site objects by SiteCode", {
    skip_if_not_installed("NCRNWater")
    
    Network <- "NCRN"
    WaterData<-importNCRNWater(file.path("Data", Network), Data='wqp_activeonly.csv', MetaData = 'wqp_ncrnwater_metadata_activeonly.csv', wqx=T)
    
    site <- "NCRN_NACE_OXRU"
    park <- "NACE"
    
    sites <- NCRNWater::getSites(WaterData, parkcode = park, sitecode = site)
    expect_true(length(sites) == 1L)
    
    nm <- NCRNWater::getSiteInfo(WaterData, parkcode = park, sitecode = site, info = "SiteName")
    expect_identical(nm, "Oxon Run")
    expect_length(nm, 1L)
})

test_that("getSiteInfo returns unique names per site under a park", {
    skip_if_not_installed("NCRNWater")
    
    Network <- "NCRN"
    WaterData<-importNCRNWater(file.path("Data", Network), Data='wqp_activeonly.csv', MetaData = 'wqp_ncrnwater_metadata_activeonly.csv', wqx=T)
    park <- "NACE"
    
    sites <- NCRNWater::getSites(WaterData, parkcode = park)
    sc <- vapply(sites, function(s) s@SiteCode, character(1))
    
    names <- NCRNWater::getSiteInfo(WaterData, parkcode = park, info = "SiteName")
    expect_true(length(names) == length(unique(sc)))
})
