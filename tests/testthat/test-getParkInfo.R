# tests/testthat/test-getParkInfo.R
#
# Example usage:
#   testthat::test_file("tests/testthat/test-getParkInfo.R")
#   devtools::test(filter = "getParkInfo")
# 
# # Sampled (fast)
# devtools::test()
# 
# # Exhaustive (pre-deploy)
# options(ncrnwater.test.exhaustive = TRUE)
# devtools::test(filter = "getParkInfo")
# 
library(testthat)
library(NCRNWater)

# Shared fixture (benign staging warning muted)
wd <- getWD()

# Respect run-mode knobs: exhaustive vs sampled parks
all_parks <- names(wd)
exhaustive <- getOption("ncrnwater.test.exhaustive", FALSE)

if (!exhaustive) {
  # Sample a subset of parks for fast local runs
  set.seed(getOption("ncrnwater.test.seed", 42))
  n_cases <- min(length(all_parks), getOption("ncrnwater.test.n_cases", 5L))
  parks <- utils::head(sample(all_parks), n_cases)
} else {
  parks <- all_parks
}

# ---- Compatibility helper: handle info names like "ShortName" vs "ParkShortName" ----
get_park_info_compat <- function(object, parkcode = NA, info) {
  # Try modern/short form first (e.g., "ShortName"), fall back to "ParkShortName"
  tryCatch(
    NCRNWater::getParkInfo(object, parkcode = parkcode, info = info),
    error = function(e) NCRNWater::getParkInfo(object, parkcode = parkcode, info = paste0("Park", info))
  )
}

# -------------------------
# Unfiltered dedup: ShortName & ParkCode
# -------------------------
test_that("[getParkInfo] unfiltered returns unique ParkShortName and ParkCode", {
  short_names <- get_park_info_compat(wd, info = "ShortName")
  expect_true(length(short_names) > 0L)
  expect_equal(length(short_names), length(unique(short_names)))
  
  park_codes <- get_park_info_compat(wd, info = "Code")
  expect_true(length(park_codes) > 0L)
  expect_equal(length(park_codes), length(unique(park_codes)))
})

# -------------------------
# Filtered by park: scalar fields exist and are non-empty
# -------------------------
for (park in parks) {
  test_that(sprintf("[getParkInfo] filtered by park '%s' returns scalar names", park), {
    short <- get_park_info_compat(wd, parkcode = park, info = "ShortName")
    long  <- get_park_info_compat(wd, parkcode = park, info = "LongName")
    net   <- get_park_info_compat(wd, parkcode = park, info = "Network")
    
    # Scalars, non-empty strings
    expect_true(is.character(short) && length(short) == 1L && nzchar(short))
    expect_true(is.character(long)  && length(long)  == 1L && nzchar(long))
    expect_true(is.character(net)   && length(net)   == 1L && nzchar(net))
  })
}

# -------------------------
# Mixed input: list of Park + duplicates -> dedup by ParkCode
# -------------------------
for (park in parks) {
  test_that(sprintf("[getParkInfo] dedupes Park objects by ParkCode for mixed inputs [%s]", park), {
    park_obj <- wd[[park]]
    
    # Build a mixed list: entire wd list plus duplicates of a single Park
    mixed <- c(wd, list(park_obj, park_obj, park_obj))
    
    # Unfiltered: ParkCode dedup
    codes <- get_park_info_compat(mixed, info = "Code")
    expect_true(length(codes) > 0L)
    expect_equal(length(codes), length(unique(codes)))
    
    # Filtered: specific park returns a single value (deduped)
    short <- get_park_info_compat(mixed, parkcode = park, info = "ShortName")
    expect_true(is.character(short) && length(short) == 1L && nzchar(short))
  })
}

# -------------------------
# Non-existent park: returns NULL or empty
# -------------------------
test_that("[getParkInfo] non-existent park returns NULL or empty vector", {
  out <- get_park_info_compat(wd, parkcode = "__no_such_park__", info = "ShortName")
  expect_true(is.null(out) || (is.character(out) && length(out) == 0L))
})

# -------------------------
# Shape across all parks (ShortName vs LongName)
# -------------------------
test_that("[getParkInfo] ShortName/LongName length matches number of unique parks", {
  short <- get_park_info_compat(wd, info = "ShortName")
  long  <- get_park_info_compat(wd, info = "LongName")
  
  # Number of unique ParkCodes drives expected lengths
  codes <- get_park_info_compat(wd, info = "Code")
  n_unique <- length(unique(codes))
  
  expect_equal(length(short), n_unique)
  expect_equal(length(long),  n_unique)
})
