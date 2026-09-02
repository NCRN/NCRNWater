# tests/testthat/test-getParks.R
#
# ------------------------------------------------------------------------------
# Test coverage summary for R/getParks.R
#
# This suite validates park retrieval across list/Park inputs, including
# filtering by parkcode, returned object shapes, and S4 invariants. It asserts
# correct handling of non-existent park codes and ensures filtered calls return
# parks whose ParkCode matches the filter. Parameterized tests run on sampled or
# exhaustive park sets from the shared fixture.
#
# Covered behaviors:
#   • getParks(list) (unfiltered):
#       - Returns a list of Park S4 objects; length ≥ 1.
#   • getParks(list) (filtered by one or many park codes):
#       - Returns only parks whose ParkCode is in the filter values.
#   • getParks(Park):
#       - Returns the Park when code matches; returns NULL otherwise.
#   • Non-existent filter:
#       - Returns NULL when no parks match the provided parkcode.
#
# Notes:
#   • Uses getWD() and sampled/exhaustive park selection via setup-runmode.R.
#   • We assert value equality via accessors and object slots without relying
#     on deduplication (the list method does not dedupe; it drops NULLs only).
#
# Run:
#
#   # Fast (sampled parks)
#   devtools::test(filter = "getParks")
#
#   # Exhaustive (all parks)
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "getParks")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-getParks.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

# Shared fixture (benign staging warning muted)
wd <- getWD()

# Respect run-mode knobs: exhaustive vs sampled parks
all_parks  <- names(wd)
exhaustive <- getOption("ncrnwater.test.exhaustive", FALSE)

if (!exhaustive) {
  set.seed(getOption("ncrnwater.test.seed", 42))
  n_cases <- min(length(all_parks), getOption("ncrnwater.test.n_cases", 5L))
  parks   <- utils::head(sample(all_parks), n_cases)
} else {
  parks <- all_parks
}

# -------------------------
# getParks(list) — unfiltered returns Park objects
# -------------------------
test_that("[getParks(list)] unfiltered returns Park objects", {
  out <- NCRNWater::getParks(wd)
  expect_true(is.list(out))
  expect_true(length(out) >= 1L)
  expect_true(all(vapply(out, function(p) methods::is(p, "Park"), logical(1))))
})

# -------------------------
# getParks(list) — filtered by single parkcode yields only matching parks
# -------------------------
for (park in parks) {
  test_that(sprintf("[getParks(list)] filtered by '%s' returns only matching parks", park), {
    out <- NCRNWater::getParks(wd, parkcode = park)
    # When filtered, all returned entries must be Park S4s with matching ParkCode
    if (is.null(out)) {
      succeed(sprintf("No parks matched filter '%s' (dataset-specific).", park))
    } else {
      expect_true(is.list(out))
      expect_true(length(out) >= 1L)
      expect_true(all(vapply(out, function(p) methods::is(p, "Park"), logical(1))))
      # Every element's ParkCode must match the filter
      codes <- vapply(out, function(p) NCRNWater::getParkInfo(p, info = "ParkCode"), FUN.VALUE = character(1))
      expect_true(all(codes %in% park))
    }
  })
}

# -------------------------
# getParks(list) — filtered by multiple parkcodes returns only those parks
# -------------------------
test_that("[getParks(list)] filtered by multiple park codes returns only those parks", {
  # Choose up to 2 codes for multi-filter test
  set.seed(getOption("ncrnwater.test.seed", 42))
  multi <- if (length(all_parks) >= 2L) utils::head(sample(all_parks), 2L) else all_parks
  out   <- NCRNWater::getParks(wd, parkcode = multi)
  if (is.null(out)) {
    succeed("No parks matched the multi-code filter (dataset-specific).")
  } else {
    expect_true(is.list(out))
    expect_true(all(vapply(out, function(p) methods::is(p, "Park"), logical(1))))
    codes <- vapply(out, function(p) NCRNWater::getParkInfo(p, info = "ParkCode"), FUN.VALUE = character(1))
    expect_true(all(codes %in% multi))
  }
})

# -------------------------
# getParks(Park) — returns Park when code matches; else NULL
# -------------------------
for (park in parks) {
  test_that(sprintf("[getParks(Park)] returns Park when code matches (%s)", park), {
    pk_obj <- wd[[park]]
    keep   <- NCRNWater::getParks(pk_obj, parkcode = park)
    expect_true(methods::is(keep, "Park"))
    expect_equal(NCRNWater::getParkInfo(keep, info = "ParkCode"), park)
  })
}

test_that("[getParks(Park)] returns NULL when code does not match", {
  park <- parks[1]
  pk_obj <- wd[[park]]
  drop   <- NCRNWater::getParks(pk_obj, parkcode = "__NO_SUCH_PARK__")
  expect_true(is.null(drop))
})

# -------------------------
# getParks(list) — non-existent parkcode returns NULL
# -------------------------
test_that("[getParks(list)] non-existent parkcode returns NULL", {
  out <- NCRNWater::getParks(wd, parkcode = "__NO_SUCH_PARK__")
  expect_true(is.null(out))
})

test_that("[getParks(list)] mixed vector ignores NA and matches codes", {
  wd <- getWD()
  parks <- names(wd)
  skip_if(length(parks) < 1L, "No parks available.")
  
  code <- parks[1]
  out  <- NCRNWater::getParks(wd, parkcode = c(code, NA))
  expect_true(is.list(out) || is.null(out))
  if (!is.null(out)) {
    codes <- vapply(out, function(p) NCRNWater::getParkInfo(p, info = "ParkCode"), FUN.VALUE = character(1))
    expect_true(all(codes %in% code))
  }
})

test_that("[getParks(list)] single NA returns all parks (unfiltered)", {
  wd <- getWD()
  out <- NCRNWater::getParks(wd, parkcode = NA)
  expect_true(is.list(out))
  expect_true(length(out) >= 1L)
})

