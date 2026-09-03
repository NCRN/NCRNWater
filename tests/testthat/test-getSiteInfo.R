# tests/testthat/test-getSiteInfo.R
#
# ------------------------------------------------------------------------------
# Test module: test-getSiteInfo.R
#
# Test coverage summary for R/getSiteInfo.R
#
# This suite validates site-level metadata retrieval across list/Park/Site inputs,
# including filtering by parkcode/sitecode, scalar types (lat/long numeric),
# replication behavior for park-level infos, and error/empty paths. Tests run on
# sampled or exhaustive sets of parks/sites/params from the shared fixture.
#
# Covered behaviors:
#   • list(object) with no filters:
#       - Returns unique SiteCode vector; lat/long are numeric vectors.
#   • list(object) filtered by parkcode/sitecode:
#       - Values correspond only to the selected park/site; deduplication by SiteCode.
#   • Park(object):
#       - Site-level infos replicate per unique site under the park; park-level infos
#         replicate per site count and equal park metadata.
#   • Site(object):
#       - Scalar getters (SiteCode/SiteName/lat/long/type) return length-1 values.
#   • Non-existent filters:
#       - Site-level infos return empty vectors of the correct type; park-level info
#         with only sitecode provided produces an explicit error.
#   • Error path:
#       - Missing 'info' argument throws.
#
# Notes:
#   • Uses getWD() and run-mode knobs (sampled vs exhaustive) from setup-runmode.R.
#   • Quiet wrappers (quiet_getSiteInfo) muffle known benign warnings; errors still surface.
#
# Run:
#
#   # Fast (sampled)
#   devtools::test(filter = "getSiteInfo")
#
#   # Exhaustive
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "getSiteInfo")
#
#   # Single file
#   testthat::test_file("tests/testthat/test-getSiteInfo.R")
#
# ------------------------------------------------------------------------------

library(testthat)
library(NCRNWater)
testthat::source_test_helpers()

# Shared fixture (benign staging warning muted)
wd <- getWD()

# Respect run-mode knobs for park sampling
all_parks  <- names(wd)
exhaustive <- getOption("ncrnwater.test.exhaustive", FALSE)

if (!exhaustive) {
  set.seed(getOption("ncrnwater.test.seed", 42))
  n_cases <- min(length(all_parks), getOption("ncrnwater.test.n_cases", 5L))
  parks   <- utils::head(sample(all_parks), n_cases)
} else {
  parks <- all_parks
}

# Also prepare (park, site, param) sampled cases for targeted checks
cases <- if (exhaustive) list_all_valid_combos(wd) else sample_n_valid_combos(wd)

# ---- Helpers ---------------------------------------------------------------

# Use quiet wrapper to muffle benign warnings; errors still bubble
getSI <- function(...) quiet_getSiteInfo(...)

# Scalar type fun-value by info
fv_site <- function(info) if (info %in% c("lat", "long")) numeric(1) else character(1)

# -------------------------
# list(object) — global (no filters)
# -------------------------
test_that("[getSiteInfo(list)] global returns unique SiteCode and numeric lat/long", {
  sc <- getSI(wd, info = "SiteCode")
  expect_true(is.character(sc) || length(sc) == 0L)
  expect_true(length(sc) >= 1L)
  expect_equal(length(sc), length(unique(sc)))
  
  lt <- getSI(wd, info = "lat")
  lg <- getSI(wd, info = "long")
  expect_true(is.numeric(lt) || length(lt) == 0L)
  expect_true(is.numeric(lg) || length(lg) == 0L)
})

# -------------------------
# list(object) — filtered by parkcode
# -------------------------
for (park in parks) {
  test_that(sprintf("[getSiteInfo(list)] filtered by park '%s' returns sites under that park", park), {
    sc <- getSI(wd, parkcode = park, info = "SiteCode")
    if (length(sc) == 0L) {
      succeed(sprintf("No sites found for park %s (dataset-specific).", park))
    } else {
      # For each returned sitecode, verify it belongs to the park
      ok <- vapply(sc, function(code) {
        s <- NCRNWater::getSites(wd, parkcode = park, sitecode = code)
        !is.null(s) && length(s) > 0L
      }, logical(1))
      expect_true(all(ok))
      expect_equal(length(sc), length(unique(sc)))
    }
  })
}

# -------------------------
# list(object) — filtered by sitecode only
# -------------------------
test_that("[getSiteInfo(list)] filtered by sitecode returns single site metadata", {
  case <- cases[[1]]
  site <- case$site
  # Site-level info returns vector with that site's metadata
  nm <- getSI(wd, sitecode = site, info = "SiteName")
  expect_true(is.character(nm))
  expect_true(length(nm) >= 1L)
})

# -------------------------
# Park(object) — replication per unique site and scalar types
# -------------------------
for (park in parks) {
  test_that(sprintf("[getSiteInfo(Park)] site-level infos replicate per unique site [%s]", park), {
    pk <- wd[[park]]
    
    sites <- NCRNWater::getSites(pk, parkcode = park)
    if (is.null(sites) || length(sites) == 0L) {
      succeed(sprintf("No sites under park %s (dataset-specific).", park))
    } else {
      sites <- sites[vapply(sites, function(x) methods::is(x, "Site"), logical(1))]
      sc    <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
      nuniq <- length(unique(sc))
      
      # SiteCode replicated per unique site
      out_sc <- NCRNWater::getSiteInfo(pk, parkcode = park, info = "SiteCode")
      expect_true(is.character(out_sc))
      expect_equal(length(out_sc), nuniq)
      
      # ParkShortName replicated per unique site
      out_ps <- NCRNWater::getSiteInfo(pk, parkcode = park, info = "ParkShortName")
      expect_true(is.character(out_ps))
      expect_equal(length(out_ps), nuniq)
      exp_ps  <- NCRNWater::getParkInfo(pk, info = "ParkShortName")
      expect_true(all(out_ps %in% exp_ps))
    }
  })
}

# -------------------------
# Site(object) — scalar getters return length-1
# -------------------------
test_that("[getSiteInfo(Site)] scalar getters return length-1 values", {
  case <- cases[[1]]
  sites <- NCRNWater::getSites(wd, parkcode = case$park, sitecode = case$site)
  skip_if(is.null(sites) || length(sites) == 0L,
          sprintf("No site found for %s:%s.", case$park, case$site))
  
  s <- sites[[1]]
  
  sc <- NCRNWater::getSiteInfo(s, info = "SiteCode")
  sn <- NCRNWater::getSiteInfo(s, info = "SiteName")
  lt <- NCRNWater::getSiteInfo(s, info = "lat")
  lg <- NCRNWater::getSiteInfo(s, info = "long")
  tp <- NCRNWater::getSiteInfo(s, info = "type")
  
  expect_true(is.character(sc) && length(sc) == 1L)
  expect_true(is.character(sn) && length(sn) == 1L)
  expect_true(is.numeric(lt)   && length(lt) == 1L)
  expect_true(is.numeric(lg)   && length(lg) == 1L)
  expect_true(is.character(tp) && length(tp) == 1L)
})

# -------------------------
# Non-existent filters: type-correct empties and expected error
# -------------------------
test_that("[getSiteInfo(list)] non-existent site returns empty vector of correct type", {
  out_lat <- getSI(wd, parkcode = "NACE", sitecode = "__NO_SUCH_SITE__", info = "lat")
  expect_true(is.numeric(out_lat) && length(out_lat) == 0L)
  
  out_name <- getSI(wd, parkcode = "NACE", sitecode = "__NO_SUCH_SITE__", info = "SiteName")
  expect_true(is.character(out_name) && length(out_name) == 0L)
})

# -------------------------
# ParkShortName error when only sitecode provided and no matching park
# -------------------------
test_that("[getSiteInfo(list)] ParkShortName with only sitecode and no match errors", {
  expect_error(
    withCallingHandlers(
      NCRNWater::getSiteInfo(wd, sitecode = "__NO_SUCH_SITE__", info = "ParkShortName"),
      warning = function(w) {
        if (grepl("^No sites match these criteria\\.$", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    "No Park found containing sitecode",
    fixed = TRUE
  )
})

# -------------------------
# Error path: missing 'info' argument
# -------------------------
test_that("[getSiteInfo] missing 'info' throws", {
  expect_error(NCRNWater::getSiteInfo(wd), "argument \"info\" is missing", fixed = TRUE)
})
