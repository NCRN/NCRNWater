# tests/testthat/test-getSites.R
#
# Run:
#   testthat::test_file("tests/testthat/test-getSites.R")
#   devtools::test(filter = "getSites")

library(testthat)
library(NCRNWater)

# Hydrate shared fixture (benign staging warning muted)
wd <- getWD()

# Select parameterized cases
cases <- sample_valid_combos(wd)  # configurable via options()

# ---- List method: dedup + filter by sitecode/type ---------------------------------

for (case in cases) {
  park <- case$park; site <- case$site
  
  test_that(sprintf("[list] returns Site objects for park %s; dedup by SiteCode", park), {
    sites <- NCRNWater::getSites(wd, parkcode = park)
    expect_true(is.list(sites))
    expect_true(length(sites) >= 1L)
    expect_true(all(vapply(sites, function(s) methods::is(s, "Site"), logical(1))))
    sc <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
    expect_equal(length(sc), length(unique(sc)))
  })
  
  test_that(sprintf("[list] sitecode filter yields one site [%s:%s]", park, site), {
    one <- NCRNWater::getSites(wd, parkcode = park, sitecode = site)
    expect_length(one, 1L)
    expect_identical(one[[1]]@SiteCode, site)
  })

  test_that(sprintf("[list] type filter returns only requested types [%s]", park), {
    types_vec <- NCRNWater::getSiteInfo(wd, parkcode = park, info = "type")
    types <- unique(na.omit(types_vec))
    skip_if(length(types) == 0L, sprintf("No non-NA 'type' values for park %s.", park))

    chosen_type <- types[1]
    filtered <- NCRNWater::getSites(wd, parkcode = park, type = chosen_type)
    expect_true(length(filtered) >= 1L)

    returned_types <- vapply(filtered, function(s) NCRNWater::getSiteInfo(s, info = "type"), FUN.VALUE = character(1))
    expect_true(all(returned_types %in% chosen_type))
  })

  test_that(sprintf("[list] flattens mixed input and dedupes [%s]", park), {
    park_obj <- wd[[park]]
    park_sites <- NCRNWater::getSites(wd, parkcode = park)
    skip_if(length(park_sites) < 2L, sprintf("Need >=2 sites to test flatten/dedupe for %s.", park))

    mixed <- list(park_obj, park_sites[[1]], park_sites[[2]])
    out <- NCRNWater::getSites(mixed, parkcode = park)
    expect_true(is.list(out))
    expect_true(length(out) >= 2L)
    expect_true(all(vapply(out, function(s) methods::is(s, "Site"), logical(1))))
    sc <- vapply(out, function(s) s@SiteCode, FUN.VALUE = character(1))
    expect_equal(length(sc), length(unique(sc)))
  })
}

test_that("[list] no matches returns NULL and warns", {
  park <- cases[[1]]$park
  expect_warning(
    out <- NCRNWater::getSites(wd, parkcode = park, sitecode = "__no_such_site__"),
    "No sites match these criteria\\."
  )
  expect_null(out)
})

# ---- Park method: returns list of sites; sitecode filter yields one --------------

for (case in cases) {
  park <- case$park

  test_that(sprintf("[Park] getSites(Park) returns Site list for %s; filter yields one", park), {
    park_obj <- wd[[park]]
    sites <- NCRNWater::getSites(park_obj, parkcode = park)
    expect_true(is.list(sites))
    expect_true(length(sites) >= 1L)
    expect_true(all(vapply(sites, function(s) methods::is(s, "Site"), logical(1))))
    sc <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
    one <- NCRNWater::getSites(park_obj, parkcode = park, sitecode = sc[1])
    expect_length(one, 1L)
    expect_equal(unname(one[[1]]@SiteCode), unname(sc[1]))
  })
}

# ---- Site method: obeys filters (sitecode/type) ----------------------------------

for (case in cases) {
  park <- case$park
  test_that(sprintf("[Site] filter by sitecode/type keeps or drops [%s]", park), {
    sites <- NCRNWater::getSites(wd, parkcode = park)
    skip_if(length(sites) == 0L, sprintf("No sites for park %s.", park))
    s_obj <- sites[[1]]

    # Matching sitecode -> keeps
    keep <- NCRNWater::getSites(s_obj, sitecode = s_obj@SiteCode)
    expect_true(methods::is(keep, "Site"))
    expect_identical(keep@SiteCode, s_obj@SiteCode)

    # Non-matching sitecode -> drops
    drop <- NCRNWater::getSites(s_obj, sitecode = "__nope__")
    expect_null(drop)

    # Type filters
    s_type <- NCRNWater::getSiteInfo(s_obj, info = "type")
    if (!is.na(s_type) && nzchar(s_type)) {
      keep2 <- NCRNWater::getSites(s_obj, type = s_type)
      expect_true(methods::is(keep2, "Site"))

      park_types <- unique(na.omit(NCRNWater::getSiteInfo(wd, parkcode = park, info = "type")))
      alt_type <- setdiff(park_types, s_type)
      if (length(alt_type)) {
        drop2 <- NCRNWater::getSites(s_obj, type = alt_type[1])
        expect_null(drop2)
      } else {
        succeed(sprintf("No alternative type available in park %s; skipped mismatching filter.", park))
      }
    } else {
      succeed(sprintf("Site has no 'type' in park %s; skipped type filter checks.", park))
    }
  })
}

# ---- Names & uniqueness sanity --------------------------------------------------

for (case in cases) {
  park <- case$park
  test_that(sprintf("[info] unique SiteName per SiteCode under %s", park), {
    sites <- NCRNWater::getSites(wd, parkcode = park)
    sc <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
    nm <- NCRNWater::getSiteInfo(wd, parkcode = park, info = "SiteName")
    expect_equal(length(nm), length(unique(sc)))
    expect_equal(length(nm), length(unique(nm)))
  })
}
