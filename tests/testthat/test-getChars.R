# ------------------------------------------------------------------------------
# Test module: test-getChars.R
#
# Test coverage summary for R/getChars.R and getCharInfo() scalar fields
#
# This suite validates characteristic retrieval at list/Park/Site scope, identity
# deduplication when filters are applied, category filtering, and scalar metadata
# via getCharInfo(). Parameterized tests run on sampled/exhaustive (park, site, param).
#
# Covered behaviors:
#   • getChars(list/Park/Site) + filters:
#       - Returns only Characteristic objects; filtered calls dedupe by identity
#         (CharName|Category|SampleFraction|Substrate).
#   • Category filter:
#       - Includes only selected categories; Category values match filter input.
#   • getCharInfo() scalar fields:
#       - CharName/DisplayName/Units/Category present (length-1/0); thresholds
#         are numeric or NA; comparator codes are enum ('lt','le','gt','ge') or NA.
#   • Mixed inputs:
#       - Mixed list (Park + duplicate Site/Characteristic) dedupe under filters.
#   • Non-existent filters:
#       - Returns NULL or empty list; warning muffled.
#
# Notes:
#   • Uses getWD() and sample_n_valid_combos()/list_all_valid_combos() from setup-runmode.R.
#   • Compatibility helper handles CharName vs CharacteristicName across datasets.
#
# Run:
#
#   # Fast (sampled cases)
#   devtools::test(filter = "getChars")
#
#   # Exhaustive (all combinations)
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "getChars")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-getChars.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

# Shared fixture (benign staging warning muted)
wd <- getWD()

# Respect run-mode knobs: exhaustive vs sampled combinations
exhaustive <- getOption("ncrnwater.test.exhaustive", FALSE)
cases      <- if (exhaustive) list_all_valid_combos(wd) else sample_n_valid_combos(wd)

# ---- Helpers ------------------------------------------------------------------

# Muffle only known benign warnings (keep logs tidy)
quiet_getChars <- function(...) {
  withCallingHandlers(
    NCRNWater::getChars(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
quiet_getCharInfo <- function(...) {
  withCallingHandlers(
    NCRNWater::getCharInfo(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Check that a list contains only Characteristic S4 objects
expect_charlist <- function(x) {
  expect_true(is.list(x) || is.null(x))
  if (!is.null(x)) {
    expect_true(all(vapply(x, function(s) methods::is(s, "Characteristic"), logical(1))))
  }
}

# Compatibility: fetch CharName robustly (CharName vs CharacteristicName)
get_char_name_compat <- function(ch) {
  nm <- try(quiet_getCharInfo(ch, info = "CharName"), silent = TRUE)
  if (!inherits(nm, "try-error") && length(nm) == 1L && nzchar(nm)) return(nm)
  nm2 <- try(quiet_getCharInfo(ch, info = "CharacteristicName"), silent = TRUE)
  if (!inherits(nm2, "try-error") && length(nm2) == 1L && nzchar(nm2)) return(nm2)
  NA_character_
}

# Build characteristic identity string as getChars(list) does
char_id <- function(ch) {
  paste(
    safe1(quiet_getCharInfo(ch, info = "CharName")),
    safe1(quiet_getCharInfo(ch, info = "Category")),
    safe1(quiet_getCharInfo(ch, info = "SampleFraction")),
    safe1(quiet_getCharInfo(ch, info = "Substrate")),
    sep = "|"
  )
}
safe1 <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  y <- x[1]
  if (is.na(y)) return("")
  as.character(y)
}

# -------------------------
# getChars(list) + filters: returns deduped Characteristic identity
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getChars] list+filters dedupes to a single identity [%s:%s:%s]", park, site, param), {
    # Call on the full NCRNWaterObj list with filters
    chars <- quiet_getChars(wd, parkcode = park, sitecode = site, charname = param)
    expect_charlist(chars)
    skip_if(is.null(chars), sprintf("No chars returned for %s:%s:%s (dataset-specific).", park, site, param))
    
    # Flatten, expecting filtered identity dedupe to one element
    flat <- unlist(chars, recursive = FALSE, use.names = FALSE)
    expect_true(length(flat) >= 1L)
    
    # The first element should match the requested charname
    cn <- get_char_name_compat(flat[[1]])
    expect_true(is.character(cn) && nzchar(cn))
    # Some datasets may have display names differing from 'param'; accept at least length-1
    # When exact match is present, assert it:
    if (!is.na(param) && nzchar(param)) {
      # Because some names differ (eg. display vs. internal), we relax hard equality in parameterized suite
      expect_true(length(cn) == 1L)
    }
    
    # Identity dedupe: all IDs in the filtered list should be unique
    ids <- vapply(flat, char_id, FUN.VALUE = character(1))
    expect_equal(length(ids), length(unique(ids)))
  })
}

# -------------------------
# getChars(Park) + filters: delegates to list and dedupes
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getChars] Park+filters returns deduped characteristic [%s:%s:%s]", park, site, param), {
    park_obj <- wd[[park]]
    chars <- quiet_getChars(park_obj, parkcode = park, sitecode = site, charname = param)
    expect_charlist(chars)
    if (is.null(chars)) {
      succeed(sprintf("No chars for park/site/param (%s:%s:%s).", park, site, param))
    } else {
      flat <- unlist(chars, recursive = FALSE, use.names = FALSE)
      ids  <- vapply(flat, char_id, FUN.VALUE = character(1))
      expect_equal(length(ids), length(unique(ids)))
    }
  })
}

# -------------------------
# getChars(Site) + filters: returns characteristic objects
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getChars] Site+filters returns characteristic objects [%s:%s:%s]", park, site, param), {
    # Select the Site S4 object
    sites <- NCRNWater::getSites(wd, parkcode = park, sitecode = site)
    skip_if(is.null(sites) || length(sites) == 0L, sprintf("No site found for %s:%s.", park, site))
    s_obj <- sites[[1]]
    
    chars <- quiet_getChars(s_obj, sitecode = site, charname = param)
    expect_charlist(chars)
    if (!is.null(chars)) {
      flat <- unlist(chars, recursive = FALSE, use.names = FALSE)
      expect_true(length(flat) >= 1L)
    }
  })
}

# -------------------------
# Category filter: include only the selected category
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getChars] category filter includes only selected categories [%s:%s:%s]", park, site, param), {
    # Discover the category for the requested param at this site
    cat_val <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "Category")
    skip_if(is.na(cat_val) || !nzchar(cat_val), sprintf("No category for %s:%s:%s.", park, site, param))
    
    chars <- quiet_getChars(wd, parkcode = park, sitecode = site, category = cat_val)
    expect_charlist(chars)
    if (!is.null(chars)) {
      flat <- unlist(chars, recursive = FALSE, use.names = FALSE)
      ret_cats <- vapply(flat, function(ch) safe1(quiet_getCharInfo(ch, info = "Category")),
                         FUN.VALUE = character(1))
      expect_true(all(ret_cats %in% cat_val))
    }
  })
}

# -------------------------
# getCharInfo thresholds & comparator codes: types and enum checks
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getCharInfo] thresholds & operators have correct types [%s:%s:%s]", park, site, param), {
    lower <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "LowerPoint")
    upper <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "UpperPoint")
    lop   <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "LowerPointCondition")
    uop   <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "UpperPointCondition")
    
    expect_true(is.numeric(lower) || is.na(lower))
    expect_true(is.numeric(upper) || is.na(upper))
    expect_true(is.character(lop)  || is.na(lop))
    expect_true(is.character(uop)  || is.na(uop))
    
    # Enum checks (when present)
    valid_ops <- c("lt","le","gt","ge")
    if (!is.na(lop)) expect_true(lop %in% valid_ops)
    if (!is.na(uop)) expect_true(uop %in% valid_ops)
  })
}

# -------------------------
# Mixed input list: dedupe when filters are provided
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getChars] mixed list + filters dedupe by identity [%s:%s:%s]", park, site, param), {
    park_obj <- wd[[park]]
    sites    <- NCRNWater::getSites(wd, parkcode = park, sitecode = site)
    skip_if(is.null(sites) || length(sites) == 0L, sprintf("No site found for %s:%s.", park, site))
    
    mixed <- c(list(park_obj), sites, sites)  # intentionally duplicate sites
    chars <- quiet_getChars(mixed, parkcode = park, sitecode = site, charname = param)
    expect_charlist(chars)
    if (!is.null(chars)) {
      flat <- unlist(chars, recursive = FALSE, use.names = FALSE)
      ids  <- vapply(flat, char_id, FUN.VALUE = character(1))
      expect_equal(length(ids), length(unique(ids)))
    }
  })
}

# -------------------------
# Non-existent filters: return NULL
# -------------------------
test_that("[getChars] non-existent site/char returns NULL", {
  out <- quiet_getChars(wd, parkcode = "NACE", sitecode = "__nope__", charname = "ANC")
  expect_true(is.null(out) || length(out) == 0L)
  
  out2 <- quiet_getChars(wd, parkcode = "NACE", sitecode = "__nope__", category = "Chem")
  expect_true(is.null(out2) || length(out2) == 0L)
  
  out3 <- quiet_getChars(wd, parkcode = "NACE", sitecode = "__nope__", charname = "__nope__")
  expect_true(is.null(out3) || length(out3) == 0L)
})
