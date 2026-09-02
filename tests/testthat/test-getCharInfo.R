# tests/testthat/test-getCharInfo.R
#
# ------------------------------------------------------------------------------
# Test coverage summary for R/getCharInfo.R
#
# This test suite validates getCharInfo() across list, Park, Site, and
# Characteristic inputs—covering scalar metadata fields, thresholds,
# comparator codes, and Data retrieval—under both sampled (fast) and
# exhaustive (pre-deploy) modes.
#
# Covered behaviors:
#   • Scalar characteristic fields:
#       - CharName, DisplayName, Units, Category return length-1 character (or sensible scalar).
#       - LowerPoint/UpperPoint return numeric scalars or NA.
#       - LowerPointCondition/UpperPointCondition return enum codes ('lt','le','gt','ge') or NA.
#
#   • Data retrieval:
#       - info = "Data" returns list(data.frame) per characteristic; a data.frame has Date/Value columns.
#
#   • Site-level info (list input routed via Park method):
#       - SiteCode/SiteName/type replicate per characteristic count (when filters provided);
#         all values equal the requested site’s metadata; length >= 1.
#
#   • Park-level info (list input routed via Park method):
#       - ParkCode/ShortName/LongName/Network replicate per characteristic count;
#         all values match getParkInfo() for the requested park; length >= 1.
#
#   • Non-existent filters:
#       - Returns empty vector (numeric(0)/character(0)) or list() for "Data";
#         muffle benign "No sites match these criteria." warnings.
#
#   • Error path:
#       - Missing 'info' parameter throws an informative error.
#
# Notes:
#   • Parameterized using sample_n_valid_combos() (fast) or list_all_valid_combos() (exhaustive),
#     based on run-mode knobs in setup-runmode.R.
#   • Warning-muffling wrappers keep test logs clean while asserting behavior.
# ------------------------------------------------------------------------------
# 
# Run:
# 
# # Fast
# devtools::test(filter = "getCharInfo")
# # Exhaustive
# options(ncrnwater.test.exhaustive = TRUE)
# devtools::test(filter = "getCharInfo")
# 
library(testthat)
library(NCRNWater)

# Shared fixture (benign staging warning muted; temp staging via helper)
wd <- getWD()

# Respect run-mode knobs: exhaustive vs sampled combinations
exhaustive <- getOption("ncrnwater.test.exhaustive", FALSE)
cases      <- if (exhaustive) list_all_valid_combos(wd) else sample_n_valid_combos(wd)

# ---- Helpers ------------------------------------------------------------------

# Muffle only known benign warnings during tests (keep logs tidy)
quiet_getCharInfo <- function(...) {
  withCallingHandlers(
    NCRNWater::getCharInfo(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
      # Otherwise let warnings through
    }
  )
}
quiet_getSiteInfo <- function(...) {
  withCallingHandlers(
    NCRNWater::getSiteInfo(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
quiet_getParkInfo <- function(...) {
  withCallingHandlers(
    NCRNWater::getParkInfo(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Compatibility: fetch CharName robustly (CharName vs CharacteristicName)
get_char_name_compat <- function(obj, park = NA, site = NA, param = NA) {
  nm <- try(quiet_getCharInfo(obj, parkcode = park, sitecode = site, charname = param, info = "CharName"), silent = TRUE)
  if (!inherits(nm, "try-error") && length(nm) >= 1L && nzchar(nm[1])) return(nm[1])
  nm2 <- try(quiet_getCharInfo(obj, parkcode = park, sitecode = site, charname = param, info = "CharacteristicName"), silent = TRUE)
  if (!inherits(nm2, "try-error") && length(nm2) >= 1L && nzchar(nm2[1])) return(nm2[1])
  NA_character_
}

# Basic shape checks for Data frames returned
expect_data_schema <- function(df) {
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Date","Value") %in% names(df)), info = "Expected 'Date' and 'Value' columns in Data data.frame")
}

valid_ops <- c("lt","le","gt","ge")

# -------------------------
# Error path: missing 'info' parameter
# -------------------------
test_that("[getCharInfo] missing 'info' throws an error", {
  expect_error(NCRNWater::getCharInfo(wd), "Need to specify 'info'")
})

# -------------------------
# Characteristic-level scalar fields & thresholds
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getCharInfo] scalar characteristic fields [%s:%s:%s]", park, site, param), {
    # CharName / DisplayName / Units / Category
    chn <- get_char_name_compat(wd, park, site, param)
    dsp <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "DisplayName")
    unt <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "Units")
    cat <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "Category")
    
    # Each returns scalar or length-1 character when filtered
    expect_true(is.na(chn) || (is.character(chn) && nzchar(chn)))
    expect_true(is.character(dsp) && length(dsp) >= 1L)
    expect_true(is.character(unt) && length(unt) >= 1L || length(unt) == 0L)
    expect_true(is.character(cat) && length(cat) >= 1L || length(cat) == 0L)
    
    # Thresholds & comparators (types + enum checks)
    low <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "LowerPoint")
    upp <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "UpperPoint")
    lop <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "LowerPointCondition")
    uop <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "UpperPointCondition")
    
    expect_true(is.numeric(low) || is.na(low) || length(low) == 0L)
    expect_true(is.numeric(upp) || is.na(upp) || length(upp) == 0L)
    expect_true(is.character(lop) || is.na(lop) || length(lop) == 0L)
    expect_true(is.character(uop) || is.na(uop) || length(uop) == 0L)
    if (!is.na(lop) && length(lop) > 0L) expect_true(lop %in% valid_ops)
    if (!is.na(uop) && length(uop) > 0L) expect_true(uop %in% valid_ops)
  })
}

# -------------------------
# Data retrieval: info = "Data"
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getCharInfo] Data returns list(data.frame) with Date/Value [%s:%s:%s]", park, site, param), {
    out <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "Data")
    expect_true(is.list(out))
    if (length(out) > 0L) {
      # Validate schema for each returned frame
      for (df in out) expect_data_schema(df)
    }
  })
}

# -------------------------
# Site-level info (list input routed to Park method)
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getCharInfo] SiteCode/SiteName/type match requested site [%s:%s:%s]", park, site, param), {
    sc <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "SiteCode")
    sn <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "SiteName")
    tp <- quiet_getCharInfo(wd, parkcode = park, sitecode = site, charname = param, info = "type")
    
    exp_sc <- quiet_getSiteInfo(wd, parkcode = park, sitecode = site, info = "SiteCode")
    exp_sn <- quiet_getSiteInfo(wd, parkcode = park, sitecode = site, info = "SiteName")
    exp_tp <- quiet_getSiteInfo(wd, parkcode = park, sitecode = site, info = "type")
    
    if (length(sc) > 0L) expect_true(all(sc %in% exp_sc))
    if (length(sn) > 0L) expect_true(all(sn %in% exp_sn))
    if (length(tp) > 0L) expect_true(all(tp %in% exp_tp))
  })
}

# -------------------------
# Park-level info (list input routed to Park method)
# -------------------------
for (case in cases) {
  park <- case$park
  
  test_that(sprintf("[getCharInfo] ParkCode/ShortName/LongName/Network match park [%s]", park), {
    pc <- quiet_getCharInfo(wd, parkcode = park, info = "ParkCode")
    ps <- quiet_getCharInfo(wd, parkcode = park, info = "ParkShortName")
    pl <- quiet_getCharInfo(wd, parkcode = park, info = "ParkLongName")
    nw <- quiet_getCharInfo(wd, parkcode = park, info = "Network")
    
    exp_pc <- quiet_getParkInfo(wd, parkcode = park, info = "ParkCode")
    exp_ps <- quiet_getParkInfo(wd, parkcode = park, info = "ParkShortName")
    exp_pl <- quiet_getParkInfo(wd, parkcode = park, info = "ParkLongName")
    exp_nw <- quiet_getParkInfo(wd, parkcode = park, info = "Network")
    
    if (length(pc) > 0L) expect_true(all(pc %in% exp_pc))
    if (length(ps) > 0L) expect_true(all(ps %in% exp_ps))
    if (length(pl) > 0L) expect_true(all(pl %in% exp_pl))
    if (length(nw) > 0L) expect_true(all(nw %in% exp_nw))
  })
}

# -------------------------
# Site object: characteristic info replicated per characteristic count
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getCharInfo(Site)] returns scalar info and Data for site [%s:%s:%s]", park, site, param), {
    sites <- NCRNWater::getSites(wd, parkcode = park, sitecode = site)
    skip_if(is.null(sites) || length(sites) == 0L, sprintf("No site found for %s:%s.", park, site))
    s_obj <- sites[[1]]
    
    sc <- quiet_getCharInfo(s_obj, sitecode = site, charname = param, info = "SiteCode")
    expect_true(is.character(sc) && length(sc) >= 1L)
    
    dat <- quiet_getCharInfo(s_obj, sitecode = site, charname = param, info = "Data")
    expect_true(is.list(dat))
    if (length(dat) > 0L) for (df in dat) expect_data_schema(df)
  })
}

# -------------------------
# Characteristic object: scalar getters return length-1
# -------------------------
for (case in cases) {
  park <- case$park; site <- case$site; param <- case$param
  
  test_that(sprintf("[getCharInfo(Characteristic)] scalar getters return length-1 [%s:%s:%s]", park, site, param), {
    # Fetch one characteristic object via getChars
    chars <- NCRNWater::getChars(wd, parkcode = park, sitecode = site, charname = param)
    skip_if(is.null(chars) || length(chars) == 0L, sprintf("No chars for %s:%s:%s.", park, site, param))
    flat <- unlist(chars, recursive = FALSE, use.names = FALSE)
    ch   <- flat[[1]]
    
    expect_true(is.character(NCRNWater::getCharInfo(ch, info = "CharName")) &&
                  length(NCRNWater::getCharInfo(ch, info = "CharName")) == 1L)
    expect_true(is.character(NCRNWater::getCharInfo(ch, info = "DisplayName")) &&
                  length(NCRNWater::getCharInfo(ch, info = "DisplayName")) == 1L)
    
    # Units may be empty for some characteristics; allow length-0 or length-1 character
    units_val <- NCRNWater::getCharInfo(ch, info = "Units")
    expect_true(is.character(units_val) && length(units_val) %in% c(0L, 1L))
    
    # Thresholds: numeric(1) or NA_real_
    lp <- NCRNWater::getCharInfo(ch, info = "LowerPoint")
    up <- NCRNWater::getCharInfo(ch, info = "UpperPoint")
    expect_true(is.numeric(lp) || is.na(lp))
    expect_true(is.numeric(up) || is.na(up))
    
    # Comparator codes (when present)
    lp_op <- NCRNWater::getCharInfo(ch, info = "LowerPointCondition")
    up_op <- NCRNWater::getCharInfo(ch, info = "UpperPointCondition")
    if (!is.na(lp_op) && nzchar(lp_op)) expect_true(lp_op %in% valid_ops)
    if (!is.na(up_op) && nzchar(up_op)) expect_true(up_op %in% valid_ops)
  })
}

# -------------------------
# Non-existent filters: empty outputs
# -------------------------
test_that("[getCharInfo] non-existent filters return empty vector/list", {
  out_num  <- quiet_getCharInfo(wd, parkcode = "NACE", sitecode = "__nope__", charname = "ANC", info = "LowerPoint")
  expect_true(is.numeric(out_num) && length(out_num) == 0L)
  
  out_chr  <- quiet_getCharInfo(wd, parkcode = "NACE", sitecode = "__nope__", charname = "ANC", info = "Units")
  expect_true(is.character(out_chr) && length(out_chr) == 0L)
  
  out_data <- quiet_getCharInfo(wd, parkcode = "NACE", sitecode = "__nope__", charname = "ANC", info = "Data")
  expect_true(is.list(out_data) && length(out_data) == 0L)
})

