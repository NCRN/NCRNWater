# tests/testthat/helper-fixtures.R

# Ensure testthat edition 3
options(testthat.edition = 3)

# Memoized environment to avoid re-importing fixtures
.test_env <- new.env(parent = emptyenv())

# ---- Core fixture: hydrate NCRNWater object from example files in a temp staging dir ----
get_waterdata_fixture <- function(wqx = TRUE) {
  
  # If already built during this test session, reuse
  if (exists("WaterData", envir = .test_env, inherits = FALSE)) {
    return(.test_env$WaterData)
  }
  
  paths <- NCRNWater::example_paths("NCRN")
  td <- withr::local_tempdir()
  stage_dir <- file.path(td, "NCRN")
  dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Copy raw example files to temp staging
  ok1 <- file.copy(file.path(paths$dir, paths$data),
                   file.path(stage_dir, paths$data), overwrite = TRUE)
  ok2 <- file.copy(file.path(paths$dir, paths$metadata),
                   file.path(stage_dir, paths$metadata), overwrite = TRUE)
  if (!ok1 || !ok2) stop("Failed to stage example files into tempdir: ", stage_dir)
  
  # Filter to "active" rows inside tempdir (outputs *_active.csv in stage_dir)
  fa <- NCRNWater::filterActive(
    network           = "NCRN",
    metadata_filename = paths$metadata,  # basenames
    data_filename     = paths$data,
    dir               = td,              # base dir; inputs at td/NCRN
    wqx               = wqx,
    out_dir           = stage_dir        # write filtered copies to td/NCRN
  )
  
  # Import using the *active* basenames (as importNCRNWater expects)
  wd <- NCRNWater::importNCRNWater(
    Dir      = stage_dir,
    Data     = fa$dname_active,   # e.g., "wqp_active.csv"
    MetaData = fa$mname_active,   # e.g., "wqp_ncrnwater_metadata_active.csv"
    wqx      = wqx
  )
  
  .test_env$WaterData <- wd
  wd
}

# ---- Friendly wrapper: hydrate fixture but muffle benign filterActive() staging warning ----
getWD <- function() {
  withCallingHandlers(
    get_waterdata_fixture(),
    warning = function(w) {
      msg <- conditionMessage(w)
      # Only silence the known staging message; let other warnings through
      if (grepl("^filterActive\\(\\): dropped \\d+ inactive metadata rows", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# ---- Picker: find a valid (park, site, param) with non-empty Date/Value data ----
pick_valid_combo <- function(wd) {
  parks <- names(wd)
  for (pk in parks) {
    sites <- names(wd[[pk]]@Sites)
    for (st in sites) {
      chars <- names(wd[[pk]]@Sites[[st]]@Characteristics)
      for (ch in chars) {
        df <- NCRNWater::getWData(
          wd, parkcode = pk, sitecode = st, charname = ch, output = "data.frame"
        )
        if (is.data.frame(df) && all(c("Date","Value") %in% names(df)) && nrow(df) > 0) {
          return(list(park = pk, site = st, param = ch))
        }
      }
    }
  }
  stop("No valid (park, site, param) with non-empty Date/Value data found in the fixture.")
}

# ---- Expectation helper: rows-mode schema check ----
expect_rows_schema <- function(df) {
  needed <- c("LowerPoint","UpperPoint",
              "LowerPointCondition","UpperPointCondition",
              "Exceed_Lower","Exceed_Upper","Exceed_Type")
  missing <- setdiff(needed, names(df))
  testthat::expect_true(length(missing) == 0,
                        info = paste("Missing columns:", paste(missing, collapse=", ")))
}

# Optional: allow tests to reset the memoized fixture (rarely needed)
reset_waterdata_fixture <- function() {
  if (exists("WaterData", envir = .test_env, inherits = FALSE)) {
    rm("WaterData", envir = .test_env)
  }
  invisible(TRUE)
}

# Find all valid (park, site, param) combos with non-empty Date/Value
list_valid_combos <- function(wd, max_per_site = 2L) {
  out <- list()
  parks <- names(wd)
  for (pk in parks) {
    sites <- names(wd[[pk]]@Sites)
    for (st in sites) {
      chars <- names(wd[[pk]]@Sites[[st]]@Characteristics)
      # Optional cap per site to keep test time reasonable
      take <- head(chars, max_per_site)
      for (ch in take) {
        df <- NCRNWater::getWData(wd, parkcode = pk, sitecode = st, charname = ch, output = "data.frame")
        if (is.data.frame(df) && all(c("Date","Value") %in% names(df)) && nrow(df) > 0) {
          out[[length(out) + 1L]] <- list(park = pk, site = st, param = ch)
        }
      }
    }
  }
  out
}

# Sample N parameterized cases; defaults allow overriding via option/env
sample_valid_combos <- function(wd,
                                n_cases = getOption("ncrnwater.test.n_cases", 5L),
                                max_per_site = getOption("ncrnwater.test.max_per_site", 2L),
                                seed = getOption("ncrnwater.test.seed", NULL)) {
  cases <- list_valid_combos(wd, max_per_site = max_per_site)
  if (length(cases) == 0L) stop("No valid cases found in fixture.")
  if (!is.null(seed)) set.seed(seed)
  if (length(cases) <= n_cases) return(cases)
  cases[sample.int(length(cases), n_cases)]
}