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

# ---- hydrate fixture but muffle benign filterActive() staging warning ----
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

# ---- Unified combo enumerator ------------------------------------------------
# Enumerate (park, site, param) combos from the fixture with flexible filters.
# Returns a data.frame for ergonomic downstream use, or a list if list_out=TRUE.
#
# Parameters:
# - wd                : hydrated NCRNWater object
# - require_data      : TRUE -> only include combos with non-empty Date/Value
# - require_threshold : TRUE -> only include combos with LowerPoint or UpperPoint defined
# - parks             : character vector of parks to include (default = all)
# - shard, n_shards   : allow CI sharding by park index (mod arithmetic)
# - max_per_site      : cap number of characteristics per site (Inf for all)
# - exhaustive        : TRUE -> return all matching; FALSE -> sample n_cases
# - n_cases, seed     : sampling controls when exhaustive = FALSE
# - list_out          : TRUE -> return list(list(park, site, param)); FALSE -> data.frame
#
# Examples
# wd <- getWD()
# 
# ## Example: Get 5 combinations of park/site/char
# n_cases      = 5
# max_per_site = 2
# seed         = 1
# enumerate_combos(wd, exhaustive = FALSE, n_cases = n_cases, max_per_site = max_per_site, seed = seed, list_out = TRUE)
# 
# ## Example: Get one combination of park/site/char and pass to getCharInfo
# df <- enumerate_combos(wd, exhaustive = FALSE, n_cases = 1L)
# upperpoint <- NCRNWater::getCharInfo(park = df$park[1], site = df$site[1], param = df$param[1], info='UpperPoint')
# 
# ## Example: Get a list of all of the park/site/char that have thresholds
# enumerate_combos(wd, require_data = TRUE, require_threshold = TRUE, exhaustive = TRUE, list_out = TRUE)
# 
# ## Example: Get a list of all park/site/char combinations
# enumerate_combos(wd, require_data = TRUE, exhaustive = TRUE, list_out = TRUE)
# 
enumerate_combos <- function(
    wd,
    require_data      = TRUE,
    require_threshold = FALSE,
    parks             = NULL,
    shard             = getOption("ncrnwater.test.shard", 1L),
    n_shards          = getOption("ncrnwater.test.n_shards", 1L),
    max_per_site      = Inf,
    exhaustive        = TRUE,
    n_cases           = getOption("ncrnwater.test.n_cases", 5L),
    seed              = getOption("ncrnwater.test.seed", NULL),
    list_out          = FALSE
) {
  stopifnot(is.list(wd))
  
  # 1) Park selection (+ sharding)
  all_parks <- names(wd)
  if (is.null(parks)) parks <- all_parks
  parks <- intersect(parks, all_parks)
  if (length(parks) == 0L) stop("No matching parks in fixture.")
  keep_idx <- which((match(parks, all_parks) - 1L) %% n_shards == (shard - 1L))
  parks <- parks[keep_idx]
  if (length(parks) == 0L) stop("Shard selection produced zero parks.")
  
  # 2) Traverse sites/characteristics
  rows <- list()
  # Simple local cache to avoid recomputing getWData for the same triple
  cache_env <- new.env(parent = emptyenv())
  cache_key <- function(pk, st, ch) paste(pk, st, ch, sep = "|")
  
  for (pk in parks) {
    sites <- names(wd[[pk]]@Sites)
    for (st in sites) {
      chars <- names(wd[[pk]]@Sites[[st]]@Characteristics)
      take  <- if (is.finite(max_per_site)) utils::head(chars, max_per_site) else chars
      for (ch in take) {
        key <- cache_key(pk, st, ch)
        
        # Data presence check
        have_data <- FALSE
        df <- NULL
        if (require_data) {
          if (exists(key, envir = cache_env, inherits = FALSE)) {
            df <- get(key, envir = cache_env, inherits = FALSE)
          } else {
            df <- NCRNWater::getWData(wd, parkcode = pk, sitecode = st, charname = ch, output = "data.frame")
            assign(key, df, envir = cache_env)
          }
          have_data <- is.data.frame(df) && all(c("Date", "Value") %in% names(df)) && nrow(df) > 0
          if (!have_data) next
        }
        
        # Threshold presence check
        have_threshold <- TRUE
        if (require_threshold) {
          lop <- NCRNWater::getCharInfo(wd, parkcode = pk, sitecode = st, charname = ch, info = "LowerPoint")
          uop <- NCRNWater::getCharInfo(wd, parkcode = pk, sitecode = st, charname = ch, info = "UpperPoint")
          have_threshold <- (!is.na(lop) || !is.na(uop))
          if (!have_threshold) next
        }
        
        rows[[length(rows) + 1L]] <- list(
          park = pk, site = st, param = ch,
          has_data = have_data, has_lower = !is.na(NCRNWater::getCharInfo(wd, parkcode = pk, sitecode = st, charname = ch, info = "LowerPoint")),
          has_upper = !is.na(NCRNWater::getCharInfo(wd, parkcode = pk, sitecode = st, charname = ch, info = "UpperPoint"))
        )
      }
    }
  }
  
  if (length(rows) == 0L) stop("No matching (park, site, param) combos satisfy the filters.")
  
  # 3) Build result frame & optional sampling
  df <- do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  rownames(df) <- NULL
  
  if (!exhaustive) {
    if (!is.null(seed)) set.seed(seed)
    n_cases <- min(n_cases, nrow(df))
    df <- df[sample.int(nrow(df), n_cases), , drop = FALSE]
  }
  
  if (isTRUE(list_out)) {
    # Return as list of triples (backwards-compatible with older helpers)
    return(lapply(seq_len(nrow(df)), function(i) list(park = df$park[i], site = df$site[i], param = df$param[i])))
  }
  df
}

# ---- Public helpers for common test modes ------------------------------------

# 1) All combos with non-empty Date/Value
list_all_valid_combos <- function(wd) {
  enumerate_combos(
    wd,
    require_data = TRUE,
    exhaustive   = TRUE,
    list_out     = TRUE
  )
}

# 2) All combos with at least one threshold (LowerPoint or UpperPoint)
list_all_threshold_combos <- function(wd) {
  enumerate_combos(
    wd,
    require_data      = TRUE,
    require_threshold = TRUE,
    exhaustive        = TRUE,
    list_out          = TRUE
  )
}

# 3) Sample N combos for fast dev runs
sample_n_valid_combos <- function(
    wd,
    n_cases      = getOption("ncrnwater.test.n_cases", 5L),
    max_per_site = getOption("ncrnwater.test.max_per_site", 2L),
    seed         = getOption("ncrnwater.test.seed", NULL)
) {
  enumerate_combos(
    wd,
    exhaustive   = FALSE,
    n_cases      = n_cases,
    max_per_site = max_per_site,
    seed         = seed,
    list_out     = TRUE
  )
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

# Reset the fixture (rarely needed)
reset_waterdata_fixture <- function() {
  if (exists("WaterData", envir = .test_env, inherits = FALSE)) {
    rm("WaterData", envir = .test_env)
  }
  invisible(TRUE)
}
