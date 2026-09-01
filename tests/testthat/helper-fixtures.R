# tests/testthat/helper-fixtures.R

# Ensure testthat edition 3
options(testthat.edition = 3)

# Memoized environment to avoid re-importing fixtures
.test_env <- new.env(parent = emptyenv())

# Main fixture: construct an NCRNWater object from built-in example data,
# staged entirely within a per-test temporary directory.
get_waterdata_fixture <- function(wqx = TRUE) {
  
  # If already built during this test session, reuse
  if (exists("WaterData", envir = .test_env, inherits = FALSE)) {
    return(.test_env$WaterData)
  }
  
  # Locate example files inside the installed package
  paths <- NCRNWater::example_paths("NCRN")
  # paths$dir      -> directory containing example files
  # paths$data     -> basename (e.g., "wqp.csv")
  # paths$metadata -> basename (e.g., "wqp_ncrnwater_metadata.csv")
  
  # Stage into a tempdir to avoid mutating the installed package
  td <- withr::local_tempdir()
  stage_dir <- file.path(td, "NCRN")
  dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Copy original example files to tempdir (keep filenames identical)
  ok1 <- file.copy(file.path(paths$dir, paths$data),
                   file.path(stage_dir, paths$data), overwrite = TRUE)
  ok2 <- file.copy(file.path(paths$dir, paths$metadata),
                   file.path(stage_dir, paths$metadata), overwrite = TRUE)
  if (!ok1 || !ok2) {
    stop("Failed to stage example files into tempdir: ", stage_dir)
  }
  
  # Filter to "active" rows inside the tempdir (outputs *_active.csv in stage_dir)
  fa <- NCRNWater::filterActive(
    network           = "NCRN",
    metadata_filename = paths$metadata,  # basenames
    data_filename     = paths$data,
    dir               = td,              # base dir (so inputs at td/NCRN)
    wqx               = wqx,
    out_dir           = stage_dir        # write filtered copies to td/NCRN
  )
  # fa$meta_path, fa$data_path -> absolute paths in td/NCRN
  # fa$mname_active, fa$dname_active -> basenames of filtered files
  
  # Import from the tempdir using the *active* basenames (as importNCRNWater expects)
  wd <- NCRNWater::importNCRNWater(
    Dir      = stage_dir,
    Data     = fa$dname_active,   # e.g., "wqp_active.csv"
    MetaData = fa$mname_active,   # e.g., "wqp_ncrnwater_metadata_active.csv"
    wqx      = wqx
  )
  
  # Cache fixture (in memory only)
  .test_env$WaterData <- wd
  wd
}

# Helper: expect the rows-mode schema (for deduplication tests)
expect_rows_schema <- function(df) {
  needed <- c("LowerPoint","UpperPoint",
              "LowerPointCondition","UpperPointCondition",
              "Exceed_Lower","Exceed_Upper","Exceed_Type")
  missing <- setdiff(needed, names(df))
  testthat::expect_true(length(missing) == 0,
                        info = paste("Missing columns:", paste(missing, collapse = ", ")))
}

# Optional: allow tests to reset the memoized fixture (rarely needed)
reset_waterdata_fixture <- function() {
  if (exists("WaterData", envir = .test_env, inherits = FALSE)) {
    rm("WaterData", envir = .test_env)
  }
  invisible(TRUE)
}