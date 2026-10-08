# tests/testthat/test-run-shiny-app.R
#
# ------------------------------------------------------------------------------
# Test module: test-run-shiny-app.R
#
# Purpose
#   Smoke tests for the packaged Shiny app entrypoint `NCRNWater::run_shiny_app()`.
#   These tests verify that:
#     • The app directory is present in the installed package (inst/shiny/app).
#     • Bootstrap objects (cfg, wd, splash) are correctly injected into options()
#       for `global.R`, `server.R`, and `ui.R` to consume.
#   IMPORTANT: This suite intentionally does not launch the Shiny app or open a
#   browser in CI; it only validates packaging and startup plumbing.
#
# Covered behaviors
#   • Packaged app directory existence:
#       system.file("shiny/app", package = "NCRNWater") -> path is non-empty and dir exists.
#   • Bootstrap state injection (without running the app):
#       cfg <- load_app_config()
#       wd  <- hydrate_network(...)
#       splash <- resolve_splash(cfg)
#       options(ncrnwater.cfg = cfg, ncrnwater.wd = wd, ncrnwater.splash = splash)
#       → All three options are set and non-NULL.
#
# Exclusions
#   • Does NOT call `run_shiny_app()` with `shiny::runApp()` in tests (no UI launch).
#   • Does NOT assert on actual rendering, routes, or reactive flows—covered elsewhere.
#
# How to run
#   devtools::test(filter = "run-shiny-app")
#   testthat::test_file("tests/testthat/test-run-shiny-app.R")
#
# Configuration notes
#   • Profiles and data paths resolve from YAML:
#       - Package default: inst/config/shiny.yml
#       - Optional user-supplied override via run_shiny_app(config_file=...).
#   • For local/dev runs, ensure the YAML points to valid data directories:
#       files.datadir/<NETWORK>/{wqp.csv, wqp_ncrnwater_metadata.csv}
#       files.imagesdir for splash/photos (optional).
#
# Future enhancements
#   • Add a non-launch "dry run" helper to exercise `run_shiny_app()` up to
#     option injection (mocking shiny::runApp).
#   • Add a snapshot test to pin the resolved cfg profile fields for a known
#     test configuration.
#   • Add a parametrized test for using a user-supplied config_file.
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

test_that("[run_shiny_app] packaged app directory exists", {
  app_dir <- system.file("shiny", "app", package = "NCRNWater")
  expect_true(nzchar(app_dir) && dir.exists(app_dir))
})

test_that("[run_shiny_app] bootstrap options are injected (temp sandbox)", {
  # 1) Locate packaged example data
  ext_root <- system.file("extdata", package = "NCRNWater")
  expect_true(nzchar(ext_root) && dir.exists(ext_root))
  
  # 2) Stage into a temp sandbox: <tmp>/NCRN/{wqp.csv, metadata.csv}
  td <- withr::local_tempdir()                 # auto-cleans up after the test
  sandbox <- file.path(td, "NCRN")
  dir.create(sandbox, recursive = TRUE, showWarnings = FALSE)
  
  file.copy(file.path(ext_root, "NCRN", "wqp.csv"),
            file.path(sandbox, "wqp.csv"), overwrite = TRUE)
  file.copy(file.path(ext_root, "NCRN", "wqp_ncrnwater_metadata.csv"),
            file.path(sandbox, "wqp_ncrnwater_metadata.csv"), overwrite = TRUE)
  
  # 3) Load cfg (profile NCRN is fine)
  cfg <- NCRNWater::load_app_config(profile = "NCRN")
  
  # 4) Hydrate using the temp sandbox (NOT inst/extdata)
  dh <- NCRNWater::hydrate_network(
    cfg$app$network_code,            # "NCRN"
    datadir      = td,               # <tmp> root (contains a subfolder "NCRN")
    dataname     = "wqp.csv",
    metadataname = "wqp_ncrnwater_metadata.csv",
    wqx          = cfg$wqx$enabled
  )
  wd <- dh$wd
  splash <- NCRNWater:::resolve_splash(cfg)
  
  # 5) Inject options (what run_shiny_app() does)
  options(ncrnwater.cfg = cfg, ncrnwater.wd = wd, ncrnwater.splash = splash)
  on.exit(options(ncrnwater.cfg = NULL, ncrnwater.wd = NULL, ncrnwater.splash = NULL), add = TRUE)
  
  expect_true(!is.null(getOption("ncrnwater.cfg")))
  expect_true(!is.null(getOption("ncrnwater.wd")))
  expect_true(!is.null(getOption("ncrnwater.splash")))
  
  # 6) Assert active files are written to temp (not inst/)
  expect_true(file.exists(file.path(sandbox, "wqp_active.csv")))
  expect_true(file.exists(file.path(sandbox, "wqp_ncrnwater_metadata_active.csv")))
})

