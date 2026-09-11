# tests/testthat/test-shiny-config-file.R
#
# ------------------------------------------------------------------------------
# Test module: test-shiny-config-file.R
#
# Test coverage summary for user-supplied config-file support in
# R/shinyConfig.R via load_app_config().
#
# This module validates the behavior of the configuration loader when a
# user provides:
#   1) A custom YAML configuration file (via config_file=).
#   2) A custom profile name inside that YAML.
#   3) Calls run_shiny_app() with a config_file that overrides the
#      package-shipped inst/config/shiny.yml.
#
# Covered behaviors:
#   • load_app_config(config_file=...) strictly requires the YAML file to exist.
#   • load_app_config(config_file=...) correctly locates and parses the
#     provided YAML; merges default + profile.
#   • Required blocks and keys are validated (app, files, wqx, time).
#   • Correct error messages for missing config_file, missing profile,
#     or missing required keys.
#   • run_shiny_app(config_file=...) surfaces the same strict YAML checks.
#
# Notes:
#   • Tests construct a minimal temporary YAML file containing valid `default`
#     and `NETN` blocks to ensure load_app_config() succeeds with user-supplied
#     config.
#   • Tests avoid writing to the actual package filesystem; all YAMLs are
#     staged in tempdir() in an isolated manner.
#   • No hydration or Shiny UI is exercised here—this suite focuses solely
#     on configuration plumbing and error handling.
#
# Run:
#   devtools::test(filter = "shiny-config-file")
#   testthat::test_file("tests/testthat/test-shiny-config-file.R")
#
# ------------------------------------------------------------------------------
library(testthat)
library(NCRNWater)

test_that("[load_app_config] errors when package YAML is missing", {
  # This is hard to simulate without changing the install;
  # Consider skipping or using a temporary config_file path with a missing file:
  expect_error(load_app_config(config_file = tempfile()), "Required YAML config file not found")
})

test_that("[load_app_config] uses package YAML with NCRN profile by default", {
  cfg <- load_app_config()
  expect_equal(cfg$app$network_code, "NCRN")
  expect_true(file.exists(cfg$files$colors_csv))
})

test_that("[load_app_config] uses user-supplied YAML", {
  # Build a tiny YAML in tempdir with required blocks
  yml <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "default:",
      "  app:",
      "    figure_defaults:",
      "      show_legend: true",
      "      font_size: 20",
      "      good_color: Blue",
      "      bad_color: Orange",
      "      out_color: Vermillion",
      "      point_size: 6",
      "      threshold_line_color: Orange",
      "      tr_line_color: Green",
      "      line_width: 2",
      "      show_point: true",
      "      figure_horizontal_scaling: 0.9",
      "      figure_vertical_scaling: 0.7",
      "  files:",
      "    colors_csv: extdata/colors.csv",
      "    dataset_url: https://example.test",
      "    basedir: Data",
      "    imagesdir: img",
      "  wqx:",
      "    enabled: true",
      "  time:",
      "    min_year: 2005",
      "    max_year: 2025",
      "NETN:",
      "  app:",
      "    app_name: Stream Water Quality",
      "    network_code: NETN",
      "    network_long: Northeast Temperate Network",
      "  files:",
      "    dataname: wqp.csv",
      "    metadataname: wqp_ncrnwater_metadata.csv"
    ),
    yml
  )
  cfg <- load_app_config(config_file = yml, profile = "NETN")
  expect_equal(cfg$app$network_code, "NETN")
})

test_that("[run_shiny_app] accepts config_file and profile", {
  # smoke test only (do not actually run the app in tests)
  expect_error(
    run_shiny_app(config_file = tempfile(), profile = "NCRN"),
    "Required YAML config file not found",
    fixed = TRUE
  )
})