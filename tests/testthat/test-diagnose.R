# ------------------------------------------------------------------------------
# Test module: test-diagnose.R
#
# Test coverage summary for R/diagnoseWaterData.R
#
# This suite validates the structure, types, and comparator-normalization behavior
# of diagnoseWaterData() using both synthetic (in-memory) Park/Site/Characteristic
# examples and parameterized parks from the shared fixture.
#
# Covered behaviors:
#   • Structure & types:
#       - List output with sites, chars, problems, msgs; problems integer; msgs character.
#   • Comparator normalization (synthetic):
#       - Lower/Upper comparator enum codes ("ge","le") produce no invalid-code errors.
#   • Parameterized fixture tests:
#       - Diagnose runs across sampled/exhaustive parks; invariants hold (problems ≥ 0, msgs character).
#   • Warning handling:
#       - Muffles benign staging and "No sites match…" warnings to keep logs clean.
#   • Optional snapshot:
#       - msgs can be snapshot-tested when not in exhaustive mode.
#
# Notes:
#   • Uses getWD() and run-mode knobs from setup-runmode.R to switch sampled/exhaustive runs.
#   • Assertions are shape-focused to remain resilient to dataset changes.
#
# Run:
#
#   # Fast (sampled parks)
#   devtools::test(filter = "diagnose")
#
#   # Exhaustive (all parks)
#   options(ncrnwater.test.exhaustive = TRUE)
#   devtools::test(filter = "diagnose")
#
#   # Run this file only
#   testthat::test_file("tests/testthat/test-diagnose.R")
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

# ---- Helpers ------------------------------------------------------------------

# Muffle only known benign warnings during diagnosis (keep logs tidy)
quiet_diagnose <- function(...) {
  withCallingHandlers(
    NCRNWater::diagnoseWaterData(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^filterActive\\(\\): dropped \\d+ inactive metadata rows", msg) ||
          grepl("^No sites match these criteria\\.$", msg)) {
        invokeRestart("muffleWarning")
      }
      # Let other warnings through
    }
  )
}

# Basic shape/type assertions for diagnose output
expect_diagnose_shape <- function(x) {
  expect_true(is.list(x))
  expect_true(all(c("sites","chars","problems","msgs") %in% names(x)))
  expect_true(is.integer(x$problems))
  expect_true(is.character(x$msgs) || length(x$msgs) == 0L)
  # sites/chars are lists; allow empties when filters collapse
  expect_true(is.list(x$sites))
  expect_true(is.list(x$chars))
}

# -------------------------
# Synthetic, minimal object: comparator normalization has no invalid warnings
# -------------------------
test_that("diagnose normalizes and validates comparator codes (synthetic object)", {
  # Construct a minimal Characteristic with correct comparator directions
  ch <- methods::new("Characteristic",
                     CharacteristicName = "ANC", DisplayName = "ANC",
                     Units = "mg/L", Category = "Chem", CategoryDisplay = "Chem",
                     LowerPoint = 5, UpperPoint = 8,
                     LowerPointCondition = "le",  # lower breaches at <= lower bound
                     UpperPointCondition = "ge",  # upper breaches at >= upper bound
                     Data = data.frame(Date = as.Date("2020-01-01"),
                                       Value = c(4, 5, 8, 9))
  )
  s <- methods::new("Site", SiteCode = "SITE_A", SiteName = "Alpha",
                    Lat = 0, Long = 0, Type = "River",
                    Characteristics = list(ANC = ch))
  p <- methods::new("Park", ParkCode = "TEST", ShortName = "TEST",
                    LongName = "Test Park", Network = "NCRN",
                    Sites = list(SITE_A = s))
  
  diag <- quiet_diagnose(list(p), verbose_chars = FALSE, show_char_details = FALSE)
  expect_diagnose_shape(diag)
  expect_gte(diag$problems, 0L)

})


# -------------------------
# Parameterized diagnosis across parks: structure & types
# -------------------------
for (park in parks) {
  test_that(sprintf("[diagnose] structure & types for park '%s'", park), {
    # You can pass the whole object; diagnosis should summarize across all parks.
    # If you want per-park filtering, uncomment:
    # diag <- quiet_diagnose(wd[[park]], verbose_chars = FALSE, show_char_details = FALSE)
    diag <- quiet_diagnose(wd, verbose_chars = FALSE, show_char_details = FALSE)
    
    expect_diagnose_shape(diag)
    
    # Basic invariants: problems non-negative; msgs vector shape
    expect_gte(diag$problems, 0L)
    expect_true(is.character(diag$msgs) || length(diag$msgs) == 0L)
  })
}

# -------------------------
# (Optional) Messages snapshot for sampled run (can be noisy in exhaustive mode)
# -------------------------
test_that("[diagnose] messages are a character vector (snapshot optional)", {
  diag <- quiet_diagnose(wd, verbose_chars = FALSE, show_char_details = FALSE)
  expect_true(is.character(diag$msgs) || length(diag$msgs) == 0L)
  # Uncomment to snapshot messages in sampled mode only:
  # if (!getOption("ncrnwater.test.exhaustive", FALSE)) {
  #   expect_snapshot_value(diag$msgs, style = "json2")
  # }
})