# tests/testthat/helper-quiet.R
#
# Quiet wrappers to muffle benign staging warnings during tests.
# We only silence the specific "filterActive()" message; all other warnings pass through.

quiet_filterActive <- function(...) {
  withCallingHandlers(
    NCRNWater::filterActive(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^filterActive\\(\\): dropped \\d+ inactive metadata rows", msg)) {
        invokeRestart("muffleWarning")
      }
      # let other warnings bubble
    }
  )
}

quiet_example_ncrnwater <- function(...) {
  withCallingHandlers(
    NCRNWater::example_ncrnwater(...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^filterActive\\(\\): dropped \\d+ inactive metadata rows", msg)) {
        invokeRestart("muffleWarning")
      }
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
      # Let other warnings bubble
    }
  )
}
