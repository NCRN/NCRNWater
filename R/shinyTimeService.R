# R/shinyTimeService.R

#' Resolve global year bounds for the app (late-stage, supports `"auto"`)
#'
#' @description
#' Computes dataset-wide **minimum** and **maximum** years used by the Shiny app
#' when the configuration specifies `"auto"` for `time.min_year` and/or
#' `time.max_year`. This helper is intentionally **late-stage** and data-aware:
#' it should be called **after hydration** so it can inspect the hydrated
#' NCRNWater object (`wd`) and/or WQP inputs when enabled.
#'
#' @details
#' Resolution follows a **three-tier strategy**:
#'
#' 1. **WQP path** (when `cfg$wqx$enabled == TRUE` and `cfg$time.* == "auto"`):  
#'    Attempts to read the raw WQP CSV named by `cfg$files$dataname` from the
#'    installed package’s examples (`inst/extdata/<network>/<dataname>`) and
#'    derive years from `ActivityStartDate`. Both `YYYY-MM-DD` and `YYYYMMDD`
#'    formats are accepted.
#'
#' 2. **Hydrated data path** (when `wd` is provided):  
#'    Falls back to the hydrated object and derives years from the `Date` column
#'    of `getWData(wd, output = "data.frame")`.
#'
#' 3. **Config fallback** (when neither path yields years):  
#'    Returns the numeric values already present in `cfg$time.min_year` /
#'    `cfg$time.max_year` (i.e., not `"auto"`).
#'
#' The function returns the resolved numeric bounds along with a `source` flag
#' indicating `"derived"` when years were computed from data (WQP or `wd`) or
#' `"fallback"` when the function returned the numeric values from the config.
#'
#' @param cfg A validated configuration list returned by
#'   `NCRNWater::load_app_config()`. It must contain:
#'   - `cfg$wqx$enabled` (logical)
#'   - `cfg$time$min_year`, `cfg$time$max_year` (numeric or `"auto"`)
#'   - `cfg$app$network_code` (character)
#'   - `cfg$files$dataname` (character; WQP CSV basename)
#' @param wd Optional hydrated NCRNWater object (list of Park S4 objects).
#'   When provided, this enables the hydrated-data path (tier 2).
#'
#' @return A named list with:
#'   - `min_year` (numeric): final minimum year
#'   - `max_year` (numeric): final maximum year
#'   - `source` (character): `"derived"` or `"fallback"`
#'
#' @examples
#' \dontrun{
#' # Load config (pure, may contain "auto"):
#' cfg <- NCRNWater::load_app_config(profile = "NCRN")
#'
#' # Hydrate data:
#' dh <- NCRNWater::hydrate_network(
#'   cfg$app$network_code,
#'   datadir      = cfg$files$datadir,
#'   dataname     = cfg$files$dataname,
#'   metadataname = cfg$files$metadataname,
#'   wqx          = cfg$wqx$enabled
#' )
#'
#' # Resolve:
#' yrs <- NCRNWater::resolve_year_bounds(cfg, wd = dh$wd)
#' yrs$min_year; yrs$max_year; yrs$source
#' }
#'
#' @keywords internal
# R/shinyTimeService.R

#' Resolve min/max year for the app (supports 'auto')
#' @keywords internal
resolve_year_bounds <- function(cfg, wd = NULL) {
  # 1) Try WQP CSV when enabled and 'auto' requested
  use_wqp  <- isTRUE(cfg$wqx$enabled)
  need_min <- identical(cfg$time$min_year, "auto")
  need_max <- identical(cfg$time$max_year, "auto")
  
  years <- integer(0)
  
  if (use_wqp && (need_min || need_max)) {
    # Prefer real datadir if provided; otherwise fall back to package examples.
    # NOTE: late-stage resolution typically happens after hydration, so reading
    # WQP here is optional; hydrated path (below) will cover most cases.
    if (!is.null(cfg$files$datadir)) {
      wqp_fp <- file.path(cfg$files$datadir, cfg$app$network_code, cfg$files$dataname)
    } else {
      base_dir <- dirname(system.file(cfg$files$dataname, package = "NCRNWater"))
      wqp_fp   <- file.path(base_dir, cfg$app$network_code, cfg$files$dataname)
    }
    
    if (file.exists(wqp_fp)) {
      w <- utils::read.csv(wqp_fp, stringsAsFactors = FALSE)
      if ("ActivityStartDate" %in% names(w)) {
        d <- suppressWarnings(lubridate::ymd(w$ActivityStartDate))
        if (all(is.na(d))) {
          d <- suppressWarnings(as.Date(w$ActivityStartDate, format = "%Y%m%d"))
        }
        yrs <- unique(as.integer(format(d, "%Y")))
        years <- yrs[!is.na(yrs)]
      }
    }
  }
  
  # 2) If still empty, derive from hydrated wd (Date column in getWData)
  if (!length(years) && !is.null(wd)) {
    df <- NCRNWater::getWData(wd, output = "data.frame")
    if (is.data.frame(df) && "Date" %in% names(df) && nrow(df) > 0) {
      yrs   <- unique(as.integer(format(df$Date, "%Y")))
      years <- yrs[!is.na(yrs)]
    }
  }
  
  # 3) Produce min/max with fallback to numeric cfg values (not 'auto')
  #    IMPORTANT: add explicit 'else' branches so values are never NULL.
  out_min <- if (identical(cfg$time$min_year, "auto")) {
    if (length(years)) min(years) else as.numeric(cfg$time$min_year)
  } else {
    as.numeric(cfg$time$min_year)
  }
  
  out_max <- if (identical(cfg$time$max_year, "auto")) {
    if (length(years)) max(years) else as.numeric(cfg$time$max_year)
  } else {
    as.numeric(cfg$time$max_year)
  }
  
  src <- if (length(years)) "derived" else "fallback"
  list(min_year = out_min, max_year = out_max, source = src)
}
