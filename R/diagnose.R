#' Diagnose park/site uniqueness and characteristic counts in NCRNWater data
#'
#' @description
#' `diagnoseWaterData()` performs lightweight diagnostics on an imported NCRNWater
#' object (i.e., the list returned by [importNCRNWater()]). It checks for:
#' - **Park-level** uniqueness by `@ParkCode`
#' - **Site-level** uniqueness (within each park) by `@SiteCode`
#' - **Per-site characteristic counts**
#'
#' The function prints a concise summary to the console (via `message()`), and
#' emits warnings (via `warning()`) when duplicates are detected. It does **not**
#' modify the input. A structured summary is returned **invisibly** for programmatic
#' use.
#'
#' @param ParksList A list of S4 `Park` objects as produced by [importNCRNWater()].
#'   Each `Park` should contain a `@ParkCode`, `@ShortName`, `@LongName`, and a
#'   `@Sites` list of S4 `Site` objects. Each `Site` should contain a `@SiteCode`
#'   and a `@Characteristics` list of S4 `Characteristic` objects.
#'
#' @return
#' A list returned **invisibly** (`invisible(...)`) with one element per park.
#' Each element is a list containing:
#' \describe{
#'   \item{park}{Character scalar: the park `@ParkCode`.}
#'   \item{n_sites}{Integer: total number of sites in the park.}
#'   \item{n_unique}{Integer: number of unique site codes in the park.}
#'   \item{dup_site}{Character vector: any duplicated `SiteCode` values (empty if none).}
#'   \item{char_cnt}{Summary of the per-site characteristic counts (an object of class
#'   `"table"` as produced by `summary(integer_vector)`).}
#' }
#'
#' @details
#' This function is intended for quick, interactive diagnostics after importing
#' water data with [importNCRNWater()]. It helps surface upstream metadata issues
#' (e.g., inconsistent labels, whitespace/casing differences) that can manifest
#' as duplicate parks or sites. Use this alongside the import function’s warnings
#' to identify and resolve metadata inconsistencies.
#'
#' The function relies on direct slot access to NCRNWater S4 classes:
#' `Park` (`@ParkCode`, `@Sites`), `Site` (`@SiteCode`, `@Characteristics`), and
#' `Characteristic` (counted via length of `@Characteristics`). It does not mutate
#' any objects.
#'
#' @seealso
#' [importNCRNWater()], [getParkInfo()], [getSiteInfo()], [getWData()]
#'
#' @examples
#' \dontrun{
#' library(NCRNWater)
#'
#' Network <- "NCRN"
#' wd <- importNCRNWater(
#'   Dir      = file.path("Data", Network),
#'   Data     = "wqp_activeonly.csv",
#'   MetaData = "wqp_ncrnwater_metadata_activeonly.csv",
#'   wqx      = TRUE
#' )
#'
#' # Print diagnostic summary to the console and capture structured results
#' diag <- diagnoseWaterData(wd)
#'
#' # Inspect the first park's diagnostics
#' diag[[1]]$park
#' diag[[1]]$n_sites
#' diag[[1]]$n_unique
#' diag[[1]]$dup_site
#' diag[[1]]$char_cnt
#'}
#' @export
#'


diagnoseWaterData <- function(ParksList) {
  stopifnot(is.list(ParksList))
  
  # Park-level checks
  park_codes  <- vapply(ParksList, function(p) p@ParkCode, FUN.VALUE = character(1))
  short_names <- vapply(ParksList, function(p) p@ShortName, FUN.VALUE = character(1))
  long_names  <- vapply(ParksList, function(p) p@LongName, FUN.VALUE = character(1))
  
  message(sprintf("Parks: %d total; %d unique ParkCode(s).",
                  length(park_codes), length(unique(park_codes))))
  
  if (anyDuplicated(park_codes)) {
    dup <- unique(park_codes[duplicated(park_codes)])
    warning(sprintf("Duplicate ParkCode(s) remain: %s", paste(dup, collapse = ", ")), call. = FALSE)
  }
  
  # Site-level checks per park
  site_summary <- lapply(ParksList, function(p) {
    site_codes <- vapply(p@Sites, function(s) s@SiteCode, FUN.VALUE = character(1))
    n_char     <- vapply(p@Sites, function(s) length(s@Characteristics), FUN.VALUE = integer(1))
    list(
      park     = p@ParkCode,
      n_sites  = length(site_codes),
      n_unique = length(unique(site_codes)),
      dup_site = unique(site_codes[duplicated(site_codes)]),
      char_cnt = summary(n_char)
    )
  })
  
  # Print summary
  for (ss in site_summary) {
    message(sprintf("[%s] Sites: %d total; %d unique.",
                    ss$park, ss$n_sites, ss$n_unique))
    if (length(ss$dup_site) > 0) {
      warning(sprintf("[%s] Duplicate SiteCode(s): %s",
                      ss$park, paste(ss$dup_site, collapse = ", ")), call. = FALSE)
    }
  }
  
  invisible(site_summary)
}
