#' Diagnose park/site uniqueness and characteristic consistency
#'
#' @description
#' `diagnoseWaterData()` provides structured diagnostics for objects created by
#' [importNCRNWater()]. It checks for uniqueness, naming consistency, structural
#' validity, and data integrity at three levels:
#'
#' * Parks
#' * Sites within each park
#' * Characteristics within each site
#'
#' Human‑readable messages are printed during evaluation, and all diagnostic
#' results are returned invisibly for programmatic inspection.
#'
#' @section Structure:
#' The return value is a nested list summarizing all diagnostic findings.
#' Conceptually, it looks like:
#'
#' \preformatted{
#' list(
#'   sites    = list(
#'                 [[1]] = list(
#'                            park     = "ANTI",
#'                            n_sites  = 1,
#'                            n_unique = 1,
#'                            dup_site = character(0),
#'                            char_cnt = table(...)
#'                          ),
#'                 [[2]] = list(...),
#'                 ...
#'              ),
#'
#'   chars    = list(
#'                 ANTI = list(
#'                           NCRN_ANTI_SHCK = list(
#'                               site      = "NCRN_ANTI_SHCK",
#'                               n_chars   = 17,
#'                               unnamed   = FALSE,
#'                               dup_slot  = character(0),
#'                               dup_list  = character(0),
#'                               per_char  = list(
#'                                   [[1]] = list(
#'                                                name_list     = "AirTemp",
#'                                                name_slot     = "AirTemp",
#'                                                category      = "Physical",
#'                                                n_rows        = 120,
#'                                                has_date      = TRUE,
#'                                                has_value     = TRUE,
#'                                                date_is_Date  = TRUE,
#'                                                value_is_num  = TRUE,
#'                                                all_value_na  = FALSE,
#'                                                all_date_na   = FALSE
#'                                             ),
#'                                   ...
#'                               )
#'                           ),
#'                           ...
#'                       ),
#'                 ...
#'              ),
#'
#'   problems = <integer>,
#'
#'   msgs     = character vector of warning messages
#' )
#' }
#'
#' @return
#' Invisibly returns a named list with four elements summarizing all diagnostics:
#'
#' \describe{
#'
#'   \item{sites}{
#'     A list with one element per park, each containing:
#'       \describe{
#'         \item{park}{The park’s `ParkCode`.}
#'         \item{n_sites}{Total number of sites in the park.}
#'         \item{n_unique}{Number of unique site codes.}
#'         \item{dup_site}{A vector of duplicated site codes (empty if none).}
#'         \item{char_cnt}{`summary()` on the number of characteristics per site.}
#'       }
#'   }
#'
#'   \item{chars}{
#'     A nested list of per‑site characteristic diagnostics:
#'
#'     \preformatted{
#'     chars[[ParkCode]][[SiteCode]]
#'     }
#'
#'     Each element contains:
#'       \describe{
#'         \item{site}{The site’s `SiteCode`.}
#'         \item{n_chars}{Number of characteristics at this site.}
#'         \item{unnamed}{TRUE if any list elements in `@Characteristics` were unnamed.}
#'         \item{dup_slot}{Duplicated names based on the S4 slot `@CharacteristicName`.}
#'         \item{dup_list}{Duplicated names of the characteristic list elements.}
#'         \item{per_char}{A list of per‑characteristic summaries, each including:
#'             \describe{
#'               \item{name_list}{List-element name (after disambiguation).}
#'               \item{name_slot}{Underlying `@CharacteristicName`.}
#'               \item{category}{Characteristic category.}
#'               \item{n_rows}{Number of data rows.}
#'               \item{has_date}{Whether the data contains a `Date` column.}
#'               \item{has_value}{Whether the data contains a `Value` column.}
#'               \item{date_is_Date}{Whether `Date` is of class `Date`.}
#'               \item{value_is_num}{Whether `Value` is numeric.}
#'               \item{all_value_na}{TRUE if all `Value` entries are `NA`.}
#'               \item{all_date_na}{TRUE if all `Date` entries are `NA`.}
#'             }
#'         }
#'       }
#'   }
#'
#'   \item{problems}{
#'     Integer count of total diagnostic problems encountered.  
#'     When this value is 0, the function prints:
#'     \preformatted{OK to proceed!}
#'     Otherwise, a message reports the number of problems found.
#'   }
#'
#'   \item{msgs}{
#'     A character vector containing all warning messages issued during diagnostics.
#'   }
#'
#' }
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{importNCRNWater}}}{Import NCRNWater objects from WQP data + metadata.}
#'     \item{\code{\link{congruency}}}{Validate raw data + metadata files before import.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(NCRNWater)
#'
#' # Import WQP data in NCRNWater format
#' Network <- "NCRN"
#' wd <- importNCRNWater(
#'   Dir      = file.path("Data", Network),
#'   Data     = "wqp_activeonly.csv",
#'   MetaData = "wqp_ncrnwater_metadata_activeonly.csv",
#'   wqx      = TRUE
#' )
#'
#' # Basic diagnostics (park → site → characteristic)
#' diag <- diagnoseWaterData(wd)
#'
#' # Suppress per-site characteristic messaging (good for large networks)
#' diag <- diagnoseWaterData(wd, verbose_chars = FALSE)
#'
#' # Enable detailed per-characteristic row/column/type messages
#' diag <- diagnoseWaterData(wd, show_char_details = TRUE)
#'
#' # Inspect diagnostic structure programmatically:
#' diag$sites[[1]]       # Park-level summary
#' diag$chars[[1]]       # Characteristic-level summary
#' diag$problems         # Number of issues detected
#' diag$msgs             # All collected warning messages
#'
#' # Users may check:
#' if (diag$problems == 0) {
#'   cat("No problems detected; OK to proceed!\n")
#' }
#' }
#' @export
#' 


diagnoseWaterData <- function(ParksList, verbose_chars = TRUE, show_char_details = FALSE) {
  stopifnot(is.list(ParksList))
  
  # Running tally & message collector
  problems <- 0L
  msgs     <- character(0)
  
  # Helper that both warns and increments the tally (and collects message)
  add_warn <- function(msg) {
    warning(msg, call. = FALSE)
    problems <<- problems + 1L
    msgs     <<- c(msgs, msg)
  }
  
  ## ------------------------------------------------------------------------
  ## PARK-LEVEL DIAGNOSTICS
  ## ------------------------------------------------------------------------
  message("Park-level diagnostics...")
  park_codes  <- vapply(ParksList, function(p) p@ParkCode,  FUN.VALUE = character(1))
  short_names <- vapply(ParksList, function(p) p@ShortName, FUN.VALUE = character(1))
  long_names  <- vapply(ParksList, function(p) p@LongName,  FUN.VALUE = character(1))
  
  message(sprintf("Parks: %d total; %d unique ParkCode(s).",
                  length(park_codes), length(unique(park_codes))))
  
  if (anyDuplicated(park_codes)) {
    dup <- unique(park_codes[duplicated(park_codes)])
    add_warn(sprintf("Duplicate ParkCode(s) remain: %s", paste(dup, collapse = ", ")))
  }
  
  ## ------------------------------------------------------------------------
  ## SITE-LEVEL DIAGNOSTICS
  ## ------------------------------------------------------------------------
  message("\nSite-level diagnostics...")
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
  
  # Print site summary lines
  for (ss in site_summary) {
    message(sprintf("[%s] Sites: %d total; %d unique.",
                    ss$park, ss$n_sites, ss$n_unique))
    if (length(ss$dup_site) > 0) {
      add_warn(sprintf("[%s] Duplicate SiteCode(s): %s",
                       ss$park, paste(ss$dup_site, collapse = ", ")))
    }
  }
  
  ## ------------------------------------------------------------------------
  ## CHARACTERISTIC-LEVEL DIAGNOSTICS (with messaging)
  ## ------------------------------------------------------------------------
  # Nested diagnostic: park -> site -> per-characteristic checks
  if (verbose_chars){message("\nCharacteristic-level diagnostics...")}
  
  char_diag <- list()
  
  for (p in ParksList) {
    park_id <- p@ParkCode
    
    char_diag[[park_id]] <- lapply(p@Sites, function(s) {
      site_id  <- s@SiteCode
      cs_list  <- s@Characteristics
      is_list  <- is.list(cs_list)
      n_cs     <- length(cs_list)
      cs_names <- names(cs_list)
      
      if (!is_list) add_warn(sprintf("[%s:%s] Characteristics is not a list.", park_id, site_id))
      if (n_cs == 0) add_warn(sprintf("[%s:%s] No characteristics found.", park_id, site_id))
      
      unnamed <- is_list && (is.null(cs_names) || any(!nzchar(cs_names)))
      if (unnamed) add_warn(sprintf("[%s:%s] Unnamed characteristic list elements detected.", park_id, site_id))
      
      slot_names  <- if (n_cs > 0) vapply(cs_list, function(c) c@CharacteristicName, FUN.VALUE = character(1)) else character(0)
      dup_by_slot <- unique(slot_names[duplicated(slot_names)])
      dup_by_list <- if (!is.null(cs_names)) unique(cs_names[duplicated(cs_names)]) else character(0)
      
      if (length(dup_by_slot) > 0) {
        add_warn(sprintf("[%s:%s] Duplicate characteristic names by slot: %s",
                         park_id, site_id, paste(dup_by_slot, collapse = ", ")))
      }
      if (length(dup_by_list) > 0) {
        add_warn(sprintf("[%s:%s] Duplicate characteristic list-element names: %s",
                         park_id, site_id, paste(dup_by_list, collapse = ", ")))
      }
      
      # Per-site characteristic summary line (messaging)
      if (verbose_chars) {
        n_unique <- length(unique(if (!is.null(cs_names)) cs_names else slot_names))
        msg <- sprintf("[%s:%s] Characteristics: %d total; %d unique; %d duplicate name(s); %s",
                       park_id, site_id, n_cs, n_unique,
                       max(0L, n_cs - n_unique),
                       if (unnamed) "unnamed entries present" else "all named")
        message(msg)
      }
      
      # Per-characteristic data integrity checks (messages + warnings)
      per_char <- lapply(seq_len(n_cs), function(i) {
        cname_list <- if (!is.null(cs_names)) cs_names[i] else NA_character_
        cobj       <- cs_list[[i]]
        
        # Prefer accessor for Data; fallback to direct slot
        cdata <- tryCatch(
          getCharInfo(cobj, info = "Data"),
          error = function(e) cobj@Data
        )
        
        # Defensive checks on cdata
        n_rows           <- tryCatch(nrow(cdata), error = function(e) NA_integer_)
        has_date         <- !is.null(cdata) && "Date"  %in% names(cdata)
        has_value        <- !is.null(cdata) && "Value" %in% names(cdata)
        date_is_Date     <- has_date  && inherits(cdata$Date, "Date")
        value_is_numeric <- has_value && is.numeric(cdata$Value)
        all_value_na     <- has_value && all(is.na(cdata$Value))
        all_date_na      <- has_date  && all(is.na(cdata$Date))
        
        # Emit warnings (and count them) for common issues
        if (!has_date)  add_warn(sprintf("[%s:%s:%s] Missing 'Date' column.",  park_id, site_id, cname_list))
        if (!has_value) add_warn(sprintf("[%s:%s:%s] Missing 'Value' column.", park_id, site_id, cname_list))
        if (has_date && !date_is_Date) {
          add_warn(sprintf("[%s:%s:%s] 'Date' is not class Date.", park_id, site_id, cname_list))
        }
        if (has_value && !value_is_numeric) {
          ratio_numeric_like <- suppressWarnings(sum(!is.na(as.numeric(cdata$Value))) / length(cdata$Value))
          if (!is.na(ratio_numeric_like) && ratio_numeric_like > 0.8) {
            add_warn(sprintf("[%s:%s:%s] 'Value' is not numeric but appears mostly numeric; check typing.",
                             park_id, site_id, cname_list))
          } else {
            add_warn(sprintf("[%s:%s:%s] 'Value' is not numeric.", park_id, site_id, cname_list))
          }
        }
        if (has_value && all_value_na) add_warn(sprintf("[%s:%s:%s] All 'Value' entries are NA.", park_id, site_id, cname_list))
        if (has_date  && all_date_na)  add_warn(sprintf("[%s:%s:%s] All 'Date' entries are NA.",  park_id, site_id, cname_list))
        
        # Optional detail message line
        if (show_char_details) {
          message(sprintf("[%s:%s:%s] rows=%s; Date=%s; Value=%s",
                          park_id, site_id, cname_list,
                          if (is.na(n_rows)) "NA" else n_rows,
                          if (!has_date) "missing" else if (date_is_Date) "Date" else "not-Date",
                          if (!has_value) "missing" else if (value_is_numeric) "numeric" else "non-numeric"))
        }
        
        list(
          name_list     = cname_list,
          name_slot     = cobj@CharacteristicName,
          category      = cobj@Category,
          n_rows        = n_rows,
          has_date      = has_date,
          has_value     = has_value,
          date_is_Date  = date_is_Date,
          value_is_num  = value_is_numeric,
          all_value_na  = all_value_na,
          all_date_na   = all_date_na
        )
      })
      
      list(
        site     = site_id,
        n_chars  = n_cs,
        unnamed  = unnamed,
        dup_slot = dup_by_slot,
        dup_list = dup_by_list,
        per_char = per_char
      )
    })
  }
  
  ## ------------------------------------------------------------------------
  ## FINAL SUMMARY LINE
  ## ------------------------------------------------------------------------
  if (problems == 0L) {
    message("\nOK to proceed!\n")
  } else {
    message(sprintf("Found %d problem(s). See warnings above.", problems))
  }
  
  # Return both site and characteristic summaries + problem tally invisibly
  invisible(list(
    sites    = site_summary,
    chars    = char_diag,
    problems = problems,
    msgs     = msgs
  ))
}
