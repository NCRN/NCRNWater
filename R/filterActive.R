#' Filter metadata & data files down to active logical keys
#'
#' @description
#' Reads raw metadata & data CSVs, filters to "active" logical keys, normalizes
#' comparator strings, and writes filtered copies to disk. Returns filenames
#' suitable for passing to `importNCRNWater()`.
#'
#' @param network Character, e.g., "NCRN". Used to build input directory: file.path(dir, network).
#' @param metadata_filename The metadata CSV filename, e.g., "wqp_ncrnwater_metadata.csv".
#' @param data_filename The data CSV filename, e.g., "wqp.csv".
#' @param dir Base directory containing your data folders; default "Data".
#' @param wqx Logical; TRUE if the data file is WQX format.
#' @param out_dir Output directory for filtered files; default is the same folder as inputs.
#' @return A list with:
#'   \describe{
#'     \item{mname_active}{Filtered metadata filename (basename).}
#'     \item{dname_active}{Filtered data filename (basename).}
#'     \item{meta_path}{Full path to filtered metadata.}
#'     \item{data_path}{Full path to filtered data.}
#'     \item{dropped}{A data.frame of dropped metadata rows (for diagnostics).}
#'   }
#' @export
filterActive <- function(network,
                         metadata_filename,
                         data_filename,
                         dir     = "Data",
                         wqx     = TRUE,
                         out_dir = NULL) {
  
  stopifnot(is.character(network), length(network) == 1L)
  base_dir <- file.path(dir, network)
  meta_in  <- file.path(base_dir, metadata_filename)
  data_in  <- file.path(base_dir, data_filename)
  
  if (!file.exists(meta_in))  stop("Metadata file not found: ", meta_in)
  if (!file.exists(data_in))  stop("Data file not found: ", data_in)
  
  # Read input CSVs
  md  <- readr::read_csv(meta_in, col_types = readr::cols())
  dat <- readr::read_csv(data_in, col_types = readr::cols(.default = "c"))
  
  # Normalize HTML-encoded comparators to real operators
  normalize_op <- function(x) {
    x <- as.character(x); x <- trimws(x)
    # Replace entities in the right order (>= / <= first)
    x <- gsub("&lt;=", "<=", x, fixed = TRUE)
    x <- gsub("&gt;=", ">=", x, fixed = TRUE)
    x <- gsub("&lt;",  "<",  x, fixed = TRUE)
    x <- gsub("&gt;",  ">",  x, fixed = TRUE)
    x
  }
  if ("LowerPointCondition" %in% names(md))
    md$LowerPointCondition <- normalize_op(md$LowerPointCondition)
  if ("UpperPointCondition" %in% names(md))
    md$UpperPointCondition <- normalize_op(md$UpperPointCondition)
  
  # Active mask: prefer IsActive; else both IsActiveSiteCode & IsActiveCharacteristicName
  to_logical <- function(x) {
    if (is.null(x))           return(rep(NA, nrow(md)))
    if (is.logical(x))        return(x)
    if (is.numeric(x))        return(as.logical(x))
    x <- tolower(trimws(as.character(x)))
    x %in% c("true", "t", "1")
  }
  
  has_IsActive   <- "IsActive" %in% names(md)
  has_ActiveSite <- "IsActiveSiteCode" %in% names(md)
  has_ActiveChar <- "IsActiveCharacteristicName" %in% names(md)
  
  is_active <- if (has_IsActive) {
    to_logical(md$IsActive)
  } else if (has_ActiveSite && has_ActiveChar) {
    to_logical(md$IsActiveSiteCode) & to_logical(md$IsActiveCharacteristicName)
  } else {
    rep(TRUE, nrow(md))  # conservative default
  }
  
  keep_idx <- which(is_active %in% TRUE)
  drop_idx <- setdiff(seq_len(nrow(md)), keep_idx)
  
  md_keep <- md[keep_idx, , drop = FALSE]
  md_drop <- md[drop_idx, , drop = FALSE]  # <-- FIXED
  
  # Join keys for filtering data
  md_site_col <- if (wqx) "SiteCodeWQX" else "SiteCode"
  md_char_col <- "DataName"  # per your schema
  
  if (!md_site_col %in% names(md_keep)) stop("Metadata missing site column: ", md_site_col)
  if (!md_char_col %in% names(md_keep)) stop("Metadata missing DataName column")
  
  site_vals <- unique(md_keep[[md_site_col]])
  char_vals <- unique(md_keep[[md_char_col]])
  
  if (wqx) {
    site_col <- if ("MonitoringLocationIdentifier" %in% names(dat)) "MonitoringLocationIdentifier" else "SiteCode"
    char_col <- if ("CharacteristicName" %in% names(dat)) "CharacteristicName" else "Characteristic"
  } else {
    site_col <- if ("StationID" %in% names(dat)) "StationID" else "SiteCode"
    char_col <- if ("Local Characteristic Name" %in% names(dat)) "Local Characteristic Name" else "Characteristic"
  }
  
  dat_keep <- dat[dat[[site_col]] %in% site_vals & dat[[char_col]] %in% char_vals, , drop = FALSE]
  
  # Output directory & filenames
  if (is.null(out_dir)) out_dir <- base_dir
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  mname_active <- sub("\\.csv$", "_active.csv", metadata_filename, ignore.case = TRUE)
  dname_active <- sub("\\.csv$", "_active.csv", data_filename,     ignore.case = TRUE)
  meta_out     <- file.path(out_dir, mname_active)
  data_out     <- file.path(out_dir, dname_active)
  
  # Write filtered copies
  readr::write_csv(md_keep,  meta_out)
  readr::write_csv(dat_keep, data_out)
  
  if (nrow(md_drop) > 0) {
    warning(sprintf(
      "filterActive(): dropped %d inactive metadata rows; wrote filtered files:\n- %s\n- %s",
      nrow(md_drop), meta_out, data_out
    ), call. = FALSE)
  }
  
  # Return paths/filenames
  list(
    mname_active = mname_active,
    dname_active = dname_active,
    meta_path    = meta_out,
    data_path    = data_out,
    dropped      = md_drop
  )
}