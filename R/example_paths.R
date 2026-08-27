#' Locate NCRNWater example CSVs installed with the package
#'
#' @description
#' Returns a named list of fully-qualified file paths for the example WQP data
#' and metadata that ship under \code{inst/extdata/NCRN/}.
#'
#' @param network Character (default: "NCRN"). Subdirectory under \code{inst/extdata}.
#' @return A named list with elements \code{data} (wqp.csv) and \code{metadata} (wqp_ncrnwater_metadata.csv).
#' @examples
#' paths <- example_paths()
#' paths$data      # absolute path to wqp.csv
#' paths$metadata  # absolute path to wqp_ncrnwater_metadata.csv
#' @export
example_paths <- function(network = "NCRN") {
  data_fp <- system.file("extdata", network, "wqp.csv", package = "NCRNWater")
  meta_fp <- system.file("extdata", network, "wqp_ncrnwater_metadata.csv", package = "NCRNWater")
  
  if (!nzchar(data_fp) || !file.exists(data_fp)) {
    stop("Example data file not found. Ensure inst/extdata/", network, "/wqp.csv exists in the package.")
  }
  if (!nzchar(meta_fp) || !file.exists(meta_fp)) {
    stop("Example metadata file not found. Ensure inst/extdata/", network, "/wqp_ncrnwater_metadata.csv exists.")
  }
  
  list(data = data_fp, metadata = meta_fp)
}
