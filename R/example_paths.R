#' Locate NCRNWater example CSVs installed with the package
#'
#' @description
#' Returns:
#'   - dir: directory under inst/extdata/<network>/
#'   - data_fp / metadata_fp: full paths to example CSVs
#'   - data / metadata: basenames parsed from those paths
#'
#' @param network Character. Default: "NCRN".
#'
#' @return A named list with:
#'   \describe{
#'     \item{dir}{Directory containing the example files.}
#'     \item{data_fp}{Absolute path to the WQP example data file.}
#'     \item{metadata_fp}{Absolute path to the example metadata file.}
#'     \item{data}{Filename (basename) of the WQP data file.}
#'     \item{metadata}{Filename (basename) of the metadata file.}
#'   }
#'
#' @examples
#' paths <- example_paths()
#' importNCRNWater(
#'     Dir      = paths$dir,
#'     Data     = paths$data,
#'     MetaData = paths$metadata,
#'     wqx      = TRUE
#' )
#'
#' @export
example_paths <- function(network = "NCRN") {
  
  # Directory containing example data
  dir_fp <- system.file("extdata", network, package = "NCRNWater")
  if (!nzchar(dir_fp) || !dir.exists(dir_fp)) {
    stop("Example directory for network '", network, "' not found under inst/extdata/")
  }
  
  # Full file paths
  data_fp <- system.file("extdata", network, "wqp.csv", package = "NCRNWater")
  meta_fp <- system.file("extdata", network, "wqp_ncrnwater_metadata.csv", package = "NCRNWater")
  
  if (!nzchar(data_fp) || !file.exists(data_fp)) {
    stop("Example WQP data file not found: ", data_fp)
  }
  if (!nzchar(meta_fp) || !file.exists(meta_fp)) {
    stop("Example metadata file not found: ", meta_fp)
  }
  
  # Filenames
  data_fname     <- basename(data_fp)
  metadata_fname <- basename(meta_fp)
  
  list(
    dir         = dir_fp,
    data        = data_fname,
    metadata    = metadata_fname,
    data_fp     = data_fp,
    metadata_fp = meta_fp
  )
}