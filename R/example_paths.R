#' Locate NCRNWater example CSVs installed with the package
#'
#' @description
#' Returns a named list of fully-qualified file paths for the example WQP data
#' and metadata that ship under \code{inst/extdata/<network>/}. Also returns
#' the directory path that can be used directly in \code{importNCRNWater()}.
#'
#' @param network Character (default: "NCRN").
#'
#' @return A named list:
#'   \describe{
#'     \item{dir}{Directory containing the example files.}
#'     \item{data}{Full path to wqp.csv.}
#'     \item{metadata}{Full path to wqp_ncrnwater_metadata.csv.}
#'   }
#'
#' @examples
#' paths <- example_paths()
#' paths$dir           # directory to pass into importNCRNWater(Dir = paths$dir)
#' paths$data          # WQP file name
#' paths$metadata      # metadata file name
#' paths$data_fp       # full WQP file path
#' paths$metadata_fp   # full metadata file path
#'
#' # Quick use:
#' wd <- importNCRNWater(paths$dir, Data = basename(paths$data), MetaData = basename(paths$metadata))
#'
#' @export
example_paths <- function(network = "NCRN") {
  
  # Directory containing the extdata resources
  dir_fp <- system.file("extdata", network, package = "NCRNWater")
  data_fname <- "wqp.csv"
  metadata_fname <- "wqp_ncrnwater_metadata.csv"
  
  if (!nzchar(dir_fp) || !dir.exists(dir_fp)) {
    stop("Example directory for network '", network, "' not found in inst/extdata/.")
  }
  
  data_fp <- system.file("extdata", network, data_fname, package = "NCRNWater")
  meta_fp <- system.file("extdata", network, metadata_fname, package = "NCRNWater")
  
  if (!nzchar(data_fp) || !file.exists(data_fp)) {
    stop("Example WQP data file not found: ", data_fp)
  }
  if (!nzchar(meta_fp) || !file.exists(meta_fp)) {
    stop("Example metadata file not found: ", meta_fp)
  }
  
  list(
    dir         = dir_fp,
    data        = data_fname,
    metadata    = metadata_fname,
    data_fp     = data_fp,
    metadata_fp = meta_fp
  )
}