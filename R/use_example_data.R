#' Load NCRNWater example WQP data & metadata
#'
#' @description
#' Reads the example CSVs installed with the package (under \code{inst/extdata/NCRN/})
#' and returns them as tibbles (or \code{data.frame}s). Optionally assigns them into
#' an environment (e.g., the user's global environment) under convenient names.
#'
#' @param network Character; default "NCRN".
#' @param reader Which reader to use: \code{"readr"} (default) or \code{"utils"}.
#' @param as_tibble Logical; when \code{TRUE} and \code{reader="readr"}, returns tibbles.
#' @param assign Logical; when \code{TRUE}, assigns the loaded objects into \code{envir}.
#' @param envir Environment; target for assignment when \code{assign=TRUE}. Default \code{.GlobalEnv}.
#' @param data_name Name to assign for the WQP data (default \code{"wqp"}).
#' @param metadata_name Name to assign for the metadata (default \code{"wqp_metadata"}).
#' @return A list with elements \code{wqp} and \code{wqp_metadata}.
#' @examples
#' # Return in-memory tibbles (no assignment)
#' ex <- use_example_data(assign = FALSE)
#' dplyr::glimpse(ex$wqp)
#'
#' # Assign to global env as 'wqp' and 'wqp_metadata'
#' use_example_data(assign = TRUE)
#' ls()  # should show 'wqp' and 'wqp_metadata'
#'
#' @export
use_example_data <- function(network = "NCRN",
                             reader = c("readr", "utils"),
                             as_tibble = TRUE,
                             assign = FALSE,
                             envir = .GlobalEnv,
                             data_name = "wqp",
                             metadata_name = "wqp_metadata") {
  reader <- match.arg(reader)
  
  paths <- example_paths(network = network)
  
  # Read CSVs
  if (reader == "readr") {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("Package 'readr' is not installed; use reader = 'utils' or install 'readr'.")
    }
    wqp <- readr::read_csv(paths$data, show_col_types = FALSE)
    wqp_md <- readr::read_csv(paths$metadata, show_col_types = FALSE)
    if (!as_tibble) {
      wqp   <- as.data.frame(wqp)
      wqp_md <- as.data.frame(wqp_md)
    }
  } else {
    # Base R fallback
    wqp   <- utils::read.csv(paths$data, stringsAsFactors = FALSE, check.names = FALSE)
    wqp_md <- utils::read.csv(paths$metadata, stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  # Optional assignment into user environment
  if (assign) {
    if (!is.environment(envir)) stop("'envir' must be an environment.")
    assign(data_name,     wqp,   envir = envir)
    assign(metadata_name, wqp_md, envir = envir)
  }
  
  list(wqp = wqp, wqp_metadata = wqp_md)
}
