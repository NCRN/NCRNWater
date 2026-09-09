# R/shinyHydrationService.R

#' Shiny Hydration Service: prepare NCRNWater data for a given network
#'
#' @description
#' The Shiny hydration service provides a thin, app-facing wrapper to **hydrate**
#' NCRNWater data for a single monitoring network. It constructs the network root
#' directory (`file.path(base_dir, network)`), imports an NCRNWater object via
#' [NCRNWater::importNCRNWater()], and loads the corresponding **active metadata**
#' table into memory for downstream use in Shiny modules.
#'
#' This helper is intentionally small and focused on presentation needs:
#' \itemize{
#'   \item It returns a structure that Shiny can consume directly: the hydrated
#'         `wd` object (list of Park S4 objects), a `metadata_active` data.frame,
#'         and the resolved `root` path used during hydration.
#'   \item It supports explicit file names for both raw and active CSVs so the
#'         Shiny app can choose its ingestion behavior. In many deployments,
#'         **users provide only raw files** (e.g., `"wqp.csv"`, `"wqp_ncrnwater_metadata.csv"`),
#'         and active files are created by calling [NCRNWater::filterActive()].
#' }
#'
#' @details
#' **Behavioral notes**
#' \itemize{
#'   \item This function imports `wd` using `dataname` and `metadataname`. It then
#'         reads the **active** metadata CSV as `metadata_active`. For `"NCRN"`, it
#'         prefers `active_metadataname`; for other networks, it falls back to
#'         `metadataname`. If your workflow **does not pre-create** `*_active.csv`,
#'         consider orchestrating the creation of active files first (e.g., a
#'         higher-level service that calls [NCRNWater::filterActive()]) and then
#'         hydrate with the active basenames.
#'   \item The returned `wd` is a list whose elements are Park S4 objects. The
#'         `metadata_active` data.frame can be header-only or populated; both are
#'         valid shapes for the Shiny presentation adapters.
#' }
#'
#' @section File layout:
#' The service expects the following layout:
#' \preformatted{
#'   <base_dir>/<network>/
#'     wqp.csv
#'     wqp_ncrnwater_metadata.csv
#'     # optionally, if you pre-create active files:
#'     wqp_active.csv
#'     wqp_ncrnwater_metadata_active.csv
#' }
#'
#' @param network Character. Monitoring network code (e.g., `"NCRN"`). Used to
#'   construct the hydration root as `file.path(base_dir, network)`.
#' @param base_dir Character. Base directory containing the network subfolder
#'   with the CSV files. In tests this can be `tempdir()`; in installed assets
#'   it can be `system.file("extdata", package = "NCRNWater")`. Default: `"Data"`.
#' @param dataname Character. Basename of the **raw** WQP data CSV (e.g., `"wqp.csv"`).
#' @param metadataname Character. Basename of the **raw** NCRNWater metadata CSV
#'   (e.g., `"wqp_ncrnwater_metadata.csv"`).
#' @param active_dataname Character. Basename of the **active** WQP data CSV
#'   (e.g., `"wqp_active.csv"`). Provided for scenarios where active files have
#'   already been created upstream; otherwise a higher-level service should
#'   generate them via [NCRNWater::filterActive()]. Default: `"wqp_active.csv"`.
#' @param active_metadataname Character. Basename of the **active** NCRNWater
#'   metadata CSV (e.g., `"wqp_ncrnwater_metadata_active.csv"`). As above, this
#'   is typically the output of [NCRNWater::filterActive()]. Default:
#'   `"wqp_ncrnwater_metadata_active.csv"`.
#' @param wqx Logical. Indicates whether the data files follow WQP format. Passed
#'   through to [NCRNWater::importNCRNWater()]. Default: `TRUE`.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{wd}{Hydrated NCRNWater object (list of Park S4 objects).}
#'   \item{metadata_active}{Active metadata as a `data.frame`.}
#'   \item{root}{Character scalar: the hydration root used (`file.path(base_dir, network)`).}
#' }
#'
#' @examples
#' \dontrun{
#' # Example: hydrate from package-shipped example files under inst/extdata/NCRN
#' base <- system.file("extdata", package = "NCRNWater")
#' res  <- hydrate_network(
#'   network              = "NCRN",
#'   base_dir             = base,
#'   dataname             = "wqp.csv",
#'   metadataname         = "wqp_ncrnwater_metadata.csv",
#'   active_dataname      = "wqp_active.csv",
#'   active_metadataname  = "wqp_ncrnwater_metadata_active.csv",
#'   wqx                  = TRUE
#' )
#' str(res$wd)              # list of Park S4 objects
#' head(res$metadata_active)
#' res$root
#' }
#'
#' @seealso [NCRNWater::importNCRNWater()], [NCRNWater::filterActive()],
#'   and Shiny-specific orchestration helpers (e.g., `shiny_hydrate_network()`).
#'
#' @export
hydrate_network <- function(network,
                            base_dir = "Data",
                            dataname = "wqp.csv",
                            metadataname = "wqp_ncrnwater_metadata.csv",
                            active_dataname = "wqp_active.csv",
                            active_metadataname = "wqp_ncrnwater_metadata_active.csv",
                            wqx = TRUE) {
  root <- file.path(base_dir, network)
  wd <- suppressWarnings(importNCRNWater(root, Data = dataname, MetaData = metadataname, wqx = wqx))
  meta_active_file <- if (network == "NCRN") active_metadataname else metadataname
  meta_active_path <- file.path(base_dir, network, meta_active_file)
  meta_active <- utils::read.csv(meta_active_path, stringsAsFactors = FALSE)
  list(wd = wd, metadata_active = meta_active, root = root)
}
hydrate_network <- function(network,
                            base_dir = "Data",
                            dataname = "wqp.csv",
                            metadataname = "wqp_ncrnwater_metadata.csv",
                            active_dataname = "wqp_active.csv",
                            active_metadataname = "wqp_ncrnwater_metadata_active.csv",
                            wqx = TRUE) {
  root <- file.path(base_dir, network)
  wd <- suppressWarnings(importNCRNWater(root, Data = dataname, MetaData = metadataname, wqx = wqx))
  meta_active_file <- if (network == "NCRN") active_metadataname else metadataname
  meta_active_path <- file.path(base_dir, network, meta_active_file)
  meta_active <- utils::read.csv(meta_active_path, stringsAsFactors = FALSE)
  list(wd = wd, metadata_active = meta_active, root = root)
}
