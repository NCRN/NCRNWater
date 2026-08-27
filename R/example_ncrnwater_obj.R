#' Build an NCRNWater object from shipped example files
#'
#' @description
#' Convenience wrapper: copies example CSVs to a tempdir, filters to active rows,
#' and calls \code{importNCRNWater()} to produce an NCRNWater object for demos/tests.
#'
#' @param wqx Logical; TRUE to treat files as WQX format.
#' @return An NCRNWater object (list of Park objects).
#' @examples
#' wd <- example_ncrnwater()
#' head(names(wd))
#' @export
example_ncrnwater <- function(wqx = TRUE) {
  paths <- example_paths("NCRN")
  
  td <- withr::local_tempdir()
  dir.create(file.path(td, "NCRN"), recursive = TRUE)
  file.copy(paths$data,     file.path(td, "NCRN", "wqp.csv"))
  file.copy(paths$metadata, file.path(td, "NCRN", "wqp_ncrnwater_metadata.csv"))
  
  fn <- filterActive(network = "NCRN",
                     metadata_filename = "wqp_ncrnwater_metadata.csv",
                     data_filename     = "wqp.csv",
                     dir               = td,
                     wqx               = wqx,
                     out_dir           = file.path(td, "NCRN"))
  
  importNCRNWater(Dir = file.path(td, "NCRN"),
                  Data = fn$dname_active,
                  MetaData = fn$mname_active,
                  wqx = wqx)
}
