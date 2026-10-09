# R/run_shiny_example.R
#' Run the NCRNWater Shiny app against the packaged example dataset
#' @export
run_shiny_example <- function(...) {
  td <- file.path(tempdir(), "NCRNWaterSandbox")
  sandbox <- file.path(td, "NCRN")
  dir.create(sandbox, recursive = TRUE, showWarnings = FALSE)
  
  ok1 <- file.copy(system.file("extdata/NCRN/wqp.csv", package = "NCRNWater"),
                   file.path(sandbox, "wqp.csv"), overwrite = TRUE)
  ok2 <- file.copy(system.file("extdata/NCRN/wqp_ncrnwater_metadata.csv", package = "NCRNWater"),
                   file.path(sandbox, "wqp_ncrnwater_metadata.csv"), overwrite = TRUE)
  if (!ok1 || !ok2) stop("Failed to stage example data.")
  
  NCRNWater::run_shiny_app(
    profile   = "NCRN",
    overrides = list(files = list(datadir = td)),
    ...
  )
}
