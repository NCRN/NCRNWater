#' Run the NCRNWater Shiny application
#'
#' @description
#' Launches the Shiny app bundled with the NCRNWater package. At startup this
#' function:
#' \enumerate{
#'   \item Loads the configuration from \code{inst/config/shiny.yml} or a
#'         \emph{user-supplied} YAML file (when \code{config_file} is provided).
#'   \item Hydrates network data from the paths defined in the YAML (e.g.,
#'         \code{files.datadir}, \code{files.dataname}, \code{files.metadataname}).
#'   \item Resolves \code{"auto"} time bounds \emph{after} hydration.
#'   \item Resolves splash content (loading text & images).
#'   \item Injects the bootstrap state into \code{options()} so
#'         \code{inst/shiny/app/global.R} and app code can read it.
#'   \item Runs the Shiny app from \code{system.file("shiny/app", package = "NCRNWater")}.
#' }
#'
#' @param profile Character, default \code{"NCRN"}. A profile name present in
#'   \code{inst/config/shiny.yml}. If you provide \code{config_file}, its profiles
#'   will be used instead.
#' @param config_file Optional character path to a user-supplied YAML configuration.
#'   When present, this overrides the package-shipped \code{inst/config/shiny.yml}.
#' @param overrides Optional named list to override any YAML keys at runtime
#'   (e.g., \code{list(files = list(datadir = "/my/data"))}).
#' @param host Character, Shiny host. Default \code{"127.0.0.1"}.
#' @param port Integer, Shiny port. Default \code{getOption("shiny.port", 8888)}.
#' @param launch_browser Logical, whether to open the browser at launch.
#'   Default \code{TRUE}.
#' @param ... Additional arguments forwarded to \code{shiny::runApp()}.
#'
#' @details
#' The Shiny app expects to find data in the directory layout:
#' \preformatted{
#'   files.datadir/<profile network_code>/
#'     wqp.csv
#'     wqp_ncrnwater_metadata.csv
#'     <imagesdir>/
#'       *.jpg
#' }
#' The exact filenames and directories are controlled by the YAML profiles.
#'
#' @return This function is called for its side-effect of launching the Shiny app.
#' @export
run_shiny_app <- function(profile = "NCRN",
                          config_file = NULL,
                          overrides = NULL,
                          host = "127.0.0.1",
                          port = getOption("shiny.port", 8888),
                          launch_browser = TRUE,
                          ...) {
  
  # ---- 1) Load configuration (strict) ---------------------------------------
  cfg <- NCRNWater::load_app_config(
    profile     = profile,
    config_file = config_file,
    overrides   = overrides,
    use_options = TRUE  # keep if you want dev convenience
  )
  
  # Basic preflight on data paths (fail-fast, clear messages)
  network_code <- cfg$app$network_code
  datadir      <- cfg$files$datadir
  dataname     <- cfg$files$dataname
  metaname     <- cfg$files$metadataname
  imagesdir    <- cfg$files$imagesdir
  
  network_dir  <- file.path(datadir, network_code)
  data_fp      <- file.path(network_dir, dataname)
  meta_fp      <- file.path(network_dir, metaname)
  photos_dir   <- file.path(network_dir, imagesdir)
  
  if (!file.exists(data_fp)) {
    stop("Data file not found: ", data_fp,
         "\nCheck YAML: files.datadir and files.dataname, and that your data directory is correct.")
  }
  if (!file.exists(meta_fp)) {
    stop("Metadata file not found: ", meta_fp,
         "\nCheck YAML: files.datadir and files.metadataname, and that your metadata file is correct.")
  }
  # Photos are optional; handled gently in the app
  photos_available <- dir.exists(photos_dir)
  
  # ---- 2) Hydrate the NCRNWater object --------------------------------------
  dh <- NCRNWater::hydrate_network(
    network_code,
    datadir      = datadir,
    dataname     = dataname,
    metadataname = metaname,
    wqx          = cfg$wqx$enabled
  )
  wd <- dh$wd
  
  # ---- 3) Late resolution of time bounds ('auto' -> numerics) ---------------
  if (identical(cfg$time$min_year, "auto") || identical(cfg$time$max_year, "auto")) {
    yrs <- NCRNWater:::resolve_year_bounds(cfg, wd = wd)
    cfg$time$min_year <- yrs$min_year
    cfg$time$max_year <- yrs$max_year
    if (identical(yrs$source, "fallback")) {
      message(sprintf("time.auto: using fallback bounds %s–%s.", yrs$min_year, yrs$max_year))
    }
  }
  
  # ---- 4) Resolve splash content --------------------------------------------
  splash <- NCRNWater:::resolve_splash(cfg)
  
  # ---- 5) Inject bootstrap state for the app to read ------------------------
  options(
    ncrnwater.cfg             = cfg,
    ncrnwater.wd              = wd,
    ncrnwater.splash          = splash,
    ncrnwater.photos_dir      = if (photos_available) photos_dir else NULL,
    ncrnwater.photos_available= photos_available
  )
  on.exit({
    options(
      ncrnwater.cfg             = NULL,
      ncrnwater.wd              = NULL,
      ncrnwater.splash          = NULL,
      ncrnwater.photos_dir      = NULL,
      ncrnwater.photos_available= NULL
    )
  }, add = TRUE)
  
  # ---- 6) Launch the packaged Shiny app -------------------------------------
  app_dir <- system.file("shiny", "app", package = "NCRNWater")
  if (!nzchar(app_dir) || !file.exists(app_dir)) {
    stop("Shiny app directory not found in installed package (expected inst/shiny/app).")
  }
  
  shiny::runApp(app_dir, host = host, port = port, launch.browser = launch_browser, ...)
}
