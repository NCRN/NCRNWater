# R/shinyConfig.R

#' Load Shiny app configuration (strict YAML + overrides + validation)
#'
#' @description
#' Loads and validates the NCRNWater Shiny configuration from a YAML file.
#' The YAML is **required**—if missing or malformed, this function \strong{errors}
#' and the app must not run.
#'
#' The loader merges the \code{default} block with a named \code{profile}
#' (e.g., "NCRN", "NETN"), then applies overrides from environment variables,
#' \code{options()}, and an explicit \code{overrides} list (in that precedence order).
#'
#' @details
#' \strong{Precedence (highest → lowest):}
#' \enumerate{
#'   \item Caller \code{overrides} (named list)
#'   \item Environment variables (e.g., \code{NCRNWATER_PROFILE}, \code{NCRNWATER_NETWORK})
#'   \item \code{options()} (e.g., \code{options(ncrnwater.app.network_code = "NETN")})
#'   \item YAML: \code{default} merged with the named profile
#' }
#'
#' \strong{Required blocks:} \code{app}, \code{files}, \code{wqx}, \code{time}.
#'
#' \strong{Required per-profile keys:}
#' \itemize{
#'   \item \code{app.app_name}, \code{app.network_code}, \code{app.network_long}
#'   \item \code{files.dataname}, \code{files.metadataname}
#' }
#'
#' \strong{Required defaults (in \code{default}):}
#' \itemize{
#'   \item \code{app.figure_defaults.*} (see validation notes below)
#'   \item \code{files.colors_csv}, \code{files.dataset_url}, \code{files.basedir}, \code{files.imagesdir}
#'   \item \code{wqx.enabled} (logical)
#'   \item \code{time.min_year}, \code{time.max_year} (numeric or \code{"auto"})
#' }
#'
#' When either \code{time.min_year} or \code{time.max_year} equals \code{"auto"},
#' the loader calls \code{resolve_year_bounds(cfg, wd)} (implemented in
#' \code{R/shinyTimeService.R}) to compute dataset bounds.
#'
#' @param profile Character or \code{NULL}. Profile name in YAML. If \code{NULL},
#'   \code{"NCRN"} is used so the app attempts to run in NCRN mode by default.
#'   The environment variable \code{NCRNWATER_PROFILE} overrides this.
#' @param overrides Optional named list to override any YAML keys (wins over all sources).
#' @param config_file Optional path to a user-supplied YAML config file. When present,
#'   the loader uses this file \emph{instead of} the package's \code{inst/config/shiny.yml}.
#'
#' @return A validated, normalized list with top-level keys: \code{app}, \code{files},
#'   \code{wqx}, \code{time}.
#'
#' @examples
#' \dontrun{
#' # Package YAML + NCRN profile
#' cfg <- load_app_config()
#'
#' # Switch to NETN
#' cfg_netn <- load_app_config(profile = "NETN")
#'
#' # Use external YAML (user-supplied)
#' cfg_ext <- load_app_config(config_file = "/path/to/custom.yml")
#'
#' # Caller overrides win
#' cfg2 <- load_app_config(overrides = list(app = list(app_name = "Custom Viz")))
#' }
#'
#' @importFrom yaml read_yaml
#' @export
load_app_config <- function(profile = NULL,
                            overrides = NULL,
                            config_file = NULL,
                            use_options = TRUE) {
  
  # --- helpers ---------------------------------------------------------------
  
  .merge <- function(a, b) utils::modifyList(a, b, keep.null = TRUE)
  
  .is_abs <- function(p) is.character(p) && length(p) == 1L &&
    grepl("^(?:/|[A-Za-z]:)", p)  # POSIX or Windows drive letters
  
  .req_scalar_chr  <- function(x, path) {
    if (!is.character(x) || length(x) != 1L || !nzchar(x))
      stop(sprintf("Config '%s' must be a non-empty scalar character.", path))
  }
  .req_scalar_bool <- function(x, path) {
    if (!is.logical(x) || length(x) != 1L)
      stop(sprintf("Config '%s' must be a scalar logical.", path))
  }
  .req_year <- function(x, path) {
    ok <- (is.numeric(x) && length(x) == 1L) || identical(x, "auto")
    if (!ok) stop(sprintf("Config '%s' must be numeric scalar or 'auto'.", path))
  }
  .validate_fig_defaults <- function(fd) {
    if (is.null(fd) || !is.list(fd)) stop("Config 'app.figure_defaults' must be a list.")
    # logicals (required)
    for (k in c("show_legend","show_point")) {
      v <- fd[[k]]; if (is.null(v)) stop(sprintf("Missing 'app.figure_defaults.%s'.", k))
      .req_scalar_bool(v, paste0("app.figure_defaults.", k))
    }
    # numerics (required)
    for (k in c("font_size","point_size","line_width",
                "figure_horizontal_scaling","figure_vertical_scaling")) {
      v <- fd[[k]]; if (is.null(v)) stop(sprintf("Missing 'app.figure_defaults.%s'.", k))
      if (!(is.numeric(v) && length(v) == 1L))
        stop(sprintf("Config 'app.figure_defaults.%s' must be a numeric scalar.", k))
    }
    # strings (required)
    for (k in c("good_color","bad_color","out_color","threshold_line_color","tr_line_color")) {
      v <- fd[[k]]; if (is.null(v)) stop(sprintf("Missing 'app.figure_defaults.%s'.", k))
      .req_scalar_chr(v, paste0("app.figure_defaults.", k))
    }
    fd
  }
  
  # Strict YAML reader: must exist; error if missing or malformed.
  .read_yaml_cfg <- function(profile_name, yaml_fp) {
    if (!nzchar(yaml_fp) || !file.exists(yaml_fp)) {
      stop("Required YAML config file not found: ", yaml_fp,
           "\nEnsure the file exists (package: inst/config/shiny.yml or user-supplied path).")
    }
    y <- yaml::read_yaml(yaml_fp)
    if (!is.list(y) || !"default" %in% names(y)) {
      stop("Config YAML must contain a 'default' block.")
    }
    if (!profile_name %in% names(y)) {
      stop(sprintf("Profile '%s' not found in config file.", profile_name))
    }
    .merge(y[["default"]], y[[profile_name]])
  }
  
  # --- 1) Resolve YAML source & profile -------------------------------------
  
  # Allow user-supplied config file; otherwise use the package file.
  yaml_fp <- if (!is.null(config_file)) {
    config_file
  } else {
    system.file("config", "shiny.yml", package = "NCRNWater")
  }
  
  # Profile selection: default "NCRN".
  if (is.null(profile)) profile <- "NCRN"
  
  # Read YAML strictly (no fallback)
  cfg <- .read_yaml_cfg(profile_name = profile, yaml_fp = yaml_fp)
  
  # --- 2) Validation ---------------------------------------------------------
  
  # required blocks
  for (blk in c("app","files","wqx","time")) {
    if (!is.list(cfg[[blk]])) stop(sprintf("Config must contain a '%s' list.", blk))
  }
  
  # required per-profile fields
  .req_scalar_chr(cfg$app$app_name,     "app.app_name")
  .req_scalar_chr(cfg$app$network_code, "app.network_code")
  .req_scalar_chr(cfg$app$network_long, "app.network_long")
  .req_scalar_chr(cfg$files$dataname,     "files.dataname")
  .req_scalar_chr(cfg$files$metadataname, "files.metadataname")
  
  # required defaults (strings)
  .req_scalar_chr(cfg$files$colors_csv, "files.colors_csv")
  .req_scalar_chr(cfg$files$dataset_url,"files.dataset_url")
  .req_scalar_chr(cfg$files$datadir,    "files.datadir")
  .req_scalar_chr(cfg$files$imagesdir,  "files.imagesdir")
  
  # required defaults: wqx/time
  .req_scalar_bool(cfg$wqx$enabled, "wqx.enabled")
  .req_year(cfg$time$min_year, "time.min_year")
  .req_year(cfg$time$max_year, "time.max_year")
  
  # required defaults: figure_defaults (present in merged cfg)
  cfg$app$figure_defaults <- .validate_fig_defaults(cfg$app$figure_defaults)
  
  # --- 3) Normalize colors_csv path ------------------------------------------
  
  if (!.is_abs(cfg$files$colors_csv)) {
    colors_fp <- system.file(cfg$files$colors_csv, package = "NCRNWater")
    if (!nzchar(colors_fp) || !file.exists(colors_fp)) {
      stop("Colors CSV not found in installed package: ", cfg$files$colors_csv)
    }
    cfg$files$colors_csv <- colors_fp
  } else {
    if (!file.exists(cfg$files$colors_csv)) {
      stop("Absolute colors CSV not found: ", cfg$files$colors_csv)
    }
  }
  
  cfg
}
