# R/shinySplashService.R

#' Resolve splash (loading) text and images from configuration
#'
#' @description
#' Collects and validates the splash (loading) content defined in the app
#' configuration (`cfg$app$splash`), and converts splash image filenames into
#' relative file paths based on the configured data directory layout. Returns
#' a ready-to-use list for the Shiny UI: a character vector of loading text
#' and a list of image entries with resolved paths and metadata.
#'
#' @details
#' This helper expects the merged configuration (from `load_app_config()`) to
#' contain a `app.splash` block. It performs:
#' \itemize{
#'   \item Validation that `loading_text` is a character vector.
#'   \item Validation that `loading_images` is a list of maps (one per image),
#'         each with required keys: `src`, `location`, and `date`.
#'   \item Resolution of each image’s full file path:
#'         \code{file.path(cfg$files$datadir, cfg$app$network_code, cfg$files$splashdir, x$src)}.
#'   \item Construction of an \code{alt} string (fallbacks to \code{basename(src)} if
#'         \code{x$alt} is missing).
#'   \item A boolean \code{exists} flag indicating whether the file is present at
#'         the resolved path. (You may choose to warn/stop in the UI if needed.)
#' }
#'
#' **Expected config keys (subset):**
#' \itemize{
#'   \item \code{cfg$app$splash$loading_text}: character vector of splash lines.
#'   \item \code{cfg$app$splash$loading_images}: list of maps; each map must include:
#'         \code{src} (filename), \code{location} (string), \code{date} (ISO string),
#'         and optionally \code{alt} (string).
#'   \item \code{cfg$files$datadir}: base data directory (e.g., \code{"Data"}).
#'   \item \code{cfg$app$network_code}: network subfolder (e.g., \code{"NCRN"}).
#'   \item \code{cfg$files$splashdir}: images subfolder for splash assets (e.g., \code{"img"} or \code{"www"}).
#' }
#'
#' @param cfg A validated configuration list produced by
#'   \code{NCRNWater::load_app_config()}, containing at minimum:
#'   \code{cfg$app$splash}, \code{cfg$files$datadir}, \code{cfg$app$network_code},
#'   and \code{cfg$files$splashdir}.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{loading_text}{Character vector (as provided by config).}
#'   \item{loading_images}{List of image entries; each entry is a list with:
#'         \code{src} (relative path), \code{location} (string), \code{date} (string),
#'         \code{alt} (string), and \code{exists} (logical).}
#' }
#'
#' @examples
#' \dontrun{
#' cfg <- NCRNWater::load_app_config(profile = "NCRN")
#'
#' # Optional: hydrate first, then use splash in UI
#' dh <- NCRNWater::hydrate_network(
#'   cfg$app$network_code,
#'   datadir      = cfg$files$datadir,
#'   dataname     = cfg$files$dataname,
#'   metadataname = cfg$files$metadataname,
#'   wqx          = cfg$wqx$enabled
#' )
#'
#' splash <- NCRNWater:::resolve_splash(cfg)
#' LOADING_TEXT   <- splash$loading_text
#' LOADING_IMAGES <- splash$loading_images
#'
#' # UI example:
#' # htmltools::tags$img(src = LOADING_IMAGES[[1]]$src,
#' #                     alt = LOADING_IMAGES[[1]]$alt,
#' #                     style = "max-width: 100%;")
#' }
#'
#' @seealso \code{\link{load_app_config}} for YAML merging and validation;
#'   \code{\link{hydrate_network}} for data hydration; and
#'   \code{\link{parsePhotos}} for photo indexing.
#'
#' @keywords internal
resolve_splash <- function(cfg) {
  
  sp <- cfg$app$splash
  if (is.null(sp) || !is.list(sp))
    stop("Config 'app.splash' must be present and a list.")
  
  # Validate loading_text
  lt <- sp$loading_text
  if (is.null(lt) || !is.vector(lt) || !all(vapply(lt, is.character, TRUE))) {
    stop("Config 'app.splash.loading_text' must be a character vector.")
  }
  
  # Validate loading_images
  li <- sp$loading_images
  if (is.null(li) || !is.list(li))
    stop("Config 'app.splash.loading_images' must be a list of maps.")
  
  # Resolve image filepaths
  img_root <- file.path(cfg$files$datadir,
                        cfg$app$network_code,
                        cfg$files$splashdir)
  
  imgs <- lapply(li, function(x) {
    required <- c("src", "location", "date")
    miss <- setdiff(required, names(x))
    if (length(miss))
      stop("Missing keys in app.splash.loading_images entry: ",
           paste(miss, collapse = ", "))
    
    fp <- file.path(img_root, x$src)
    
    list(
      src       = fp,
      location  = x$location,
      date      = x$date,
      alt       = if (!is.null(x$alt)) x$alt else basename(x$src),
      exists    = file.exists(fp)
    )
  })
  
  list(loading_text = lt, loading_images = imgs)
}
