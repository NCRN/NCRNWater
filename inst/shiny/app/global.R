# global.R
library(NCRNWater)
## --- Load config ------------------------------------------------------
# inst/shiny/app/global.R

# Read bootstrapped objects injected by NCRNWater::run_shiny_app()
cfg    <- getOption("ncrnwater.cfg")
wd     <- getOption("ncrnwater.wd")
splash <- getOption("ncrnwater.splash")

PHOTOS_DIR       <- getOption("ncrnwater.photos_dir")
PHOTOS_AVAILABLE <- isTRUE(getOption("ncrnwater.photos_available"))

if (is.null(cfg) || is.null(wd) || is.null(splash)) {
  stop("NCRNWater::run_shiny_app() must be used to launch the app.\n",
       "Do not call load_app_config() or hydrate inside global.R.")
}

# Unpack values the app expects
NETWORK_LONG <- cfg$app$network_long
NETWORK      <- cfg$app$network_code
VIZ_NAME     <- cfg$app$app_name
MINYR        <- cfg$time$min_year
MAXYR        <- cfg$time$max_year

WaterData        <- wd
metadata_active <- getOption("ncrnwater.metadata_active")

LOADING_TEXT   <- splash$loading_text
LOADING_IMAGES <- splash$loading_images

# Colors CSV path already normalized in load_app_config()
COLORS_CSV_FILENAME <- cfg$files$colors_csv
GRAPH_COLORS <- tryCatch({
  utils::read.csv(COLORS_CSV_FILENAME, header = TRUE, stringsAsFactors = FALSE)
}, error = function(e) {
  stop("Failed to read colors CSV at '", COLORS_CSV_FILENAME, "': ", conditionMessage(e))
})

# Source the helper functions (yearChooserUI, parkChooserUI, etc.)
# runApp() sets the working directory to app_dir, so a relative path works:
source("helpers.R", local = TRUE)

photos_available <- F

