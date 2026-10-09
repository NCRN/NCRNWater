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

# if (!file.exists(DATA_FILEPATH)) {
#   stop("Data file not found: ", DATA_FILEPATH, 
#        ". Check cfg$files$datadir and cfg$files$dataname.")
# }
# if (!file.exists(METADATA_FILEPATH)) {
#   stop("Metadata file not found: ", METADATA_FILEPATH, 
#        ". Check cfg$files$datadir and cfg$files$metadataname.")
# }
# 
# ## --- Pre-flight: existence checks for optional assets -----------------------------
# # Don't fail if photos missing; handle gently later
photos_available <- F
# 
# ## --- Hydrate NCRNWater object -----------------------------------------------
# dh <- NCRNWater::hydrate_network(
#   NETWORK,
#   datadir      = DATA_DIRECTORY,
#   dataname     = DATA_FILENAME,
#   metadataname = METADATA_FILENAME,
#   wqx          = WQX_ENABLED
# )
# WaterData        <- dh$wd
# metadata_active  <- dh$metadata_active
# 
# ## --- Late resolution of time bounds ('auto' -> numeric) ----------------------
# if (identical(cfg$time$min_year, "auto") || identical(cfg$time$max_year, "auto")) {
#   yrs <- NCRNWater:::resolve_year_bounds(cfg, wd = WaterData)
#   cfg$time$min_year <- yrs$min_year
#   cfg$time$max_year <- yrs$max_year
#   if (identical(yrs$source, "fallback")) {
#     message(sprintf("time.auto: using fallback bounds %s–%s.", yrs$min_year, yrs$max_year))
#   }
# }
# MINYR <- cfg$time$min_year
# MAXYR <- cfg$time$max_year
# 
# 
# # Resolve splash content from cfg (no globals, no hard-coding)
# splash <- NCRNWater:::resolve_splash(cfg)
# 
# LOADING_TEXT   <- splash$loading_text
# LOADING_IMAGES <- splash$loading_images

#-------------------------
# Facts and images for loading screen
#-------------------------

# LOADING_TEXT <- c(
#   "Loading application..."
#   ,"Loading application..."
#   ,"Loading application..."
# )
# 
# LOADING_IMAGES <- list(
#   list(src= "dwq_NCRN_ANTI_SHCK_2024-06-04_20240604-131442.jpg", location = "Antietam National Battlefield, Sharpsburg Creek", date = "June 4, 2024")
#   ,list(src= "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg", location = "Monocacy National Battlefield, Bush Creek", date = "June 4, 2024")
#   ,list(src= "dwq_NCRN_PRWI_BONE_2024-06-11_20240611-130821.jpg", location = "Prince William Forest Park, Boneyard Run", date = "June 11, 2024")
# )


#### Map Module ####
# 
# mapChooserUI<-function(id){
#   ns<-NS(id)
#   selectizeInput(inputId=ns("MapIn"),label="Select Parks:" , choices=NULL, multiple = TRUE, selected = NULL)
# }
# 
# mapChooser<-function(input,output,session, data, park, chosen){
#   observe({updateSelectizeInput(session, "MapIn", selected=chosen(),
#                                 choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
#   )})
#   return(reactive(input$MapIn))
# }


# mapChooserUI<-function(id){
#   ns<-NS(id)
#   checkboxGroupInput(inputId=ns("MapIn"),label="Select Parks:" , choices=NULL, inline = TRUE)
# }
# 
# mapChooser<-function(input,output,session, data, park, chosen){
#   observe({updateCheckboxGroupInput(session, "MapIn",
#                                 choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
#   )})
#   return(reactive(input$MapIn))
# }
# 
