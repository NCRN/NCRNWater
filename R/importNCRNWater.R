#' @include NCRNWater_Park_Class_def.R
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @title importNCRNWater
#' 
#' @description This function imports data from a .csv files exported from NPStoret and saves it as \code{Park} objects. 
#' 
#' @param Dir The directory where the data is found
#' @param Data The data file. Defaults to "Water Data.csv"
#' @param MetaData The metadata file. Defaults to "MetaData.csv"
#' @param wqx Indicates if the data file is in wqx format, defaults to \code{FALSE}.
#' 
#' @return Returns \code{Park} objects, one for each park, as a \code{list}.
#' 
#' @importFrom dplyr distinct group_by filter mutate rename select ungroup
#' @importFrom lubridate mdy ymd
#' @importFrom magrittr %>%
#' @importFrom purrr map map2 pmap
#' @importFrom readr cols read_csv
#' @importFrom methods new
#' 
#' @examples 
#' ncrnwd<-importNCRNWater(Dir = "./Data/NCRN", Data = "Water Data.csv", MetaData = "VizMetaData.csv")
#' 
#' @export
#' 
importNCRNWater <- function(Dir, Data = "Water Data.csv", MetaData = "MetaData.csv", wqx = FALSE){
  
  #### Read in Data ####
  if (wqx) {
    Indata <- read_csv(paste(Dir, Data, sep = "/"), col_types = cols(.default = "c")) %>%
      rename(SiteCode = MonitoringLocationIdentifier, Date = `ActivityStartDate`,
             Characteristic = `CharacteristicName`, Value = `ResultMeasureValue`) %>%
      mutate(TextValue = Value)
    Indata$Date <- ymd(Indata$Date)
  } else {
    Indata <- read_csv(paste(Dir, Data, sep = "/"), col_types = cols(.default = "c")) %>%
      rename(SiteCode = StationID, Date = `Visit Start Date`, Characteristic = `Local Characteristic Name`,
             Value = `Result Value/Text`) %>%
      mutate(TextValue = Value)
    Indata$Date <- mdy(Indata$Date)
  }
  
  if (any(names(Indata) == "ValueCen") & any(names(Indata) == "Censored")) {
    Indata <- Indata %>% mutate(ValueCen = as.numeric(ValueCen), Censored = as.logical(Censored))
  }
  
  MetaData <- read_csv(paste(Dir, MetaData, sep = "/"), col_types = cols())  # makes function less chatty
  
  #### Check whether MQL and UQL fields (Minimum and Upper Detection Limits) are in Indata. 
  # Add them if not, make them numeric if they are
  if (any(names(Indata) == "MQL")) {
    Indata$MQL <- as.numeric(Indata$MQL)
  } else {
    Indata$MQL <- as.numeric(NA)
  }
  
  if (any(names(Indata) == "UQL")) {
    Indata$UQL <- as.numeric(Indata$UQL)
  } else {
    Indata$UQL <- as.numeric(NA)
  }
  
  #### Create Data part of each characteristic ####
  if (wqx) {
    MetaData$Data <- MetaData %>%
      dplyr::select(SiteCodeWQX, DataName) %>%
      pmap(.f = function(SiteCodeWQX, DataName) {
        dplyr::filter(Indata, SiteCode == !!SiteCodeWQX, Characteristic == DataName) %>%
          dplyr::select(-SiteCode, -Characteristic)
      })
  } else {
    MetaData$Data <- MetaData %>%
      dplyr::select(SiteCode, DataName) %>%
      pmap(.f = function(SiteCode, DataName) {
        dplyr::filter(Indata, SiteCode == !!SiteCode, Characteristic == DataName) %>%
          dplyr::select(-SiteCode, -Characteristic)
      })
  }
  
  #### Change numeric data to numeric, but leave the rest as character ####
  NumDat <- MetaData$DataType == "numeric"
  MetaData[NumDat, ]$Data <- suppressWarnings(
    MetaData[NumDat, ]$Data %>% purrr::map(.f = function(x) dplyr::mutate(x, Value = as.numeric(Value)))
  )
  
  #### Create Characteristic objects ####
  MetaData$Characteristics <- MetaData %>%
    dplyr::select(CharacteristicName, DisplayName, Units, Category,
                  CategoryDisplay, LowerPoint, UpperPoint, LowerDescription,
                  UpperDescription, AssessmentDetails, Data) %>%
    pmap(.f = new, Class = "Characteristic")
  
  ## NEW: Standardize codes/names to prevent pseudo-duplicates
  MetaData <- MetaData %>%
    mutate(
      ParkCode  = toupper(trimws(ParkCode)),
      ShortName = trimws(ShortName),
      LongName  = trimws(LongName),
      SiteCode  = toupper(trimws(SiteCode)),
      SiteName  = trimws(SiteName),
      Type      = trimws(Type)
    )
  
  ## NEW: Build a canonical Parks data frame (ONE row per ParkCode), warn on conflicts
  park_conflicts <- MetaData %>%
    group_by(ParkCode) %>%
    summarize(
      n_short = dplyr::n_distinct(ShortName[!is.na(ShortName) & ShortName != ""]),
      n_long  = dplyr::n_distinct(LongName[!is.na(LongName) & LongName != ""]),
      n_net   = dplyr::n_distinct(Network[!is.na(Network) & Network != ""]),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_short > 1 | n_long > 1 | n_net > 1)
  
  if (nrow(park_conflicts) > 0) {
    warning(
      sprintf("Inconsistent park metadata detected for ParkCode(s): %s; using first non-empty values.",
              paste(park_conflicts$ParkCode, collapse = ", ")),
      call. = FALSE
    )
  }
  
  ParksDf <- MetaData %>%
    group_by(ParkCode) %>%
    summarize(
      Network  = dplyr::first(na.omit(Network)),
      ShortName= dplyr::first(na.omit(ShortName)),
      LongName = dplyr::first(na.omit(LongName)),
      .groups  = "drop"
    )
  
  #### Create Park objects (one per ParkCode)
  Parks <- pmap(.l = ParksDf, .f = new, Class = "Park")
  
  ## NEW: Build canonical Site rows (ONE row per ParkCode + SiteCode), merge & dedupe characteristics
  site_conflicts <- MetaData %>%
    group_by(ParkCode, SiteCode) %>%
    summarize(
      n_name = dplyr::n_distinct(SiteName[!is.na(SiteName) & SiteName != ""]),
      n_type = dplyr::n_distinct(Type[!is.na(Type) & Type != ""]),
      n_lat  = dplyr::n_distinct(Lat[!is.na(Lat) & Lat != ""]),
      n_long = dplyr::n_distinct(Long[!is.na(Long) & Long != ""]),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_name > 1 | n_type > 1 | n_lat > 1 | n_long > 1)
  
  if (nrow(site_conflicts) > 0) {
    warning(
      sprintf("Inconsistent site metadata for %d site(s); using first non-empty values per field.",
              nrow(site_conflicts)),
      call. = FALSE
    )
  }
  
  
  AllSites <- MetaData %>%
    group_by(ParkCode, SiteCode) %>%
    summarize(
      SiteName = dplyr::first(na.omit(SiteName)),
      Lat      = suppressWarnings(as.numeric(dplyr::first(na.omit(Lat)))),
      Long     = suppressWarnings(as.numeric(dplyr::first(na.omit(Long)))),
      Type     = dplyr::first(na.omit(Type)),
      Characteristics = list({
        cs <- unlist(Characteristics, recursive = FALSE)
        if (length(cs) == 0) cs else {
          # Prefer the accessor; fallback to the actual slot name if needed
          char_names <- vapply(
            cs,
            function(c) {
              tryCatch(
                getCharInfo(c, info = "CharName"),
                error = function(e) c@CharacteristicName  # direct slot fallback
              )
            },
            FUN.VALUE = character(1)
          )
          keep <- !duplicated(char_names)
          cs[keep]
        }
      }),
      .groups = "drop"
    )
  
  
  ###### Make a list of sites with each park a nested list
  PSites <- purrr::map(Parks, function(Park) {
    SiteDf <- dplyr::filter(AllSites, ParkCode == Park@ParkCode) %>% dplyr::select(-ParkCode)
    SiteList <- pmap(.l = SiteDf, .f = new, Class = "Site")
    
    ## Name sites by SiteCode and gracefully disambiguate duplicates
    site_names <- SiteDf$SiteCode
    dup_mask <- duplicated(site_names) | duplicated(site_names, fromLast = TRUE)
    if (any(dup_mask)) {
      warning("Duplicate SiteCode values detected within a park; disambiguating site list element names.", call. = FALSE)
      site_suffix <- if ("SiteName" %in% names(SiteDf)) SiteDf$SiteName else seq_along(site_names)
      site_suffix <- tolower(gsub("[^a-z0-9]+", "_", site_suffix))
      site_suffix <- gsub("^_+|_+$", "", site_suffix)
      site_names[dup_mask] <- paste0(site_names[dup_mask], "_", site_suffix[dup_mask])
      if (anyDuplicated(site_names)) site_names <- make.unique(site_names, sep = "_")
    }
    names(SiteList) <- site_names
    SiteList
  })
  
  ### Join Park objects with the sites  
  Parks <- purrr::map2(.x = Parks, .y = PSites, .f = function(x, y) { x@Sites <- y; x })
  
  ## Name parks by ShortName, disambiguate duplicates if needed
  short_names <- vapply(Parks, function(p) p@ParkCode, FUN.VALUE = character(1))
  dup_mask <- duplicated(short_names) | duplicated(short_names, fromLast = TRUE)
  if (any(dup_mask)) {
    warning("Duplicate ParkCode values detected across parks; appending index to ensure uniqueness.", call. = FALSE)
    park_codes <- vapply(Parks, function(p) p@ParkCode, FUN.VALUE = character(1))
    short_names[dup_mask] <- paste0(short_names[dup_mask], "_", park_codes[dup_mask])
    if (anyDuplicated(short_names)) short_names <- make.unique(short_names, sep = "_")
  }
  names(Parks) <- short_names
  
  return(Parks)
}