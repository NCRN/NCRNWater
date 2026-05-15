#' @include NCRNWater_Park_Class_def.R
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include diagnose.R
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
    # congruency(file.path(Dir, Data), file.path(Dir, MetaData))
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
  
  ## Build a canonical Parks data frame (ONE row per ParkCode), warn on conflicts
  # Compute distinct values per ParkCode for ShortName, LongName, Network
  park_field_values <- MetaData %>%
    dplyr::group_by(ParkCode) %>%
    dplyr::summarize(
      short_vals = list(unique(ShortName[!is.na(ShortName) & ShortName != ""])),
      long_vals  = list(unique(LongName[!is.na(LongName) & LongName != ""])),
      net_vals   = list(unique(Network[!is.na(Network) & Network != ""])),
      .groups = "drop"
    ) %>%
    # Count distinct values per field
    dplyr::mutate(
      n_short = lengths(short_vals),
      n_long  = lengths(long_vals),
      n_net   = lengths(net_vals)
    )
  
  # Filter parks with any conflicts
  park_conflicts <- park_field_values %>%
    dplyr::filter(n_short > 1 | n_long > 1 | n_net > 1)
  
  if (nrow(park_conflicts) > 0) {
    # Build an actionable message with per-park, per-field values
    msg_lines <- purrr::pmap_chr(
      .l = list(park_conflicts$ParkCode,
                park_conflicts$short_vals,
                park_conflicts$long_vals,
                park_conflicts$net_vals,
                park_conflicts$n_short,
                park_conflicts$n_long,
                park_conflicts$n_net),
      .f = function(pc, svals, lvals, nvals, ns, nl, nn) {
        # Helper: format a vector of values concisely
        fmt_vals <- function(x) {
          x <- sort(unique(x))
          if (length(x) <= 4) {
            paste(x, collapse = '", "')
          } else {
            paste(c(x[1:3], "...", x[length(x)]), collapse = '", "')
          }
        }
        parts <- c(
          if (ns > 1) sprintf('ShortName (%d): "%s"', ns, fmt_vals(svals)),
          if (nl > 1) sprintf('LongName  (%d): "%s"', nl, fmt_vals(lvals)),
          if (nn > 1) sprintf('Network   (%d): "%s"', nn, fmt_vals(nvals))
        )
        sprintf('[%s] Conflicts -> %s', pc, paste(parts[parts != ""], collapse = " | "))
      }
    )
    
    warning(
      paste0(
        "Inconsistent park metadata detected; using first non-empty values per field.\n",
        paste(msg_lines, collapse = "\n")
      ),
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
  
  ## Build canonical Site rows (ONE row per ParkCode + SiteCode), merge & dedupe characteristics
  ## report which site fields differ (SiteName, Type, Lat, Long) for a (ParkCode, SiteCode)
  site_field_values <- MetaData %>%
    dplyr::group_by(ParkCode, SiteCode) %>%
    dplyr::summarize(
      name_vals = list(unique(SiteName[!is.na(SiteName) & SiteName != ""])),
      type_vals = list(unique(Type[!is.na(Type) & Type != ""])),
      lat_vals  = list(unique(Lat[!is.na(Lat)])),
      long_vals = list(unique(Long[!is.na(Long)])),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_name = lengths(name_vals),
      n_type = lengths(type_vals),
      n_lat  = lengths(lat_vals),
      n_long = lengths(long_vals)
    ) %>%
    dplyr::filter(n_name > 1 | n_type > 1 | n_lat > 1 | n_long > 1)
  
  if (nrow(site_field_values) > 0) {
    msg_lines <- purrr::pmap_chr(
      list(site_field_values$ParkCode, site_field_values$SiteCode,
           site_field_values$name_vals, site_field_values$type_vals,
           site_field_values$lat_vals,  site_field_values$long_vals,
           site_field_values$n_name,    site_field_values$n_type,
           site_field_values$n_lat,     site_field_values$n_long),
      function(pc, sc, nvals, tvals, lats, longs, nn, nt, nla, nlo) {
        fmt_vals <- function(x) {
          x <- sort(unique(x))
          if (length(x) <= 4) paste(x, collapse = '", "')
          else paste(c(x[1:3], "...", x[length(x)]), collapse = '", "')
        }
        parts <- c(
          if (nn  > 1) sprintf('SiteName (%d): "%s"', nn,  fmt_vals(nvals)),
          if (nt  > 1) sprintf('Type     (%d): "%s"', nt,  fmt_vals(tvals)),
          if (nla > 1) sprintf('Lat      (%d): "%s"', nla, fmt_vals(lats)),
          if (nlo > 1) sprintf('Long     (%d): "%s"', nlo, fmt_vals(longs))
        )
        sprintf('[%s:%s] Conflicts -> %s', pc, sc, paste(parts[parts != ""], collapse = " | "))
      }
    )
    warning(
      paste0(
        "Inconsistent site metadata detected; using first non-empty values per field.\n",
        paste(msg_lines, collapse = "\n")
      ),
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
        if (length(cs) == 0) {
          cs
        } else {
          # Get characteristic names (prefer accessor, fallback to slot)
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
          
          # De-duplicate by characteristic name
          keep <- !duplicated(char_names)
          cs <- cs[keep]
          char_names <- char_names[keep]
          
          # Disambiguate if duplicates still present (rare, but safe)
          dup_mask <- duplicated(char_names) | duplicated(char_names, fromLast = TRUE)
          if (any(dup_mask)) {
            cat_suffix <- vapply(cs, function(c) c@Category, FUN.VALUE = character(1))
            char_names[dup_mask] <- paste0(char_names[dup_mask], "_", cat_suffix[dup_mask])
            if (anyDuplicated(char_names)) char_names <- make.unique(char_names, sep = "_")
          }
          
          # Name the list elements by characteristic name
          names(cs) <- char_names
          cs
        }
      }),
      .groups = "drop"
    )
  
  
  ###### Make a list of sites with each park a nested list
  PSites <- purrr::map(Parks, function(Park) {
    SiteDf   <- dplyr::filter(AllSites, ParkCode == Park@ParkCode) %>% dplyr::select(-ParkCode)
    SiteList <- pmap(.l = SiteDf, .f = new, Class = "Site")
    
    ## Name sites by SiteCode and gracefully disambiguate duplicates
    site_names <- SiteDf$SiteCode
    dup_mask   <- duplicated(site_names) | duplicated(site_names, fromLast = TRUE)
    if (any(dup_mask)) {
      warning("Duplicate SiteCode values detected within a park; disambiguating site list element names.", call. = FALSE)
      site_suffix <- if ("SiteName" %in% names(SiteDf)) SiteDf$SiteName else seq_along(site_names)
      site_suffix <- tolower(gsub("[^a-z0-9]+", "_", site_suffix))
      site_suffix <- gsub("^_+|_+$", "", site_suffix)
      site_names[dup_mask] <- paste0(site_names[dup_mask], "_", site_suffix[dup_mask])
      if (anyDuplicated(site_names)) site_names <- make.unique(site_names, sep = "_")
    }
    names(SiteList) <- site_names
    
    ## --- BELT & SUSPENDERS: ensure characteristics are named for each site ---
    SiteList <- lapply(SiteList, function(s) {
      cs <- s@Characteristics
      if (length(cs)) {
        # If names are missing or blank, (re)name from the slot; disambiguate duplicates
        if (is.null(names(cs)) || any(!nzchar(names(cs)))) {
          char_names <- vapply(cs, function(c) c@CharacteristicName, FUN.VALUE = character(1))
          
          # Duplicate-safe disambiguation using Category as suffix (then make.unique as final guard)
          dup_mask <- duplicated(char_names) | duplicated(char_names, fromLast = TRUE)
          if (any(dup_mask)) {
            cat_suffix <- vapply(cs, function(c) c@Category, FUN.VALUE = character(1))
            char_names[dup_mask] <- paste0(char_names[dup_mask], "_", cat_suffix[dup_mask])
            if (anyDuplicated(char_names)) char_names <- make.unique(char_names, sep = "_")
          }
          
          names(cs) <- char_names
          s@Characteristics <- cs
        }
      }
      s
    })
    ## --- END BELT & SUSPENDERS ---
    
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
  
  diagnoseWaterData(Parks, verbose_chars = F)
  
  return(Parks)
}