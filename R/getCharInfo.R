#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include getParkInfo.R
#' @include getSites.R
#' @include getSiteInfo.R
#' @include getChars.R
#' @importFrom magrittr %>%
#' 
#' @title getCharInfo
#' 
#' @description Retrieves the metadata from a \code{Site} object or a \code{list} of such objects.
#' 
#' @inheritParams getChars
#' @param object Either a \code{Characteristic} object or a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param info Type of information to return. One of several options, in quotes.
#' \describe{
#' \item{"CharName"}{The name of the characteristic.}
#' \item{"DisplayName"}{The display name of the characteristic.}
#' \item{"Substrate"}{"The substrate of the characteristic.}
#' \item{"SampleFraction}{SampleFraction The sample fraction of the characteristic. Stored as a length 1 character vector.}
#' \item{"Category"}{The category for the charactersitic. Stored as a length 1 character vector.}
#' \item{"CategoryDisplay}{The display name for the category of the characteristic.}
#' \item{"Details"}{A description of the characteristic as needed. Stored as a length 1 character vector.}
#' \item{"Units"}{The units of measurement of the characteristic. Stored as a length 1 character vector.}
#' \item{"Data'}{A \code{data.frame} containing the water quality data. Should have columns representing the date, measurement, any QAQC flags etc. for each measurement.}
#' \item{"LowerPoint","UpperPoint}{Indicates the assessment points. Values lower than the lower point are considered to have failed the assessement, whereas values higher than the higher point are considered failures.}
#' \item{"LowerType","UpperType"}{The type of assessment indicated by the lower and upper points.}
#' \item{"LowerDescription","UpperDescription"}{A description of the lowerand upper assesements point.}
#' \item{"AssessmentDetails"}{Additional description of the assessement point.}
#' \item{"SiteCode"}{Returns the site code.}
#' \item{"SiteName"}{Returns the name of the site.}
#' \item{"coords"}{Returns the latitude and longitude of the site as a length 2 vector.}
#' \item{"type"}{Returns the type of the site.}
#' \item{"ParkCode"}{Returns the park code for the park the site is in.}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park the site is in.}
#' \item{"ParkLongName"}{Returns the long name of the park the site is in.}
#' \item{"Network}{Returns the network code for the network the site is in.}
#' } 
#' 
#' @return Either a vector or a list with information for each character. Only the data option returns a list.
#' 
#' @export

setGeneric(name="getCharInfo",function(object,parkcode=NA, sitecode=NA,charname=NA,category=NA,info=NA){standardGeneric("getCharInfo")},
           signature=c("object") )

setMethod(f = "getCharInfo", signature = c(object = "list"),
          function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA, info = NA) {
            if (is.na(info)) stop("Need to specify 'info'")
            
            # Filters present?
            filters_provided <- !all(is.na(c(parkcode, sitecode, charname, category)))
            
            # Info categories
            is_site_info <- info %in% c("SiteCode", "SiteName", "coords", "type")
            is_park_info <- info %in% c("ParkCode", "ParkShortName", "ParkLongName", "Network")
            is_data      <- identical(info, "Data")
            
            # Helper: get list of Park objects from the top-level list
            parks <- object[!vapply(object, is.null, logical(1))]
            parks <- unlist(parks, recursive = FALSE, use.names = FALSE)
            parks <- parks[vapply(parks, function(x) methods::is(x, "Park"), logical(1))]
            
            if (is_site_info || is_park_info) {
              # Route Site/Park infos to the Park method (not to Characteristic!)
              vals <- lapply(
                parks,
                FUN = getCharInfo,
                parkcode  = parkcode,
                sitecode  = sitecode,
                charname  = charname,
                category  = category,
                info      = info
              )
              vals <- vals[!vapply(vals, is.null, logical(1))]
              # Concatenate atomic outputs
              return(unlist(vals, use.names = FALSE))
            }
            
            # === Characteristic-level infos (including Data) ===
            
            # Conditional dedupe mirroring earlier behavior:
            # - If filters provided: dedupe Characteristics by identity (Name|Category|SampleFraction|Substrate)
            # - If no filters: preserve legacy concatenation (NO dedupe)
            if (is_data) {
              # List of data.frames
              chars <- getChars(object, parkcode = parkcode, sitecode = sitecode, charname = charname, category = category)
              if (is.null(chars)) return(list())
              
              chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
              chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
              
              if (filters_provided) {
                .safe1 <- function(x) { if (is.null(x) || length(x) == 0L) return(""); y <- x[1]; if (is.na(y)) return(""); as.character(y) }
                .char_id <- function(ch) paste(.safe1(ch@CharacteristicName),
                                               .safe1(ch@Category),
                                               .safe1(ch@SampleFraction),
                                               .safe1(ch@Substrate), sep = "|")
                ids <- vapply(chars, .char_id, character(1))
                chars <- chars[!duplicated(ids)]
              }
              
              return(lapply(chars, getCharInfo, info = "Data"))
            } else {
              # Scalar (character or numeric) characteristic-level infos
              # NOTE: If you have numeric characteristic infos (e.g., "LowerPoint"/"UpperPoint"), they are handled
              # via getCharInfo(object="Characteristic"), which returns numeric scalars there.
              
              chars <- getChars(object, parkcode = parkcode, sitecode = sitecode, charname = charname, category = category)
              if (is.null(chars)) return(character(0))
              
              chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
              chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
              
              if (filters_provided) {
                .safe1 <- function(x) { if (is.null(x) || length(x) == 0L) return(""); y <- x[1]; if (is.na(y)) return(""); as.character(y) }
                .char_id <- function(ch) paste(.safe1(ch@CharacteristicName),
                                               .safe1(ch@Category),
                                               .safe1(ch@SampleFraction),
                                               .safe1(ch@Substrate), sep = "|")
                ids <- vapply(chars, .char_id, character(1))
                chars <- chars[!duplicated(ids)]
              }
              
              # Map to scalar outputs
              out <- sapply(chars, function(ch) getCharInfo(ch, info = info))
              return(unname(out))
            }
          }
)



#### Given one park get the sites and run again ####
setMethod(f = "getCharInfo", signature = c(object = "Park"),
          function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA, info = NA) {
              if (is.na(info)) stop("Need to specify 'info'")
              
              filters_provided <- !all(is.na(c(parkcode, sitecode, charname, category)))
              
              # Safe scalar extractor (handles NULL, length-0, NA → "")
              .safe1 <- function(x) {
                  if (is.null(x) || length(x) == 0L) return("")
                  y <- x[1]; if (is.na(y)) return(""); as.character(y)
              }
              # Identity: Name|Category|SampleFraction|Substrate (robust to empties)
              .char_id <- function(ch) {
                  nm  <- .safe1(ch@CharacteristicName)
                  cat <- .safe1(ch@Category)
                  sf  <- .safe1(ch@SampleFraction)
                  sub <- .safe1(ch@Substrate)
                  paste(nm, cat, sf, sub, sep = "|")
              }
              # Expected output type for characteristic-level infos
              .fv <- function(info) {
                  if (identical(info, "Data")) return(NULL)
                  if (info %in% c("LowerPoint", "UpperPoint")) return(numeric(1))
                  return(character(1))
              }
              
              switch(info,
                     
                     #### Park-level info: replicate per Characteristic count
                     ParkCode = ,
                     ParkShortName = ,
                     ParkLongName = ,
                     Network = {
                         chars <- getChars(object = object, parkcode = parkcode, sitecode = sitecode,
                                           charname = charname, category = category)
                         n <- 0L
                         if (!is.null(chars)) {
                             chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
                             chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
                             if (length(chars)) {
                                 if (filters_provided) {
                                     ids <- vapply(chars, .char_id, character(1))
                                     n <- length(unique(ids))
                                 } else {
                                     n <- length(chars)  # original global behavior (no dedupe)
                                 }
                             }
                         }
                         pi <- getParkInfo(object, info = info)
                         return(rep(pi, times = n))
                     },
                     
                     #### Site-level info: replicate per Characteristic count at each Site
                     SiteCode = ,
                     SiteName = ,
                     coords  = ,
                     type    = {
                         sites <- getSites(object, parkcode = parkcode, sitecode = sitecode)
                         if (is.null(sites)) return(character(0))
                         
                         # Conditional dedupe of Sites (dedupe only if filtered)
                         if (filters_provided) {
                             sites <- sites[vapply(sites, function(x) methods::is(x, "Site"), logical(1))]
                             sc <- vapply(sites, function(s) s@SiteCode, character(1))
                             sites <- sites[!duplicated(sc)]
                         }
                         
                         out <- unlist(lapply(sites, function(st) {
                             chs <- getChars(st, charname = charname, category = category)
                             if (is.null(chs)) return(character(0))
                             chs <- unlist(chs, recursive = FALSE, use.names = FALSE)
                             chs <- chs[vapply(chs, function(x) methods::is(x, "Characteristic"), logical(1))]
                             
                             k <- if (filters_provided) {
                                 ids <- if (length(chs)) vapply(chs, .char_id, character(1)) else character(0)
                                 length(unique(ids))
                             } else {
                                 length(chs)  # original global behavior (no dedupe)
                             }
                             
                             val <- getSiteInfo(st, info = info)
                             rep(val, times = k)
                         }), use.names = FALSE)
                         
                         return(out)
                     },
                     
                     #### Data: return list of data.frames (per Characteristic)
                     Data = {
                         chars <- getChars(object = object, parkcode = parkcode, sitecode = sitecode,
                                           charname = charname, category = category)
                         if (is.null(chars)) return(list())
                         
                         chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
                         chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
                         if (!length(chars)) return(list())
                         
                         if (filters_provided) {
                             ids <- vapply(chars, .char_id, character(1))
                             chars <- chars[!duplicated(ids)]
                         }
                         return(lapply(chars, getCharInfo, info = "Data"))
                     },
                     
                     #### Default: characteristic-level info (e.g., LowerDescription, Units, LowerPoint, UpperPoint, etc.)
                     {
                         chars <- getChars(object = object, parkcode = parkcode, sitecode = sitecode,
                                           charname = charname, category = category)
                         if (is.null(chars)) {
                             return(if (identical(info, "Data")) list() else
                                 if (info %in% c("LowerPoint", "UpperPoint")) numeric(0) else character(0))
                         }
                         
                         chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
                         chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
                         if (!length(chars)) {
                             return(if (info %in% c("LowerPoint", "UpperPoint")) numeric(0) else character(0))
                         }
                         
                         if (filters_provided) {
                             ids <- vapply(chars, .char_id, character(1))
                             chars <- chars[!duplicated(ids)]
                         }
                         fv <- .fv(info)
                         if (is.null(fv)) {
                             return(lapply(chars, getCharInfo, info = "Data"))
                         } else {
                             return(vapply(chars, function(ch) getCharInfo(ch, info = info), FUN.VALUE = fv))
                         }
                     }
              )
          }
)


 #### Given one Site get the characteristics and run again ####
 setMethod(f="getCharInfo", signature=c(object="Site"),
    function(object, sitecode = NA, charname = NA, category = NA, info = NA){
      if (is.na(info)) stop("Need to specify 'info'" )
      switch(info,
              SiteCode=,SiteName=,coords=,type= 
        return(getSiteInfo(object, info=info) %>% 
                 rep(times=getChars(object=object, charname=charname, category=category) %>% length)), #info from Site Object
        Data=return(lapply(getChars(object=object,charname=charname, category = category) , FUN=getCharInfo,info=info)), #data returns a list
        return(sapply(getChars(object=object,charname=charname, category = category), FUN=getCharInfo,info=info)) #default-info from Characteristic object
      )
 })


#### Given one Characteristic get the info ####
setMethod(f="getCharInfo", signature=c(object="Characteristic"),
          function(object,info){
            if (is.na(info)) stop("Need to specify 'info'" )
            switch(info,
                   CharName = return(object@CharacteristicName),
                   DisplayName=return(object@DisplayName),
                   Substrate=return(object@Substrate),
                   SampleFraction=return(object@SampleFraction),
                   Category=return(object@Category),
                   CategoryDisplay=return(object@CategoryDisplay), 
                   Details=return(object@Details),
                   Units=return(object@Units),
                   Data=return(object@Data),
                   LowerPoint=return(object@LowerPoint),
                   UpperPoint=return(object@UpperPoint),
                   LowerType=return(object@LowerType),
                   UpperType=return(object@UpperType),
                   LowerDescription=return(object@LowerDescription),
                   UpperDescription=return(object@UpperDescription),
                   AssessmentDetails=return(object@AssessmentDetails),
                   SiteCode=,SiteName=,coords=,type= return("No Site object provided, cannot retrieve information"),
                   ParkCode=, ParkShortName=, ParkLongName=, Network = return('No Park object provided, cannot retrieve information'),
                   stop("Unrecognized info in getCharInfo")
            )
})