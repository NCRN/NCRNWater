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
              
              # Choose expected output type for characteristic-level info
              .fv <- function(info) {
                  if (identical(info, "Data")) return(NULL)         # handled separately
                  if (info %in% c("LowerPoint", "UpperPoint")) return(numeric(1))
                  # All other characteristic-level infos are character scalars
                  return(character(1))
              }
              
              # Get deduped Characteristic objects from any list/Park/Site inputs
              chars <- getChars(object, parkcode = parkcode, sitecode = sitecode,
                                charname = charname, category = category)
              
              if (is.null(chars)) {
                  return(if (identical(info, "Data")) list() else
                      if (info %in% c("LowerPoint", "UpperPoint")) numeric(0) else character(0))
              }
              
              # Flatten safely and keep only Characteristic objects
              chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
              chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
              
              if (identical(info, "Data")) {
                  # Return list of data.frames
                  return(lapply(chars, getCharInfo, info = "Data"))
              } else {
                  # Type-stable scalar output per characteristic
                  fv <- .fv(info)
                  out <- vapply(chars, function(ch) getCharInfo(ch, info = info), FUN.VALUE = fv)
                  return(unname(out))
              }
          }
)

#### Given one park get the sites and run again ####
setMethod(f = "getCharInfo", signature = c(object = "Park"),
          function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA, info = NA) {
              if (is.na(info)) stop("Need to specify 'info'")
              
              # Safe scalar extractor: handles NULL, length-0, NA; returns "" for missing
              .safe1 <- function(x) {
                  if (is.null(x) || length(x) == 0L) return("")
                  y <- x[1]
                  if (is.na(y)) return("")
                  as.character(y)
              }
              
              # Stable identity for Characteristic: Name|Category|SampleFraction|Substrate
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
                     
                     #### Park-level info: replicate per UNIQUE Characteristic count
                     ParkCode = ,
                     ParkShortName = ,
                     ParkLongName = ,
                     Network = {
                         chars <- getChars(object = object, parkcode = parkcode, sitecode = sitecode,
                                           charname = charname, category = category)
                         n <- 0L
                         if (!is.null(chars)) {
                             chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
                             # keep only Characteristic objects
                             chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
                             if (length(chars)) {
                                 ids <- vapply(chars, .char_id, character(1))
                                 n <- length(unique(ids))
                             }
                         }
                         pi <- getParkInfo(object, info = info)
                         return(rep(pi, times = n))
                     },
                     
                     #### Site-level info: for each site, replicate per UNIQUE Characteristic count
                     SiteCode = ,
                     SiteName = ,
                     coords  = ,
                     type    = {
                         sites <- getSites(object, parkcode = parkcode, sitecode = sitecode)
                         if (is.null(sites)) return(character(0))
                         # Keep only Site objects and dedupe by SiteCode
                         sites <- sites[vapply(sites, function(x) methods::is(x, "Site"), logical(1))]
                         sc <- vapply(sites, function(s) s@SiteCode, character(1))
                         sites <- sites[!duplicated(sc)]
                         
                         out <- unlist(lapply(sites, function(st) {
                             chs <- getChars(st, charname = charname, category = category)
                             if (is.null(chs)) return(character(0))
                             chs <- unlist(chs, recursive = FALSE, use.names = FALSE)
                             chs <- chs[vapply(chs, function(x) methods::is(x, "Characteristic"), logical(1))]
                             if (!length(chs)) return(character(0))
                             ids <- vapply(chs, .char_id, character(1))
                             k <- length(unique(ids))
                             val <- getSiteInfo(st, info = info)
                             rep(val, times = k)
                         }), use.names = FALSE)
                         
                         return(out)
                     },
                     
                     #### Data: return a list of data.frames (one per UNIQUE Characteristic)
                     Data = {
                         chars <- getChars(object = object, parkcode = parkcode, sitecode = sitecode,
                                           charname = charname, category = category)
                         if (is.null(chars)) return(list())
                         
                         chars <- unlist(chars, recursive = FALSE, use.names = FALSE)
                         chars <- chars[vapply(chars, function(x) methods::is(x, "Characteristic"), logical(1))]
                         if (!length(chars)) return(list())
                         
                         # Deduplicate
                         ids <- vapply(chars, .char_id, character(1))
                         chars <- chars[!duplicated(ids)]
                         
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
                         
                         # Deduplicate
                         ids <- vapply(chars, .char_id, character(1))
                         chars <- chars[!duplicated(ids)]
                         
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
    function(object,sitecode,charname,info){
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