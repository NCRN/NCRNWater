#' @include NCRNWater_Park_Class_def.R 
#' @include getParks.R
#' @title getParkInfo
#' 
#' @description Retrieves park information from an \code{Park} object or a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param parkcode Code of one or more parks, in quotes. Only data from parks whose code is included in parkcode is returned.
#' @param info Type of info to return. One of several, in quotes.
#' \describe{
#' \item{"ParkCode"}{Returns the park code}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park}
#' \item{"ParkLongName"}{Returns the long name of the park}
#' \item{"Network}{Returns the network code}
#' } 
#' @return A character vector with the requested information. 
#' 
#' @export

setGeneric(
    name = "getParkInfo",
    function(object, parkcode = NA, info = "ParkShortName") {
        standardGeneric("getParkInfo")
    },
    signature = c("object")
)

# list method: safe flatten + dedupe by ParkCode
setMethod(f = "getParkInfo", signature = c(object = "list"),
          function(object, parkcode = NA, info = "ParkShortName") {
              
              # If a parkcode is provided, try to subset each element with getParks;
              # otherwise, keep the element as-is.
              parks_list <- lapply(object, function(x) {
                  if (!is.na(parkcode)) getParks(x, parkcode = parkcode) else x
              })
              
              # Drop NULLs
              parks_list <- parks_list[!vapply(parks_list, is.null, logical(1))]
              
              # Flatten ONE level (avoid atomic coercion)
              flat <- unlist(parks_list, recursive = FALSE, use.names = FALSE)
              
              # Keep only Park objects
              flat <- flat[vapply(flat, function(x) methods::is(x, "Park"), logical(1))]
              
              # If nothing left, return empty vector
              if (!length(flat)) return(character(0))
              
              # Deduplicate by ParkCode (fixes both unfiltered and filtered duplication)
              pc <- vapply(flat, function(p) p@ParkCode, character(1))
              flat <- flat[!duplicated(pc)]
              
              # Map to requested info (type-stable)
              out <- switch(info,
                            ParkCode      = vapply(flat, function(p) p@ParkCode,   FUN.VALUE = character(1)),
                            ParkShortName = vapply(flat, function(p) p@ShortName,  FUN.VALUE = character(1)),
                            ParkLongName  = vapply(flat, function(p) p@LongName,   FUN.VALUE = character(1)),
                            Network       = vapply(flat, function(p) p@Network,    FUN.VALUE = character(1)),
                            stop("Unrecognized info in getParkInfo")
              )
              
              return(unname(out))
          }
)

# Park method: unchanged; returns scalar from a single Park
setMethod(f = "getParkInfo", signature = c(object = "Park"),
          function(object, parkcode = NA, info = "ParkShortName") {
              Park <- getParks(object, parkcode = parkcode)
              if (is.null(Park)) return()
              switch(info,
                     ParkCode      = return(Park@ParkCode),
                     ParkShortName = return(Park@ShortName),
                     ParkLongName  = return(Park@LongName),
                     Network       = return(Park@Network),
                     stop("Unrecognized info in getParkInfo")
              )
          }
)