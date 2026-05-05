#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include getSites.R
#' @include getParkInfo.R
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' 
#' @title getSiteInfo
#' 
#' @description Retrieves the metadata from a \code{Site} object or a \code{list} of such objects.
#' 
#' @inheritParams getSites
#' @param object Either a \code{Park} object or a \code{site} object or a \code{list} of such objects.
#' @param info Type of information to return. One of several options, in quotes.
#' \describe{
#' \item{"SiteCode"}{The default. Returns the site code.}
#' \item{"SiteName"}{Returns the name of the site.}
#' \item{"lat"}{Returns the latitude of the site.}
#' \item{"long}{Rerturns the longitude of the site.}
#' \item{"type"}{Returns the type of the site.}
#' \item{"ParkCode"}{Returns the park code for the park the site is in.}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park the site is in.}
#' \item{"ParkLongName"}{Returns the long name of the park the site is in.}
#' \item{"Network}{Returns the network code for the network the site is in.}
#' } 
#' 
#' @return A vector of information for each site. 
#' 
#' @details   If \code{object} is a \code{list} or a \code{Park} object, then the list is filtered using both the \code{parkcode} and \code{sitecode} arguments. If \code{object} is a \code{Site} then both \code{parkcode} and \code{sitecode} are ignored.
#'  
#' @export

setGeneric(name="getSiteInfo",function(object,parkcode=NA,sitecode=NA,info){standardGeneric("getSiteInfo")},signature=c("object") )


setMethod(f = "getSiteInfo", signature = c(object = "list"),
          function(object, parkcode = NA, sitecode = NA, info) {
            
            # Detect whether filters are provided
            filters_provided <- !all(is.na(c(parkcode, sitecode)))
            
            # Expected scalar type for Site-level info
            .fv <- function(info) {
              if (info %in% c("lat", "long")) return(numeric(1))
              return(character(1))
            }
            
            if (!filters_provided) {
              # === Global behavior (original semantics): DO NOT subset with getSites() here.
              # Iterate over each element (Park objects), call getSiteInfo(Park, ...) and concatenate
              vals <- lapply(object, FUN = getSiteInfo, parkcode = parkcode, sitecode = sitecode, info = info)
              # Remove NULLs; then unlist. This preserves numeric type for lat/long and character otherwise.
              vals <- vals[!vapply(vals, is.null, logical(1))]
              if (!length(vals)) {
                # Empty, match original return type: numeric(0) for lat/long, else character(0)
                return(if (info %in% c("lat", "long")) numeric(0) else character(0))
              }
              return(unlist(vals, use.names = FALSE))
            }
            
            # === Filtered behavior: subset with getSites(), keep Site objects, dedupe by SiteCode
            suppressWarnings({
              object <- getSites(object, parkcode = parkcode, sitecode = sitecode)
            })
            
            if (is.null(object)) {
              return(if (info %in% c("lat", "long")) numeric(0) else character(0))
            }
            
            # Keep only Site objects
            object <- object[vapply(object, function(x) methods::is(x, "Site"), logical(1))]
            
            # Deduplicate by SiteCode
            sc <- vapply(object, function(s) s@SiteCode, FUN.VALUE = character(1))
            object <- object[!duplicated(sc)]
            
            # If a specific sitecode was requested, enforce exact match
            if (!is.na(sitecode)) {
              object <- object[vapply(object, function(s) s@SiteCode == sitecode, logical(1))]
              if (length(object) == 0L) {
                stop(sprintf("No Site found for sitecode '%s'", sitecode))
              }
            }
            
            # Type-stable mapping
            fv <- .fv(info)
            out <- vapply(object, function(s) getSiteInfo(s, info = info), FUN.VALUE = fv)
            return(unname(out))
          }
)

#### Given one park get the sites and run again ####
setMethod(f = "getSiteInfo", signature = c(object = "Park"),
          function(object, parkcode = NA, sitecode = NA, info) {
              sites <- getSites(object = object, parkcode = parkcode, sitecode = sitecode)
              
              if (is.null(sites)) {
                  return(character(0))
              }
              
              # Keep only Site objects and dedupe
              sites <- sites[vapply(sites, function(x) methods::is(x, "Site"), logical(1))]
              sc <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
              sites <- sites[!duplicated(sc)]
              
              switch(info,
                     ParkCode = ,
                     ParkShortName = ,
                     ParkLongName = ,
                     Network = {
                         pi <- getParkInfo(object, info = info)
                         return(rep(pi, times = length(sites)))
                     },
                     {
                         v <- vapply(sites, function(s) getSiteInfo(s, info = info), FUN.VALUE = character(1))
                         return(unname(v))
                     }
              )
          }
)



#### Given one Site get the info ####
setMethod(f="getSiteInfo", signature=c(object="Site"),
          function(object,info){
            switch(info,
                   SiteCode = return(object@SiteCode),
                   SiteName = return(object@SiteName),
                   lat = return(object@Lat),
                   long = return(object@Long),
                   type = return(object@Type),
                   ParkCode=, ParkShortName=, ParkLongName=, Network = return('No Park object provided, cannot retrieve information'),
                   stop("Unrecognized info in getSiteInfo")
            )
})