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
            
            # Detect filters
            filters_provided <- !all(is.na(c(parkcode, sitecode)))
            
            # Expected scalar type for Site-level info
            .fv <- function(info) {
              if (info %in% c("lat", "long")) return(numeric(1))
              return(character(1))
            }
            
            # Helper: return park-level info for a set of Park objects,
            # replicated by the number of UNIQUE sites under each park (optionally filtered by sitecode).
            .parks_info_by_sitecount <- function(parks, info, sitecode = NA) {
              if (!length(parks)) return(character(0))
              # Dedupe parks by ParkCode
              parks <- parks[vapply(parks, function(x) methods::is(x, "Park"), logical(1))]
              pc <- vapply(parks, function(p) p@ParkCode, character(1))
              parks <- parks[!duplicated(pc)]
              
              unlist(lapply(parks, function(p) {
                # get matching sites under this park
                sites <- getSites(p@Sites, sitecode = sitecode)
                if (is.null(sites)) return(character(0))
                sites <- sites[vapply(sites, function(x) methods::is(x, "Site"), logical(1))]
                sc <- vapply(sites, function(s) s@SiteCode, character(1))
                sites <- sites[!duplicated(sc)]
                k <- length(sites)
                if (k == 0L) return(character(0))
                rep(getParkInfo(p, info = info), times = k)
              }), use.names = FALSE)
            }
            
            # Branch: Park-level infos need Park objects, not Site objects
            if (info %in% c("ParkCode", "ParkShortName", "ParkLongName", "Network")) {
              
              if (!filters_provided) {
                # Global: gather all Park objects from the top-level list and replicate
                parks_list <- object[!vapply(object, is.null, logical(1))]
                # Flatten one level safely
                parks <- unlist(parks_list, recursive = FALSE, use.names = FALSE)
                parks <- parks[vapply(parks, function(x) methods::is(x, "Park"), logical(1))]
                
                out <- .parks_info_by_sitecount(parks, info = info, sitecode = NA)
                return(unname(out))
              }
              
              # Filtered: if a parkcode is provided, use that park
              if (!is.na(parkcode)) {
                parks_list <- lapply(object, function(x) getParks(x, parkcode = parkcode))
                parks_list <- parks_list[!vapply(parks_list, is.null, logical(1))]
                parks <- unlist(parks_list, recursive = FALSE, use.names = FALSE)
                parks <- parks[vapply(parks, function(x) methods::is(x, "Park"), logical(1))]
                
                out <- .parks_info_by_sitecount(parks, info = info, sitecode = sitecode)
                return(unname(out))
              } else if (!is.na(sitecode)) {
                # Only sitecode provided: find the park(s) that contain this site, return single park info
                parks_list <- object[!vapply(object, is.null, logical(1))]
                parks <- unlist(parks_list, recursive = FALSE, use.names = FALSE)
                parks <- parks[vapply(parks, function(x) methods::is(x, "Park"), logical(1))]
                
                # Filter to parks that contain the given site
                parks_with_site <- Filter(function(p) {
                  s <- getSites(p@Sites, sitecode = sitecode)
                  !is.null(s) && length(s) > 0L
                }, parks)
                
                if (!length(parks_with_site)) {
                  stop(sprintf("No Park found containing sitecode '%s'", sitecode))
                }
                
                # Deduplicate by ParkCode; return the requested info ONCE
                pc <- vapply(parks_with_site, function(p) p@ParkCode, character(1))
                parks_with_site <- parks_with_site[!duplicated(pc)]
                return(unname(vapply(parks_with_site, function(p) getParkInfo(p, info = info),
                                     FUN.VALUE = character(1))))
              } else {
                # No filters, but we shouldn't be here; global case handled above.
                return(character(0))
              }
            }
            
            # === Site-level infos ===
            if (!filters_provided) {
              # Global: build from all unique Sites across the list (avoid duplicates)
              all_sites <- getSites(object)
              if (is.null(all_sites)) {
                return(if (info %in% c("lat", "long")) numeric(0) else character(0))
              }
              
              all_sites <- all_sites[vapply(all_sites, function(x) methods::is(x, "Site"), logical(1))]
              sc <- vapply(all_sites, function(s) s@SiteCode, FUN.VALUE = character(1))
              all_sites <- all_sites[!duplicated(sc)]
              
              fv <- .fv(info)
              out <- vapply(all_sites, function(s) getSiteInfo(s, info = info), FUN.VALUE = fv)
              return(unname(out))
            }
            
            # Filtered: subset via getSites(), dedupe by SiteCode, then map
            suppressWarnings({
              object <- getSites(object, parkcode = parkcode, sitecode = sitecode)
            })
            
            if (is.null(object)) {
              return(if (info %in% c("lat", "long")) numeric(0) else character(0))
            }
            
            object <- object[vapply(object, function(x) methods::is(x, "Site"), logical(1))]
            sc <- vapply(object, function(s) s@SiteCode, FUN.VALUE = character(1))
            object <- object[!duplicated(sc)]
            
            if (!is.na(sitecode)) {
              object <- object[vapply(object, function(s) s@SiteCode == sitecode, logical(1))]
              if (length(object) == 0L) {
                stop(sprintf("No Site found for sitecode '%s'", sitecode))
              }
            }
            
            fv <- .fv(info)
            out <- vapply(object, function(s) getSiteInfo(s, info = info), FUN.VALUE = fv)
            return(unname(out))
          }
)


#### Given one park get the sites and run again ####
setMethod(f = "getSiteInfo", signature = c(object = "Park"),
          function(object, parkcode = NA, sitecode = NA, info) {
            sites <- getSites(object = object, parkcode = parkcode, sitecode = sitecode)
            
            # Return empty vector of the correct type if no sites
            if (is.null(sites)) {
              return(if (info %in% c("lat", "long")) numeric(0) else character(0))
            }
            
            # Keep only Site objects and dedupe by SiteCode
            sites <- sites[vapply(sites, function(x) methods::is(x, "Site"), logical(1))]
            sc <- vapply(sites, function(s) s@SiteCode, FUN.VALUE = character(1))
            sites <- sites[!duplicated(sc)]
            
            # Expected scalar type: numeric for lat/long, character otherwise
            fv <- if (info %in% c("lat", "long")) numeric(1) else character(1)
            
            switch(info,
                   ParkCode = ,
                   ParkShortName = ,
                   ParkLongName = ,
                   Network = {
                     pi <- getParkInfo(object, info = info)
                     return(rep(pi, times = length(sites)))
                   },
                   {
                     v <- vapply(sites, function(s) getSiteInfo(s, info = info), FUN.VALUE = fv)
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