#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include getParks.R
#' @importFrom magrittr %>%
#' @title getSites
#' 
#' @description Retrieves sites from a \code{Park} object or a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param parkcode Park code of one or more parks, in quotes.
#' @param sitecode Site code of one or more sites, in quotes.
#' @param type Type of site, such as Lake or Stream, in quotes.
#' 
#' @return  A list of one or more site objects. If \code{parkcode} and/or \code{sitecode} are specified 
#' then only sites which match those codes will be returned. If there are no such sites then the function 
#' will return \code{NULL}
#' 
#' @export

setGeneric(name="getSites",function(object,parkcode=NA, sitecode=NA, type=NA){standardGeneric("getSites")},signature=c("object") )

setMethod(f = "getSites", signature = c(object = "list"),
          function(object, parkcode = NA, sitecode = NA, type = NA) {
              # Collect per-element results
              OutList <- lapply(object, FUN = getSites, parkcode = parkcode, sitecode = sitecode, type = type)
              
              # If all are NULL, warn and return NULL
              if (all(vapply(OutList, is.null, logical(1)))) {
                  warning("No sites match these criteria.")
                  return(NULL)
              }
              
              # Drop NULLs
              kept <- OutList[!vapply(OutList, is.null, logical(1))]
              
              # Detect whether elements are lists (lists-of-Site) or scalar Site objects
              is_list_elem <- vapply(kept, is.list, logical(1))
              
              # Flatten one level only; preserve S4 objects (no atomic coercion)
              flat <- if (any(is_list_elem)) {
                  unlist(kept, recursive = FALSE, use.names = FALSE)
              } else {
                  kept
              }
              
              # Keep only Site objects
              flat <- flat[vapply(flat, function(x) methods::is(x, "Site"), logical(1))]
              
              # Apply filters again here (guard against upstream oddities)
              if (!is.na(sitecode)) {
                  flat <- flat[vapply(flat, function(s) getSiteInfo(s, info = "SiteCode") %in% sitecode, logical(1))]
              }
              if (!is.na(type)) {
                  flat <- flat[vapply(flat, function(s) getSiteInfo(s, info = "type") %in% type, logical(1))]
              }
              
              # **Deduplicate by SiteCode** to enforce identity uniqueness
              sc <- vapply(flat, function(s) s@SiteCode, FUN.VALUE = character(1))
              flat <- flat[!duplicated(sc)]
              
              if (length(flat) == 0L) return(NULL)
              return(flat)
          }
)


setMethod(f="getSites", signature=c(object="Park"),
          function(object,parkcode,sitecode, type){
            ParkUse<-getParks(object, parkcode=parkcode)
            if (is.null(ParkUse)) return() else 
            SitesOut<-getSites(ParkUse@Sites, sitecode=sitecode, type=type)
            if(all(sapply(SitesOut,is.null))) return() else return(SitesOut)
})

setMethod(f = "getSites", signature = c(object = "Site"),
          function(object, sitecode = NA, type = NA) {
              if (!is.na(sitecode) && !(getSiteInfo(object, info = "SiteCode") %in% sitecode)) {
                  return(NULL)
              }
              if (!is.na(type) && !(getSiteInfo(object, info = "type") %in% type)) {
                  return(NULL)
              }
              return(object)
          }
)








