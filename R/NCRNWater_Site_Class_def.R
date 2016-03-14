#' @title S4 Class Definition for Site object in  NCRNWater
#' 
#' @description
#' An S4 class that contains the data from water monitoring from a single site. Data on indivudiaul characteristics will be stored as 
#' one o more S4 objects in each site object. Site objects are themselves stored as a list within a park object.
#' 
#' @slot SiteCode A short code to designate the site, stored as a length 1 character \code{vector}. 
#' @slot SiteName A name for the site. Stored as a length 1 character \code{vector}. 
#' @slot Coordinates A length 2 numeric \code{vector} containing the latitude and longitude of the site in WGS84 coordinates.
#' @slot Type The type of water body at the site - e.g. Stream, River, Lake, Ocean etc. Stored as a length 1 character \code{vector}.
#' @slot Characteristic A \code{list} of water quality charactertic objects that include the water quality data and associated meta-data.
#' 
#' @exportClass Site

setClass(Class="Site",
         slots=c(SiteCode="character",
                 SiteName="character",
                 Coordinates="numeric",
                 Type="character",
                 Characteristics="list"
          ),
         
         prototype = list(SiteCode=character(),
                          SiteName=character(),
                          coordinates=numeric(),
                          Type=character(),
                          Characteristics=list()
           
         )
)