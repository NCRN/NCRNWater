#' @title addSite
#' 
#' @description This function adds a site to an existing \code{Park} object. 
#' 
#' @param park A \code{Park} object
#' @param SiteCode A short code to designate the site, stored as a length 1 character \code{vector}. 
#' @param SiteName A name for the site. Stored as a length 1 character \code{vector}. 
#' @param Coordinates A length 2 numeric \code{vector} containing the latitude and longitude of the site in WGS84 coordinates.
#' @param Type The type of water body at the site - e.g. Stream, River, Lake, Ocean etc. Stored as a length 1 character \code{vector}.
#' 
#' @return Returns the park object with the site added.
#' 
#' @export
 

 addSite<-function(park,SiteCode,SiteName,Coordinates,Type){
   XSite<-new("Site",
              SiteCode=SiteCode,
              SiteName=SiteName,
              Coordinates=Coordinates,
              Type=Type
       )
   park@Sites<-Xsite
   return(park)
   
   
 }
 

