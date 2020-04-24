#' @title addSite
#' 
#' @description This function adds a site to an existing \code{Park} object. 
#' 
#' @importFrom methods new
#' 
#' @param park A \code{Park} object
#' @param SiteCode A short code to designate the site, stored as a length 1 character \code{vector}. 
#' @param SiteName A name for the site. Stored as a length 1 character \code{vector}. 
#' @param Coordinates A length 2 numeric \code{vector} containing the latitude and longitude of the site in WGS84 coordinates.
#' @param Type The type of water body at the site - e.g. Stream, River, Lake, Ocean etc. Stored as a length 1 character \code{vector}.
#' 
#' @return Returns the park object with the site added.
#' 
#' @details This command can be used to add a \code{Site} object to the list of existing sites in an exisitng \code{Park} object. The \code{SiteCode} will be used to name the Site in the list. \code{addCharacteristic} can be used to add a charactersitic to the site. 
#' 
#' @export
 

 addSite<-function(park,SiteCode,SiteName,Lat,Long,Type){
  XCall<-match.call() #figure out what args user put in.
  XSite<-list(
    do.call("new", 
      c(Class="Site",as.list(XCall)[-1][!names(XCall[-1]) %in% "park"]) #takes arguments from user and passes all but "park" to new()
    )
  )

  park@Sites<-c(park@Sites,XSite)
  return(park)
   
   
 }
 

