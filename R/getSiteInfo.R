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


setMethod(f="getSiteInfo", signature=c(object="list"),
          function(object, parkcode, sitecode, info){
            suppressWarnings( if(!is.na(parkcode)|!is.na(sitecode)) {
              object<-getSites(object, parkcode = parkcode, sitecode = sitecode)
              object<-object[object %>% purrr::map_lgl(~class(.x)=="Site")]
            }
            )
            lapply(object,FUN=getSiteInfo, parkcode=parkcode, sitecode=sitecode, info=info) %>% unname %>% unlist
})  


#### Given one park get the sites and run again ####
setMethod(f="getSiteInfo", signature=c(object="Park"),
    function(object, parkcode, sitecode, info){
      switch(info,
        ParkCode=, ParkShortName=, ParkLongName=, Network =
          return(getParkInfo(object, info=info) %>% 
                   rep(times=getSites(object=object, parkcode=parkcode, sitecode=sitecode) %>% 
                   length)), #info from Park Object
      
        return(sapply(getSites(object=object, parkcode=parkcode, sitecode = sitecode) %>% unname, 
                      FUN=getSiteInfo, info=info)) #default - info from site object
       )
})


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