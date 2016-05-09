#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include getSites.R
#' @importFrom magrittr %>%
#' 
#' @title getSiteInfo
#' 
#' @description Retreives the metadata from a \code{Site} object or a \code{list} of such objects.
#' 
#' @inheritParams getSites
#' @param object Either a \code{Park} object or a \code{site} object or a \code{list} of such objects.
#' @param info Type of infromation to return. One of several options, in quotes.
#' \describe{
#' \item{"SiteCode"}{ The default. Returns the site code.}
#' \item{"SiteName"}{Returns the name of the site.}
#' \item{"coords"}{Returns the latitude and longitude of the site as a length 2 vector.}
#' \item{"type"}{Returns the type of the site.}
#'  \item{"ParkCode"}{Returns the park code for the park the site is in.}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park the site is in.}
#' \item{"ParkLongName"}{Returns the long name of the park the site is in.}
#' \item{"Network}{Returns the network code for the network the site is in.}
#' } 
#' 
#' @return Either a vector or a list with information for each site. Only the "coords" option returns a list.
#' 
#' @export

setGeneric(name="getSiteInfo",function(object,sitecode=NA,info){standardGeneric("getSiteInfo")},signature=c("object") )


setMethod(f="getSiteInfo", signature=c(object="list"),
          function(object,sitecode, info){
            lapply(object,FUN=getSiteInfo, sitecode=sitecode, info=info) %>% unname %>% unlist(recursive=F)
})  


#### Given one park get the sites and run again ####
setMethod(f="getSiteInfo", signature=c(object="Park"),
    function(object,sitecode,info){
      switch(info,
        ParkCode=, ParkShortName=, ParkLongName=, Network =
          return(getParkInfo(object, info=info) %>% rep(times=getSites(object=object, sitecode=sitecode) %>% length)), #info from Park Object
      
        return(sapply(getSites(object=object, sitecode = sitecode) %>% unname(), FUN=getSiteInfo, info=info)) #default - info from site object
       )
})


#### Given one Site get the info ####
setMethod(f="getSiteInfo", signature=c(object="Site"),
          function(object,info){
            switch(info,
                   SiteCode = return(object@SiteCode),
                   SiteName = return(object@SiteName),
                   coords = return(object@Coordinates),
                   type = return(object@Type),
                   ParkCode=, ParkShortName=, ParkLongName=, Network = return('No Park object provided, cannot retrieve information'),
                   stop("Unrecognized info in getSiteInfo")
            )
})