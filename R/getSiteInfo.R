#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include getSites.R
#' 
#' @title getSiteInfo
#' 
#' @description Retreives the metadata from a \code{Site} object or a \code{list} of such objects.
#' 
#' @inheritParams getsites
#' @param object Either a \code{Park} object or a \code{site} object or a \code{list} of such objects.
#' @param info Type of infromation to return. One of four options, in quotes.
#' \describe{
#' \item{"code"}{ The default. Returns the site code}
#' \item{"name"}{Returns the name of the site}
#' \item{"coords"}{Returns the latitude and longitude of the site as a length 2 vector}
#' \item{"type"}{Returns the type of the site}
#' } 
#' @return A character vector with one or more park names. 
#' 
#' @export

setGeneric(name="getSiteInfo",function(object,sitecode=NA,info){standardGeneric("getSiteInfo")},signature=c("object") )


setMethod(f="getSiteInfo", signature=c(object="list"),
          function(object,sitecode, info){
            sapply(object,FUN=getSiteInfo, sitecode=sitecode, info=info) } )  


#### Given one park get the sites and run again ####
setMethod(f="getSiteInfo", signature=c(object="Park"),
          function(object,sitecode,info){
            Xsite<-getSites(object=object, sitecode = sitecode)
            sapply(Xsite, FUN=getSiteInfo, info=info)
            

          })


#### Given one Site  get the info ####
setMethod(f="getSiteInfo", signature=c(object="Site"),
          function(object,info){
            switch(info,
                   code = return(object@SiteCode),
                   name = return(object@SiteName),
                   coords = return(object@Coordinates),
                   type = return(object@Type),
                   stop("Unrecognized info in getSiteInfo")
            )
          })