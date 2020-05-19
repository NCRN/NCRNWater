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
#' 
#' @return  A list of one or more site objects. If \code{parkcode} and/or \code{sitecode} are specified 
#' then only sites which match those codes will be returned. If there are no such sites then the function 
#' will return \code{NULL}
#' 
#' @export

setGeneric(name="getSites",function(object,parkcode=NA, sitecode=NA){standardGeneric("getSites")},signature=c("object") )

setMethod(f="getSites", signature=c(object="list"),
          function(object, parkcode, sitecode){
            OutList<-lapply(object,FUN=getSites, parkcode=parkcode, sitecode=sitecode)
            if(all(sapply(OutList,is.null))) return()
            
            if(any(lapply(OutList,FUN=class)=="list")) return(OutList[!sapply(OutList, is.null)] %>% 
                                                            unlist) else
            return(OutList[!sapply(OutList, is.null)])  
})  

setMethod(f="getSites", signature=c(object="Park"),
          function(object,parkcode,sitecode){
            
            ParkUse<-getParks(object, parkcode=parkcode)
            if (is.null(ParkUse) ) return() else 
              SitesOut<-getSites(ParkUse@Sites, sitecode=sitecode)
              if(all(sapply(SitesOut,is.null))) return() else return(SitesOut)
})

setMethod(f="getSites", signature=c(object="Site"),
          function(object,parkcode, sitecode){
            if(is.na(sitecode) || getSiteInfo(object, info="SiteCode") %in% sitecode ) return(object) else return()
})