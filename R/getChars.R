#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @importFrom magrittr %>%
#' 
#' @title getChars
#' 
#' @description Retrieves one or more \code{Characteristic} objects from a \code{Site} object or a \code{list} of such objects. The \code{Site} object can be contained within a \code{Park} object.
#' 
#' @param object One of a \code{Site} object, a \code{Park} object or a list of \code{Site} and/or \code{Park} objects.
#' @param sitecode One of more sitecodes, in quotes. If \code{object} is a \code{Park} object or a \code{list} of \code{Park} objects, then the site code argument can be used to select which sites should be ssued 
#' @param charname Name of one or more characteristics, in quotes.

#' @return  A list of one or more \code{Characteristic} objects. 
#' 
#' @export

setGeneric(name="getChars",function(object,parkcode=NA, sitecode=NA,charname=NA) {standardGeneric("getChars")},signature=c("object") )

setMethod(f="getChars", signature=c(object="list"),
          function(object,parkcode, sitecode, charname){
            
            OutList<-lapply(object,getChars, parkcode=parkcode, sitecode=sitecode, charname=charname)# %>% 
                  #   unlist(recursive=F)
            if(all(sapply(OutList, is.null))) return() else
              return(OutList[!sapply(OutList, is.null)] %>% unlist)
            
})  


setMethod(f="getChars", signature=c(object="Park"),
          function(object,parkcode,sitecode,charname){
            
            ParkUse<-getParks(object, parkcode=parkcode)
            if (is.null(ParkUse) ) return() else 
                 SitesUse<-getSites(ParkUse@Sites, sitecode=sitecode)
                 if(all(sapply(SitesUse, is.null))) return() else
                  return(getChars(SitesUse, sitecode=sitecode, charname=charname ) %>% unlist)
  })


setMethod(f="getChars", signature=c(object="Site"),
          function(object,sitecode,charname){
            SiteUse<-getSites(object, sitecode = sitecode)
            if (is.null(SiteUse) ) return() else 
              CharsOut<-getChars(SiteUse@Characteristics, charname=charname)
            if(all(sapply(CharsOut,is.null))) return() else return(CharsOut)
})

setMethod(f="getChars", signature=c(object="Characteristic"),
          function(object,charname){
            if(is.na(charname) || getCharInfo(object, info="CharName") %in% charname ) return(object) else return()
})

