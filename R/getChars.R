#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include getParks.R
#' @include getSites.R
#' @importFrom magrittr %>%
#' 
#' @title getChars
#' 
#' @description Retrieves one or more \code{Characteristic} objects from a \code{Site} object or a \code{list} of such objects. The \code{Site} object can be contained within a \code{Park} object.
#' 
#' @param object One of a \code{Characteristic} object, a \code{Site} object, a \code{Park} object or a list of such objects.
#' @param parkcode One or more parkcodes, in quotes.  If \code{object} is a \code{Park} object or a \code{list} of \code{Park} objects, then the \code{parkcode} argument can be used to select which sites should be used.
#' @param sitecode One or more sitecodes, in quotes. If \code{object} is a \code{Park} object or a \code{list} of \code{Park} or \code{Site} objects, then the \code{sitcode} argument can be used to select which sites should be used. 
#' @param charname Name of one or more characteristics, in quotes. Only those characteristics will be returned. 
#' @param category One or more categories of characteristics, in quotes. Only those categories of characteristics will be returned. 

#' @return  A list of one or more \code{Characteristic} objects. 
#' 
#' @export

setGeneric(name="getChars",function(object,parkcode=NA, sitecode=NA,charname=NA, category=NA) {standardGeneric("getChars")},
           signature=c("object") )

setMethod(f="getChars", signature=c(object="list"),
          function(object,parkcode, sitecode, charname, category){
            
            OutList<-lapply(object,getChars, parkcode=parkcode, sitecode=sitecode, charname=charname, category=category)
            if(all(sapply(OutList, is.null))) return() else
              return(OutList[!sapply(OutList, is.null)] %>% unlist)
            
})  


setMethod(f="getChars", signature=c(object="Park"),
          function(object,parkcode,sitecode,charname, category){
            
            ParkUse<-getParks(object, parkcode=parkcode)
            if (is.null(ParkUse) ) return() else {
                 SitesUse<-getSites(ParkUse@Sites, sitecode=sitecode)
                 if(all(sapply(SitesUse, is.null))) return() else
                  return(getChars(SitesUse, sitecode=sitecode, charname=charname, category=category ) %>% unlist)
            }
})


setMethod(f="getChars", signature=c(object="Site"),
          function(object,sitecode,charname, category){
            SiteUse<-getSites(object, sitecode = sitecode)
            if (is.null(SiteUse) ) return() else {
              CharsOut<-getChars(SiteUse@Characteristics, charname=charname, category=category)
              if(all(sapply(CharsOut,is.null))) return() else return(CharsOut)
            }
})

setMethod(f="getChars", signature=c(object="Characteristic"),
          function(object,charname, category){
            if(!anyNA(category) & !getCharInfo(object, info="Category") %in% category) return()
            if(is.na(charname) || getCharInfo(object, info="CharName") %in% charname ) return(object) else return()
})

