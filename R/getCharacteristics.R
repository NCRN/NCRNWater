#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @title getCharacteristics
#' 
#' @description Retrieves one or more \code{Characteristic} objects from a \code{Site} object or a \code{list} of such objects. The \code{Site} object can be contained within a \code{Park} object.
#' 
#' @param object One of a \code{Site} object, a \code{Park} object or a list of \code{Site} and/or \code{Park} objects.
#' @param sitecode One of more sitecodes, in quotes. If \code{object} is a \code{Park} object or a \code{list} of \code{Park} objects, then the site code argument can be used to select which sites should be ssued 
#' @param charname Name of one or more characteristics, in quotes.

#' @return  A list of one or more characteristic objects. 
#' 
#' @export

setGeneric(name="getCharacteristics",function(object,sitecode=NA,charname=NA) {standardGeneric("getCharacteristics")},signature=c("object") )

setMethod(f="getCharacteristics", signature=c(object="list"),
          function(object,sitecode,charname){
            
            return(lapply(object,getCharacteristics,sitecode=sitecode, charname=charname) %>% 
                     unname %>% 
                     unlist(recursive=F)
            )
})  


setMethod(f="getCharacteristics", signature=c(object="Park"),
          function(object,sitecode,charname){
           
            SiteList<-getSites(object=object, sitecode = sitecode) 
            getCharacteristics(object=SiteList, sitecode=sitecode,charname=charname)
})


setMethod(f="getCharacteristics", signature=c(object="Site"),
          function(object,charname){
            
            Chars<-names(object@Characteristics) %in% charname
            
            CharOut<-if(all(is.na(charname))) return(object@Characteristics) else {
              if(all(!Chars)) return() else {
                return(object@Characteristics[Chars]) 
              }  
            }
})



