#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @importFrom magrittr %>%
#' @title getSites
#' 
#' @description Retrieves sites from a \code{Park} object or a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param sitecode Name of one or more sites, in quotes.

#' @return  A list of one or more site objects. 
#' 
#' @export

setGeneric(name="getSites",function(object,sitecode=NA){standardGeneric("getSites")},signature=c("object") )


setMethod(f="getSites", signature=c(object="list"),
          function(object,sitecode){
            OutList<-lapply(object,FUN=getSites, sitecode) %>% unname
            return(OutList[!sapply(OutList, is.null)] ) %>% unlist(recursive=FALSE)
})  
            


setMethod(f="getSites", signature=c(object="Park"),
          function(object,sitecode){
            OutSites<-names(object@Sites) %in% sitecode
            
            SiteOut<-if(all(is.na(sitecode))) return(object@Sites) else {
              if(all(!OutSites)) return() else  {
                return(object@Sites[OutSites]) 
              }  
            }
})