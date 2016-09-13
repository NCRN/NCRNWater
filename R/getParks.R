#' @include NCRNWater_Park_Class_def.R 
#' @title getParks
#' 
#' @description Retrieves one or more parks from a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param parkcode Park code of one or more parks, in quotes.
#' @return  A list of one or more park objects. If \code{parkcode} is specified, only parks with those park codes will be returned. If no parks are matched by \code{parkcode} then the funciton will return \code{NULL}
#' 
#' @export

setGeneric(name="getParks",function(object,parkcode=NA){standardGeneric("getParks")},signature=c("object") )


setMethod(f="getParks", signature=c(object="list"),
          function(object,parkcode){
            OutList<-lapply(object ,FUN=getParks, parkcode=parkcode)
            if(all(sapply(OutList,is.null))) return()
            return(OutList[!sapply(OutList, is.null)] )#%>% as.vector ) 
})  



setMethod(f="getParks", signature=c(object="Park"),
          function(object,parkcode){
          if(is.na(parkcode) || getParkInfo(object, info="ParkCode") %in% parkcode) return(object) else return()
})