#' @include NCRNWater_Park_Class_def.R 
#' @title getParkInfo
#' 
#' @description Retreives park information from an \code{Park} object or a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param info Type of info to return. One of several, in quotes.
#' \describe{
#' \item{"ParkCode"}{Returns the park code}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park}
#' \item{"ParkLongName"}{Returns the long name of the park}
#' \item{"Network}{Returns the network code}
#' } 
#' @return A character vector with one or more park names. 
#' 
#' @export

setGeneric(name="getParkInfo",function(object,info="ShortName"){standardGeneric("getParkInfo")},signature=c("object") )


setMethod(f="getParkInfo", signature=c(object="list"),
          function(object,info){
            sapply(object,FUN=getParkInfo, info=info) } )  


setMethod(f="getParkInfo", signature=c(object="Park"),
          function(object,info){
            switch(info,
                   ParkCode = return(object@ParkCode),
                   ParkShortName = return(object@ShortName),
                   ParkLongName = return(object@LongName),
                   Network = return(object@Network),
                   stop("Unrecognized info in getParkInfo")
            )
          })