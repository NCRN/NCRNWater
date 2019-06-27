#' @include NCRNWater_Park_Class_def.R 
#' @include getParks.R
#' @title getParkInfo
#' 
#' @description Retreives park information from an \code{Park} object or a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param parkcode Code of one or more parks, in quotes. Only data from parks whose code is included in parkcode is returned.
#' @param info Type of info to return. One of several, in quotes.
#' \describe{
#' \item{"ParkCode"}{Returns the park code}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park}
#' \item{"ParkLongName"}{Returns the long name of the park}
#' \item{"Network}{Returns the network code}
#' } 
#' @return A character vector with the requested information. 
#' 
#' @export

setGeneric(name="getParkInfo",function(object,parkcode=NA,info="ParkShortName"){standardGeneric("getParkInfo")},signature=c("object") )


setMethod(f="getParkInfo", signature=c(object="list"),
          function(object,parkcode, info){
            OutPark<-sapply(object, FUN=getParkInfo, parkcode=parkcode, info=info)
            return( unlist (OutPark[!sapply(OutPark, is.null) ]))
})  



setMethod(f="getParkInfo", signature=c(object="Park"),
          function(object, parkcode, info){
            Park<-getParks(object, parkcode=parkcode)
            if ( is.null(Park))  return()
            switch(info,
                   ParkCode = return(Park@ParkCode),
                   ParkShortName = return(Park@ShortName),
                   ParkLongName = return(Park@LongName),
                   Network = return(Park@Network),
                   stop("Unrecognized info in getParkInfo")
            )
  })