#' @include NCRNWater_Park_Class_def.R 
#' @title getNParkNames
#' 
#' @description Retreives park names from an \code{Park} object or a \code{list} of such objects.
#' 
#' @param object Either a \code{Park} object or a \code{list} of such objects.
#' @param name.class Type of name to return. One of four options, in quotes.
#' \describe{
#' \item{"short"}{ The default. Returns the short name of the park}
#' \item{"long"}{Returns the long name of the park}
#' \item{"code"}{Returns the park code}
#' \item{"network}{Returns the network code}
#' } 
#' @return A character vector with one or more park names. 
#' 
#' @export

setGeneric(name="getParkNames",function(object,name.class="short"){standardGeneric("getParkNames")},signature=c("object") )


setMethod(f="getParkNames", signature=c(object="list"),
          function(object,name.class){
            sapply(object,FUN=getParkNames, name.class=name.class) } )  


setMethod(f="getParkNames", signature=c(object="Park"),
          function(object,name.class){
            switch(name.class,
                   code = return(object@ParkCode),
                   short = return(object@ShortName),
                   long = return(object@LongName),
                   network = return(object@Network),
                   stop("Unrecognized name.class in getNames")
            )
          })