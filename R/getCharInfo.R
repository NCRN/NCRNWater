#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include getSites.R
#' @importFrom magrittr %>%
#' 
#' @title getCharInfo
#' 
#' @description Retreives the metadata from a \code{Site} object or a \code{list} of such objects.
#' 
#' @inheritParams getChars
#' @param object Either a \code{Characteristic} object or a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param info Type of information to return. One of several options, in quotes.
#' \describe{
#' \item{"CharName"}{The name of the characteristic.}
#' \item{"DisplayName"}{The display name of the characteristic.}
#' \item{"Substrate"}{"The substrate of the characteristic.}
#' \item{"SampleFraction}{SampleFraction The sample fraction of the characteristic. Stored as a length 1 character vector.}
#' \item{"Category"}{The category for the charactersitic. Stored as a length 1 character vector.}
#' \item{"Details"}{A description of the characteristic as needed. Stored as a length 1 character vector.}
#' \item{"Units"}{The units of measurement of the characteristic. Stored as a length 1 character vector.}
#' \item{"Data'}{A \code{data.frame} containing the water quality data. Should have cloumns representing the date, measurment, any QAQC flags etc. for each measurement.}
#' \item{"LowerPoint","UpperPoint}{Indicates the assessment points. Values lower than the lower point are considered to have failed the assessement, whereas values higher than the higher point are considered failures.}
#' \item{"LowerType","UpperType"}{The type of assessment indicated by the lower and upper points.}
#' \item{"LowerDescription","UpperDescription"}{A description of the lowerand upper assesements point.}
#' \item{"AssessmentDetails"}{Additional description of the assessement point.}
#' \item{"SiteCode"}{Returns the site code.}
#' \item{"SiteName"}{Returns the name of the site.}
#' \item{"coords"}{Returns the latitude and longitude of the site as a length 2 vector.}
#' \item{"type"}{Returns the type of the site.}
#' \item{"ParkCode"}{Returns the park code for the park the site is in.}
#' \item{"ParkShortName"}{ The default. Returns the short name of the park the site is in.}
#' \item{"ParkLongName"}{Returns the long name of the park the site is in.}
#' \item{"Network}{Returns the network code for the network the site is in.}
#' } 
#' 
#' @return Either a vector or a list with information for each site. Only the "coords" option returns a list.
#' 
#' @export

setGeneric(name="getCharInfo",function(object,sitecode=NA,charname=NA,info){standardGeneric("getCharInfo")},signature=c("object") )


# setMethod(f="getSiteInfo", signature=c(object="list"),
#           function(object,sitecode, info){
#             lapply(object,FUN=getSiteInfo, sitecode=sitecode, info=info) %>% unname %>% unlist(recursive=F)
# })  
# 
# 
# #### Given one park get the sites and run again ####
# setMethod(f="getSiteInfo", signature=c(object="Park"),
#     function(object,sitecode,info){
#       switch(info,
#         ParkCode=, ParkShortName=, ParkLongName=, Network =
#           return(getParkInfo(object, info=info) %>% rep(times=getSites(object=object, sitecode=sitecode) %>% length)), #info from Park Object
#       
#         return(sapply(getSites(object=object, sitecode = sitecode) %>% unname(), FUN=getSiteInfo, info=info)) #default - info from site object
#        )
# })


#### Given one Characteristic get the info ####
setMethod(f="getCharInfo", signature=c(object="Characteristic"),
          function(object,info){
            switch(info,
                   CharName = return(object@CharacteristicName),
                   DisplayName=return(object@DisplayName),
                   Substrate=return(object@Substrate),
                   SampleFraction=return(object@SampleFraction),
                   Category=return(object@Category),
                   Details=return(object@Details),
                   Units=return(object@Units),
                   Data=return(object@Data),
                   LowerPoint=return(object@LowerPoint),
                   UpperPoint=return(object@UpperPoint),
                   LowerType=return(object@LowerType),
                   UpperType=return(object@UpperType),
                   LowerDescription=return(object@LowerDescription),
                   UpperDescription=return(object@UpperDescription),
                   AssessmentDetails=return(object@AssessmentDetails),
                   SiteCode=,SiteName=,coords=,type= return("No Site object provided, cannot retrieve information"),
                   ParkCode=, ParkShortName=, ParkLongName=, Network = return('No Park object provided, cannot retrieve information'),
                   stop("Unrecognized info in getCharInfo")
            )
})