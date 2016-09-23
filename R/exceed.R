#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getCharInfo.R
#' @include getWData.R
#' @include waterseries.R
#' @importFrom purrr pmap
#' 
#' @title exceed
#' 
#' @description Determines if water data contains observtions that exceed a upper and/or lower assessment points. 
#' 
#' @inheritParams getChars
#' @inheritParams getWData
#' @param points What assessemnt points to use when determining if there are exceedences. There are three options
#' \describe{
#' \item{"lower"}{Only assess the data against the lower reference point}
#' \item{"upper"}{Only assess the data against the upper reference point}
#' \item{"both"}{The default. Assess the data against both reference points}
#' }
#' @param lower,upper The lower and uppers assessment points. Either a number specified by the user, or if \code{NA}, the default, the assessment point is determined by the \code{LowerPoint} and \code{UpperPoint} slots in the \code{Characteristic} objects. 
#' 
#' @export

setGeneric(name="exceed",function(object, parkcode=NA, sitecode=NA, charname=NA, points="both", lower=NA, upper=NA,...){standardGeneric("exceed")},signature=c("object") )



setMethod(f="exceed", signature=c(object="NCRNWaterObj"),
  function(object,parkcode, sitecode, charname,points,lower,upper,...){       
    DataUse<-getWData(object,parkcode=parkcode, sitecode=sitecode, charname=charname,output="list",...)
    NotNull<-!sapply(DataUse, is.null)  ### to filter out NULL data
    lower<-if((points=="lower" |points=="both") & is.na(lower)){
      getCharInfo(object,parkcode,sitecode,charname, info='LowerPoint') }else lower
    upper<-if((points=="upper" |points=="both") & is.na(upper)) {
      getCharInfo(object,parkcode,sitecode,charname, info='UpperPoint')} else upper
    
    pmap(.l=list(object=DataUse[NotNull],lower=lower[NotNull],upper=upper[NotNull]), .f=exceed ) %>% 
      bind_rows()


            
})

setMethod(f="exceed", signature=c(object="data.frame"),
          function(object,lower,upper,...){       
            Park<-if(exists("Park",object)) unique(object$Park) else NA
            Site<-if(exists("Site", object)) unique(object$Site) else NA
            Characteristic<-if(exists("Characteristic",object)) unique(object$Characteristic) else NA
            Total<-nrow(object)
            Missing<-sum(is.na(object$Value))
            TooLow<-if(is.na(lower)) NA else sum(object$Value<lower)
            TooHigh<-if(is.na(upper)) NA else sum(object$Value>upper)
            AllExceed<-sum(TooLow,TooHigh, na.rm=T)
            Acceptable=Total-Missing-AllExceed
            OutVec<-data.frame(Park, Site, Characteristic,Total,Acceptable,TooLow,TooHigh, AllExceed, stringsAsFactors=F)
            return(OutVec)
            
          })