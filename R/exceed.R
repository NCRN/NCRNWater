#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getCharInfo.R
#' @include getWData.R
#' @include waterseries.R
#' @importFrom purrr pmap
#' @importFrom dplyr filter group_by summarize
#' 
#' @title exceed
#' 
#' @description Determines if water data contains observations that exceed a upper and/or lower assessment points. 
#' 
#' @inheritParams getChars
#' @inheritParams getWData
#' @param points What assessment points to use when determining if there are exceedances. There are three options
#' \describe{
#' \item{"lower"}{Only assess the data against the lower reference point}
#' \item{"upper"}{Only assess the data against the upper reference point}
#' \item{"both"}{The default. Assess the data against both reference points}
#' }
#' @param lower,upper The lower and uppers assessment points. Either a number specified by the user, or if \code{NA}, the default,
#'  the assessment point is determined by the \code{LowerPoint} and \code{UpperPoint} slots in the \code{Characteristic} objects. 
#' @param all Logical, defaults to \code{FALSE}. Not used when \code{object} is a \code{data.frame}. If \code{all} is \code{FALSE} 
#' characteristics without upper or lower references points will not be included in the results. If \code{all} is \code{TRUE} such characteristics will be included.
#' @param catsum Logical, defaults to \code{False}. When true summarizes by category rather than a characteristic. Not used if 
#' \code{object} is a \code{data.frame}
#' @param ... Additional commands passed to \code{\link{getWData}} for filtering or subsetting the data.
#' 
#' @export

setGeneric(name="exceed",function(object, parkcode=NA, sitecode=NA, charname=NA, category=NA, 
              points="both", lower=NA, upper=NA,all=F, catsum=F,...){standardGeneric("exceed")},signature=c("object") )

setMethod(f="exceed", signature=c(object="NCRNWaterObj"),
  function(object,parkcode, sitecode, charname, category, points,lower,upper,all, ...){       
    DataUse<-getWData(object,parkcode=parkcode, sitecode=sitecode, charname=charname, category=category, output="list",...)
    NotNull<-!sapply(DataUse, is.null)  ### to filter out NULL data
    lower<-if((points=="lower" |points=="both") & is.na(lower)){
      getCharInfo(object=object,parkcode=parkcode,sitecode=sitecode,charname=charname,category=category, info='LowerPoint') }else lower
    upper<-if((points=="upper" |points=="both") & is.na(upper)) {
      getCharInfo(object=object,parkcode=parkcode,sitecode=sitecode,charname=charname,category=category, info='UpperPoint')} else upper
    
    X<-pmap(.l=list(object=DataUse[NotNull],lower=lower[NotNull],upper=upper[NotNull]), .f=exceed ) %>% 
      bind_rows() %>% 
      {if (!all) filter(.,!(is.na(TooLow)&is.na(TooHigh))) else .}
    
    X<-{if(catsum) X %>% group_by(Park, Site,  Category) %>% summarize(Total=sum(Total), Acceptable=sum(Acceptable),
                                      TooLow=sum(TooLow), TooHigh=sum(TooHigh), AllExceed=sum(AllExceed)) else X}
    
    return (X)
            
})

setMethod(f="exceed", signature=c(object="data.frame"),
          function(object,lower,upper,...){       
            Park<-if(exists("Park",object)) unique(object$Park) else NA
            Site<-if(exists("Site", object)) unique(object$Site) else NA
            Characteristic<-if(exists("Characteristic",object)) unique(object$Characteristic) else NA
            Category<-if(exists("Category",object)) unique(object$Category) else NA
            Total<-nrow(object)
            Missing<-sum(is.na(object$Value))
            TooLow<-if(is.na(lower)) NA else sum(object$Value<lower, na.rm=T)
            TooHigh<-if(is.na(upper)) NA else sum(object$Value>upper, na.rm = T)
            AllExceed<-sum(TooLow,TooHigh, na.rm=T)
            Acceptable=Total-Missing-AllExceed
            OutVec<-data.frame(Park, Site, Characteristic,Category, Total,Acceptable,TooLow,TooHigh, AllExceed, stringsAsFactors=F)
            return(OutVec)
            
          })