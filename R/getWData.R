#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include getSites.R
#' @include getChars.R
#' @include getParkInfo.R
#' @include getSiteInfo.R
#' @include getCharInfo.R
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate
#' 
#' @title getWData
#' 
#' @description Gets data from \code{Characteristic} objects and formats it for ease in analysis.
#' 
#' @inheritParams getChars
#' @param object Either a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param mindate The earliest date of data to include, in quotes. Date is in the same format as the \code{Date} in the \code{Data} slot. 
#' @param maxdate The latest date of data to include, in quotes. Date is in the same format as the \code{Date} in the \code{Data} slot. 

#' 
#' @return A \code{data.frame} based on the \code{Data} slot of \code{Charactersitic} objects. 
#' 
#' @details   This fuction is intended to be the primary method for accessing water data. Data from various \code{objects} (see below) is comined into a single \code{data.frame} along with comumns indicating which park/site/characteristic the data is from. 
#'  
#'    If \code{object} is a \code{Charactersitic} object \code{getWData} will return a \code{data.frame} with colums consisting of \code{Date} and \code{Value} from the \code{Data} slot as well as a column named \code{Characteristic} with the data from the {charname} slot. If \code{object} is a \code{Site} object it will return a \code{data.frame} that also includes a column named \code{Site} which comes from the \code{SiteCode} slot. If \code{object} is a \code{Park} object then a column named \code{Park} is included which comes from the \code{ParkCode} slot.  
#' 
#' @export

setGeneric(name="getWData",function(object,sitecode=NA,charname=NA,mindate=NA,maxdate=NA){standardGeneric("getWData")},signature=c("object") )


#### Given a list, break it down and feed it back to getWData ####
setMethod(f="getWData", signature=c(object="list"),
          function(object,sitecode,charname,mindate,maxdate){
            lapply(object,FUN=getWData, sitecode=sitecode, charname=charname, mindate=mindate, maxdate=maxdate) %>% 
              bind_rows() %>% as.data.frame %>% return
})  

#### Given one park get the sites and run again ####
setMethod(f="getWData", signature=c(object="Park"),
     function(object,sitecode,charname,mindate,maxdate){
        getSites(object, sitecode=sitecode) %>% lapply(getWData, charname=charname, mindate=mindate, maxdate=maxdate) %>% 
        bind_rows() %>% 
        mutate(Park=getParkInfo(object,info='ParkCode')) %>% as.data.frame() %>% return
})


#### Given one Site get the characteristics and run again ####
setMethod(f="getWData", signature=c(object="Site"),
     function(object,charname,mindate,maxdate){
         getChars(object,charname=charname) %>% lapply(getWData,mindate=mindate, maxdate=maxdate) %>% bind_rows() %>%
         mutate(Site=getSiteInfo(object, info="SiteCode")) %>% 
         as.data.frame %>% return
})


#### Given one Characteristic get the data ####
setMethod(f="getWData", signature=c(object="Characteristic"),
  function(object,mindate,maxdate){
      OutData<-data.frame(c(getCharInfo(object,info="Data"), Characteristic= getCharInfo(object,info="CharName")), stringsAsFactors = FALSE) 
      
      if(!is.na(mindate)) OutData<-filter(OutData, Date>mindate)
      if(!is.na(maxdate)) OutData<-filter(OutData, Date<maxdate)
      
      
      ### To do: months, years, min and max values
      
      return(OutData)
})