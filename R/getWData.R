#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include getSites.R
#' @include getChars.R
#' @include getParkInfo.R
#' @include getSiteInfo.R
#' @include getCharInfo.R
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom lubridate month year mdy interval %within%
#' 
#' @title getWData
#' 
#' @description Gets data from \code{Characteristic} objects and formats it for ease in analysis.
#' 
#' @inheritParams getChars
#' @param object Either a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param mindate The earliest date of data to include, in quotes. Date is in the same format as the \code{Date} in the \code{Data} slot. 
#' @param maxdate The latest date of data to include, in quotes. Date is in the same format as the \code{Date} in the \code{Data} slot. 
#' @param months A numeric vector corresponding to months of the year. Only data from those months will be returned.
#' @param years A numeric vector corresponding to calendar years. Only data from those months will be returned.
#' @param wyears A numeric vector correspoing to the water years. Only data from those water years will be returned.
#' @param minvalue A number, indicating that only measuerments wtih values greater than or equal to \code{minvalue} should be returned.
#' @param maxalue A number, indicating that only measuerments wtih values less than or equal to \code{maxvalue} should be returned.
#' @param minobs  An integer indicating the minimum number of observations needed for a dataset to be returned. This is assessed after the other filters are applied. Defaults to 0.
#' 
#' @return A \code{data.frame} based on the \code{Data} slot of \code{Charactersitic} objects. 
#' 
#' @details   This fuction is intended to be the primary method for accessing water data. Data from various \code{objects} (see below) is comined into a single \code{data.frame} along with comumns indicating which park/site/characteristic the data is from. 
#'  
#'    If \code{object} is a \code{Charactersitic} object \code{getWData} will return a \code{data.frame} with colums consisting of \code{Date} and \code{Value} from the \code{Data} slot as well as a column named \code{Characteristic} with the data from the {charname} slot. If \code{object} is a \code{Site} object it will return a \code{data.frame} that also includes a column named \code{Site} which comes from the \code{SiteCode} slot. If \code{object} is a \code{Park} object then a column named \code{Park} is included which comes from the \code{ParkCode} slot.  
#'    
#'    Data can be filtered in a variety of ways. A date range can be selected by using the \code{mindate} and/or \code{maxdate} arguments. Dates passed to these arguments must be in text format and the same format as the dates in the data. The \code{months} argument will filter for data from one or more months. This argument requires months to be listed as one or more integers, e.g. \code{months=c(3,4)} will return only data collected in March or April. If \code{NA} is passed to \code{months} (the default), or \code{NA} is an element of the vector passed to \code{months} then data from all months will be returned. The \code{years} argument works in the same way, except that it only returns data from specific years. The \code{wyears} argument is the similar to \code{years} except that it filters by "water year". USGS water years start on October 1st and end on September 30th, so water year 2010 went from October 1st 2009 through September 30th 2010.
#'  
#'    
#'    If an \code{Characteristic} object has an empty data slot, or the filtering options are such that no data meets the criteria, then a \code{data.frame} with zero rows in returned. An empty \code{data.frame} is also returned if after filtering there are fewer observatiosn than \code{minobs}. This can be useful when getting data for analysis or graphics and you only wish to use datasets with sufficient observations. 
#' 
#' @export

setGeneric(name="getWData",function(object,sitecode=NA,charname=NA,mindate=NA,maxdate=NA,months=NA,years=NA,wyears=NA,
                                    minvalue=NA,maxvalue=NA,minobs=0){standardGeneric("getWData")},signature=c("object") )


#### Given a list, break it down and feed it back to getWData ####
setMethod(f="getWData", signature=c(object="list"),
          function(object,sitecode,charname,mindate,maxdate,months,years,wyears,minvalue,maxvalue,minobs){
            lapply(object,FUN=getWData, sitecode=sitecode, charname=charname, mindate=mindate, maxdate=maxdate,months=months,years=years, wyears=wyears,minvalue=minvalue,maxvalue=maxvalue,minobs=minobs) %>% 
            bind_rows() %>% 
            as.data.frame %>% 
            return
})  

#### Given one park get the sites and run again ####
setMethod(f="getWData", signature=c(object="Park"),
     function(object,sitecode,charname,mindate,maxdate,months,years,wyears,minvalue,maxvalue,minobs){
        getSites(object, sitecode=sitecode) %>% 
        lapply(getWData, charname=charname, mindate=mindate, maxdate=maxdate, months=months,years=years,wyears=wyears,
               minvalue=minvalue,maxvalue=maxvalue,minobs=minobs) %>% 
        bind_rows() %>% 
        mutate(Park=getParkInfo(object,info='ParkCode')) %>% 
        as.data.frame %>% 
        return
})


#### Given one Site get the characteristics and run again ####
setMethod(f="getWData", signature=c(object="Site"),
     function(object,charname,mindate,maxdate,months,years,wyears,minvalue,maxvalue,minobs){
         getChars(object,charname=charname) %>% 
         lapply(getWData,mindate=mindate, maxdate=maxdate,months=months,years=years,wyears=wyears,minvalue=minvalue,
                maxvalue=maxvalue,minobs=minobs) %>% 
         bind_rows() %>%
         mutate(Site=getSiteInfo(object, info="SiteCode")) %>% 
         as.data.frame %>% 
         return
})


#### Given one Characteristic get the data ####
setMethod(f="getWData", signature=c(object="Characteristic"),
  function(object,mindate,maxdate, months,years,wyears,minvalue,maxvalue,minobs){
     OutData<-tryCatch({    ### tryCatch is here if there data slot is empty 
        data.frame(c(getCharInfo(object,info="Data"), Characteristic= getCharInfo(object,info="CharName")), stringsAsFactors = FALSE)
       },
       error=function(e){
         return(data.frame())
       }
     )
    if(!is.na(mindate)) OutData<-filter(OutData, Date>mindate)
    if(!is.na(maxdate)) OutData<-filter(OutData, Date<maxdate)
    if(all(!is.na(months)))  OutData<-filter(OutData, month(Date) %in% months)
    if(all(!is.na(years)))  OutData<-filter(OutData, year(Date) %in% years)
      
    if(all(!is.na(wyears))){ 
      #need to get data ranges for the water years, allowing for non-consecutive years
      MyIntervals<-lapply(wyears,FUN=function(x) {interval(start=mdy(paste0("10-01-",x-1)), end=mdy(paste0("09-30-",x)) ) })
      OutData<-filter(OutData, outer(OutData$Date, do.call("c", MyIntervals), "%within%") %>% rowSums >0) #filtering here
    } 
    
    if(!is.na(minvalue)) OutData<-filter(OutData, Value>=minvalue)
    if(!is.na(maxvalue)) OutData<-filter(OutData, Value<=maxvalue)
    if(is.null(OutData) || nrow(OutData)<minobs) return(NULL)
    
    
    return(OutData)
})