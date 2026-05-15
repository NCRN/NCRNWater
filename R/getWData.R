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
#' @param wyears A numeric vector corresponding to water years. Only data from those water years will be returned.
#' @param minvalue A number, indicating that only measurements with values greater than or equal to \code{minvalue} should be returned.
#' @param maxvalue A number, indicating that only measurements with values less than or equal to \code{maxvalue} should be returned.
#' @param minobs  An integer indicating the minimum number of observations needed for a dataset to be returned. T
#' his is assessed after the other filters are applied. Defaults to 0.
#' @param output Either "dataframe", the default, or "list". Determines the type of output when data from more than one 
#' characteristic or site are returned. If \code{dataframe} is chosen then a single \code{data.frame} is returned. If \code{list} 
#' is chosen then it returns a \code{list} of \code{data.frames}, where each element is a unique site and characteristic. 
#' 
#' @return A \code{data.frame} or \code{list} based on the \code{Data} slot of \code{Charactersitic} objects. 
#' 
#' @details   This function is intended to be the primary method for accessing water data. Data from various \code{objects} (see below)
#'  is combined into a single \code{data.frame} along with columns indicating which park/site/characteristic the data is from. 
#'  
#'    If \code{object} is a \code{Characteristic} object \code{getWData} will return a \code{data.frame} with columns 
#'    consisting of \code{Date} and \code{Value} from the \code{Data} slot as well as a column named \code{Characteristic}
#'    with the data from the {charname} slot. and a column named \code{Category} iwth data from the \code{category} slot. 
#'    If \code{object} is a \code{Site} object it will return a \code{data.frame} that also includes a column named \code{Site} 
#'    which comes from the \code{SiteCode} slot. If \code{object} is a \code{Park} object then a column named \code{Park} is 
#'    included which comes from the \code{ParkCode} slot.  
#'    
#'    Data can be filtered in a variety of ways. A date range can be selected by using the \code{mindate} and/or \code{maxdate} 
#'    arguments. Dates passed to these arguments must be in text format and the same format as the dates in the data. The \code{months} 
#'    argument will filter for data from one or more months. This argument requires months to be listed as one or more integers,
#'    e.g. \code{months=c(3,4)} will return only data collected in March or April. If \code{NA} is passed to 
#'    \code{months} (the default), or \code{NA} is an element of the vector passed to \code{months} then data from all months
#'     will be returned. The \code{years} argument works in the same way, except that it only returns data from specific years. 
#'     The \code{wyears} argument is the similar to \code{years} except that it filters by "water year". USGS water years start 
#'     on October 1st and end on September 30th, so water year 2010 went from October 1st 2009 through September 30th 2010.
#'    
#'    If an \code{Characteristic} object has an empty data slot, or the filtering options are such that no data meets the criteria,
#'    then the function returns \code{NULL}. \code{NULL} is also returned if after filtering there are fewer observations 
#'    than \code{minobs}. This can be useful when getting data for analysis or graphics and you only wish to use datasets with 
#'    sufficient observations. 
#' 
#' @export

setGeneric(name="getWData",function(object,parkcode=NA,sitecode=NA,charname=NA,category=NA, mindate=NA,maxdate=NA,months=NA,
          years=NA,wyears=NA,minvalue=NA,maxvalue=NA,minobs=1,output="dataframe"){standardGeneric("getWData")},signature=c("object") )


#### Given a list, break it down and feed it back to getWData ####
setMethod(f="getWData", signature=c(object="list"),
  function(object,parkcode,sitecode,charname,category,mindate,maxdate,months,years,wyears,minvalue,maxvalue,minobs,output){
    OutData<- lapply(object,FUN=getWData, parkcode=parkcode, sitecode=sitecode, charname=charname, category=category,
          mindate=mindate, maxdate=maxdate, months=months, years=years, wyears=wyears, minvalue=minvalue, maxvalue=maxvalue,
          minobs=minobs, output=output)
            
    if(output=="list") { return(unlist(OutData, recursive=F)) } else {
      OutData %>% 
      bind_rows %>%
      as.data.frame(stringsAsFactors=F)%>% 
      return
    }
})  

#### Given one park get the sites and run again ####
setMethod(f="getWData", signature=c(object="Park"),
     function(object,parkcode,sitecode,charname,category,mindate,maxdate,months,years,wyears,minvalue,maxvalue,minobs,output){
       
        OutData<-lapply(X=getSites(object=object,parkcode=parkcode,sitecode=sitecode), FUN=function(x){
          getWData(x, sitecode=sitecode, charname=charname, category=category, mindate=mindate, maxdate=maxdate,
                  months=months, years=years, wyears=wyears, minvalue=minvalue, maxvalue=maxvalue, minobs=minobs, output="list")}) %>% 
         unlist(recursive=F) %>% 
         unname

        OutData<-lapply(OutData, FUN=function(x){if(is.null(x)) x else  mutate(x, Park=getParkInfo(object, info="ParkCode")) })      

        if(output=="list") return(OutData) else {
          OutData %>% 
            bind_rows %>%
            as.data.frame(stringsAsFactors=F) %>% 
            return
        }
})


#### Given one Site get the characteristics and run again ####
setMethod(f="getWData", signature=c(object="Site"),
          function(object, charname, category, mindate, maxdate, months, years, wyears, minvalue, maxvalue, minobs, output){
            
            ## --- NEW: validate requested charname(s) when provided ---
            if (!all(is.na(charname))) {
              # Get all available Character objects (respecting category)
              available_chars <- getChars(object = object, charname = NA, category = category)
              
              # Extract the available characteristic names
              available_names <- vapply(
                available_chars,
                FUN = function(x) getCharInfo(x, info = "CharName"),
                FUN.VALUE = character(1)
              ) %>% unique()
              
              # Normalize requested names to a character vector
              requested <- as.character(charname)
              
              # Compute valid/invalid splits
              valid_requested   <- intersect(requested, available_names)
              invalid_requested <- setdiff(requested,  available_names)
              
              # Warn for any invalid requested names
              if (length(invalid_requested) > 0L) {
                warning(
                  sprintf(
                    "The following `charname` value(s) are not valid: %s. Valid names include: %s",
                    paste(invalid_requested, collapse = ", "),
                    paste(available_names, collapse = ", ")
                  ),
                  call. = FALSE
                )
              }
              
              # If none of the requested names are valid, return empty immediately
              if (length(valid_requested) == 0L) {
                if (identical(output, "list")) {
                  return(list())    # empty list for list output
                } else {
                  return(data.frame())  # 0x0 data.frame, as requested
                }
              }
              
              # Use only the valid subset downstream
              charname <- valid_requested
            }
            ## --- END NEW validation block ---
            
            OutData <- getChars(object = object, charname = charname, category = category) %>%
              lapply(., FUN = function(x) {
                getWData(
                  object = x, mindate = mindate, maxdate = maxdate,
                  months = months, years = years, wyears = wyears,
                  minvalue = minvalue, maxvalue = maxvalue, minobs = minobs
                )
              })
            
            OutData <- lapply(OutData, FUN = function(x) {
              if (is.null(x)) x else mutate(x, Site = getSiteInfo(object, info = "SiteCode"))
            })
            
            if (output == "list") {
              return(OutData)
            } else {
              OutData %>%
                bind_rows %>%
                as.data.frame(stringsAsFactors = FALSE) %>%
                return()
            }
          })


#### Given one Characteristic get the data ####
setMethod(f="getWData", signature=c(object="Characteristic"),
  function(object, mindate, maxdate, months, years, wyears, minvalue, maxvalue, minobs, output){
    OutData<-tryCatch({    ### tryCatch is here if the data slot is empty 
      data.frame(c(
        getCharInfo(object,info="Data"), 
        Characteristic= getCharInfo(object,info="CharName"), 
        Category=getCharInfo(object,info="Category")), 
                   stringsAsFactors = FALSE)
     },
    error=function(e){
       return(NULL)
    })
    
    if(is.null(OutData)) return(NULL) #### NULL does not work in filter
    
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
    
    if(output=="list") return(list(OutData)) else return(OutData)
})