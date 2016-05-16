#' @include getWData.R
#' @importFrom magrittr %>% 
#' @importFrom lubridate year month

#' 
#' @title waterbox
#' 
#' @description Produces box plots from water data. 
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param groupby Indicates how the data for the boxplot should be grouped. A text variable in quotes. Choices are:
#' \describe{
#' \item{"year"}{The default. Will prodice a boxplot for each year in the data}
#' \item{"month"}{ Will prodoce a boxplot for each month, combining data across years. That is all January data will be in one box, all February data in another, etc.}
#' }
#' 
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return 
#' 
#' @details   
#' 
#' @export

setGeneric(name="waterbox",function(object,groupby="year",...){standardGeneric("waterbox")},signature=c("object") )
setClassUnion(name="NCRNWaterObj",members=c("Park","Site","Characteristic"))
#            setMethod(f="getWData", signature=c(object="list"),
#                      function(object,sitecode,charname,mindate,maxdate,months,years,wyears,minvalue,maxvalue){
#                        lapply(object,FUN=getWData, sitecode=sitecode, charname=charname, mindate=mindate, maxdate=maxdate,months=months,years=years, wyears=wyears,minvalue=minvalue,maxvalue=maxvalue) %>% 
#                          bind_rows() %>% 
#                          as.data.frame %>% 
#                          return
#                      })  
           
#            #### Given one park get the sites and run again ####
#            setMethod(f="getWData", signature=c(object="Park"),
#                      function(object,sitecode,charname,mindate,maxdate,months,years,wyears,minvalue,maxvalue){
#                        getSites(object, sitecode=sitecode) %>% 
#                          lapply(getWData, charname=charname, mindate=mindate, maxdate=maxdate, months=months,years=years,wyears=wyears,
#                                 minvalue=minvalue,maxvalue=maxvalue) %>% 
#                          bind_rows() %>% 
#                          mutate(Park=getParkInfo(object,info='ParkCode')) %>% 
#                          as.data.frame %>% 
#                          return
#                      })
           
#            
#            #### Given one Site get the characteristics and run again ####
#            setMethod(f="getWData", signature=c(object="Site"),
#                      function(object,charname,mindate,maxdate,months,years,wyears,minvalue,maxvalue){
#                        getChars(object,charname=charname) %>% 
#                          lapply(getWData,mindate=mindate, maxdate=maxdate,months=months,years=years,wyears=wyears,minvalue=minvalue,maxvalue=maxvalue) %>% 
#                          bind_rows() %>%
#                          mutate(Site=getSiteInfo(object, info="SiteCode")) %>% 
#                          as.data.frame %>% 
#                          return
#                      })
#            
           
           #### Given one Characteristic get the data ####
setMethod(f="waterbox", signature=c(object="NCRNWaterObj"),
                     function(object,groupby,...){
                       PlotData<-getWData(object=object,...)
                       waterbox(object=PlotData,groupby=groupby)
})

setMethod(f="waterbox", signature=c(object="data.frame"),
          function(object,groupby,...){
            Grouper<-switch(groupby,
                            year=object$Date %>% year %>% factor,
                            month=object$Date %>% month %>% factor)
            
            ggplot(object,aes(Grouper,Value)) +
              geom_boxplot() %>%
              return
          })


###
# boxplot ideas
# use getWdata to get a data set so do dots.
# response will be Values
# possible gropus are = year, month, site, park
# can add thresholds if desired



       