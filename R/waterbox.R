#' @include getWData.R
#' @importFrom magrittr %>% 
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_y_continuous scale_x_discrete

#' 
#' @title waterbox
#' 
#' @description Produces box plots from water data. 
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param charname Required if \code{object} is not a \code{data.frame}. Name, in quotes, of a single \code{Characteristic} whose data should be graphed.
#' @param groupby Indicates how the data for the boxplot should be grouped. A text variable in quotes. Choices are:
#' \describe{
#' \item{"year"}{The default. Will prodice a boxplot for each year in the data}
#' \item{"month"}{ Will prodoce a boxplot for each month, combining data across years. That is all January data will be in one box, all February data in another, etc.}
#' \item{"site}{If more than one site is included in the input object, this will produce a different box for each site.}
#' \item{"park"}{If more than one park is included int he input object, theis will prodice a different box for each park}
#' }
#' @param yname Text, defaults to \code{NA}. A label for the y-axis. If an \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the y-label will default to the Display Name and Units for the Charecteristic, unless overwitten by the \code{yname} argument. If a \code{data.frame} is passed then the y-label will either be the text from \code{yname} or blank if \code{yname} is left as \code{NA}.
#' @param xname Text, defaults to \code{NA}. A label for the x-axis. If a \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the x-label will default to whatever is indicated in \code{groupby}.unless overwitten by the \code{xname} argument. If a \code{data.frame} is passed then the x-label will either be the text from \code{xname} or blank if \code{xname} is left as \code{NA}.
#' 
#' @param labels A character vector indicating the labels for the bars, defaults to NA. If labels are provided (one for each bar) they will be printed. If \code{object} is a \code{data.frame} and \code{labels} is \code{NA} then no labels will be printed. If \code{object} is a \code{Characteristic}, \code{Site}, or \code{Park} object, and \code{labels} is \code{NA} then the default will depend on the \code{gropby} argument. "year" will be labeled with 4 digit years, "month" with 3 letter months (Jan,Feb, etc), "site" with the site name from the \code{Site}'s \code{Display Name} slot and "park" with the \code{Park}'s short name from the \code{ShortName} slot. 
#' 
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return Creates a boxplot
#' 
#' @details   
#' 
#' @export

setGeneric(name="waterbox",function(object,charname, groupby="year",yname=NA,xname=NA,labels=NA,...){standardGeneric("waterbox")},signature=c("object") )

setClassUnion(name="NCRNWaterObj", members=c("Park","Site","Characteristic","list"))


setMethod(f="waterbox", signature=c(object="NCRNWaterObj"),
  function(object,charname, groupby,yname,xname,labels,...){
   PlotData<-getWData(object=object,charname=charname,...)
   if(is.na(yname)) yname<-paste0(getCharInfo(object=object, charname=charname, info="DisplayName")," (",
                                getCharInfo(object=object, charname=charname, info="Units"),")") %>% unique
   if(is.na(xname)) xname<-switch(groupby,
                          year="Year",
                          month="Month",
                          site=getSiteInfo(object,info="SiteName") %>% unique,
                          park=getParkInfo(object,info="ParkCode") %>% unique
                    )
   
   waterbox(object=PlotData,groupby=groupby,yname=yname,xname=xname,labels=labels)
})

setMethod(f="waterbox", signature=c(object="data.frame"),
          function(object,groupby,yname,xname,labels){
            Grouper<-switch(groupby,
                            year=object$Date %>% year %>% factor,
                            month=object$Date %>% month %>% factor,
                            site=object$Site,
                            park=object$Park)
            if(is.na(yname)) yname<-""
            if(is.na(xname)) xname<-""
            if(all(is.na(labels))) labels<-""
            ggplot(object,aes(Grouper,Value)) +
              geom_boxplot() +
              scale_y_continuous(name=yname)+
              scale_x_discrete(name=xname,labels=labels) %>% 
            return
})


###
# boxplot ideas
# can add thresholds if desired
#limit data used to sets with x number of observations



       