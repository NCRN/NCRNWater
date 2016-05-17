#' @include getWData.R
#' @importFrom magrittr %>% 
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_boxplot

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
#' \item{"site}{If more than one site is included in the input object, this will produce a different box for each site.}
#' \item{"park"}{If more than one park is included int he input object, theis will prodice a different box for each park}
#' }
#' @param ylab, Text, defaults to \code{NA}. A label for the y-axis. If an \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the y-label will default to the Display Name and Units for the Charecteristic, unless overwitten by the \code{ylab} argument. If a \code{data.frame} is passed then the y-lable will either be the text from \code{ylab} or blank if \code{ylab} is left as \code{NA}.
#' 
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return 
#' 
#' @details   
#' 
#' @export

setGeneric(name="waterbox",function(object,groupby="year",ylab=NA,...){standardGeneric("waterbox")},signature=c("object") )
setClassUnion(name="NCRNWaterObj",members=c("Park","Site","Characteristic"))

setMethod(f="waterbox", signature=c(object="list"),
          function(object, groupby,ylab,...){
            PlotData<-getWData(object=object,...)
            waterbox(object=PlotData, groupby=groupby,ylab=ylab)
})

setMethod(f="waterbox", signature=c(object="NCRNWaterObj"),
                     function(object,groupby,ylab,...){
                       PlotData<-getWData(object=object,...)
                       waterbox(object=PlotData,groupby=groupby,ylab=ylab)
})

setMethod(f="waterbox", signature=c(object="data.frame"),
          function(object,groupby,ylab){
            Grouper<-switch(groupby,
                            year=object$Date %>% year %>% factor,
                            month=object$Date %>% month %>% factor,
                            site=object$Site,
                            park=object$Park)
            if(is.na(ylab)) ylab<-""
            ggplot(object,aes(Grouper,Value)) +
            geom_boxplot() +
              scale_y_continuous(name=ylab) %>% 
            return
})


###
# boxplot ideas
# can add thresholds if desired
#limit data used to sets with x number of observations



       