#' @include getWData.R
#' @include waterbox.R
#' @importFrom magrittr %>% 
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_gradientn element_rect labs theme_bw theme element_blank 
#' @importFrom plotly ggplotly config

#' 
#' @title waterheat
#' 
#' @description Produces heat maps from water data. 
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param charname Required if \code{object} is not a \code{data.frame}. Name, in quotes, of a single \code{Characteristic} whose data should be graphed.
#' @param by Indicates how the data for the boxplot should be grouped. A text variable in quotes. Choices are:
#' \describe{
#' \item{"year"}{The default. Will prodice a boxplot for each year in the data}
#' \item{"month"}{ Will prodoce a boxplot for each month, combining data across years. That is all January data will be in one box, all February data in another, etc.}
#' \item{"site}{If more than one site is included in the input object, this will produce a different box for each site.}
#' \item{"park"}{If more than one park is included int he input object, theis will prodice a different box for each park}
#' }
#' @param points Vector indicating if assesment points will be maked on the graph. See details below.
#' @param yname Text, defaults to \code{NA}. A label for the y-axis. If an \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the y-label will default to the Display Name and Units for the Charecteristic, unless overwitten by the \code{yname} argument. If a \code{data.frame} is passed then the y-label will either be the text from \code{yname} or blank if \code{yname} is left as \code{NA}.
#' @param xname Text, defaults to \code{NA}. A label for the x-axis. If a \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the x-label will default to whatever is indicated in \code{by}.unless overwitten by the \code{xname} argument. If a \code{data.frame} is passed then the x-label will either be the text from \code{xname} or blank if \code{xname} is left as \code{NA}.
#' 
#' @param labels A character vector indicating the labels for the bars, defaults to NA. If labels are provided (one for each bar) they will be printed. If \code{object} is a \code{data.frame} and \code{labels} is \code{NA} then no labels will be printed. If \code{object} is a \code{Characteristic}, \code{Site}, or \code{Park} object, and \code{labels} is \code{NA} then the default will depend on the \code{gropby} argument. "year" will be labeled with 4 digit years, "month" with 3 letter months (Jan,Feb, etc), "site" with the site name from the \code{Site}'s \code{Display Name} slot and "park" with the \code{Park}'s short name from the \code{ShortName} slot. 
#' @param title A title in the graph in quotes. Defauls to \code{NULL}, which indicates no title should be used. 
#' 
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return Creates a heatmap
#' 
#' @details  The \code{points} argument determines if lines representing the assessment points should be drawn on the graph. If \code{FALSE} then no lines will be drawn. If \code{TRUE}, the default, then the upper and lower points indicated in \code{object}'s \code{Character} objects will be used to draw the lines. Note that if there are multiple assessemnt points, for example if diffrernt parks have different points, or if htere is both an upper and lower point, they will all be drawn. If a \code{vector} of numbers is passed to \code{points} instead then those will serve as the assessment points and lines will be drawn accordingly. Note that if \code{obejct} is a \code{data.frame} then the only way to draw assessment points is by passing a \code{numeric vector} to \code{points}.
#' 
#' @export

setGeneric(name="waterheat",function(object,charname, by="year",points=TRUE,yname=NA,xname=NA,labels=NA,title=NULL,webplot=FALSE,...){standardGeneric("waterheat")},signature=c("object") )


setMethod(f="waterheat", signature=c(object="NCRNWaterObj"),
          function(object,charname,by,points,yname,xname,labels,title,webplot,...){
            PlotData<-getWData(object=object,charname=charname,...)
          #  if(is.na(yname)) yname<-paste0(getCharInfo(object=object, charname=charname, info="DisplayName")," (",
          #                                 getCharInfo(object=object, charname=charname, info="Units"),")") %>% unique
          #  if(is.na(xname)) xname<-switch(by,
            #                                year="Year",
            #                                month="Month",
            #                                site="Site",
            #                                park="Park"
            # )
            
            if(points) points<-c(getCharInfo(object=object,charname=charname, 
                                             info="LowerPoint"),getCharInfo(object=object,charname=charname, info="UpperPoint")) %>% unlist %>% unique
            waterheat(object=PlotData,by=by,points=points,yname=yname,xname=xname,labels=labels,title=title,webplot=webplot)
          })

setMethod(f="waterheat", signature=c(object="data.frame"),
          function(object,by,points,yname,xname,labels,title,webplot){
            Grouper<-switch(by,
                            year=object$Date %>% year %>% factor,
                            month=object$Date %>% month(label=T) %>% factor,
                            site=object$Site,
                            park=object$Park)
            if(is.na(yname)) yname<-""
            if(all(is.na(xname))) xname<-""
            if(all(is.na(labels))) labels<-switch(by,
                                                  year=object$Date %>% year %>% unique,
                                                  month=object$Date %>% month(label=T) %>% unique %>% sort %>% as.character,
                                                  site=object$Site %>% unique,
                                                  park=object$Park %>% unique)
            object<-object[order(object$Date),]
            
            
            ### make new dataframe for heat map.
            HeatData=data.frame
            HeatData$Horiz<--seq(from=min(object$Date), to=max(object$Date), by="month") %>% format("%b-%y") %>% factor(levels(unique(.)))
            
            Vert<-switch(by,
                         month=object$Date %>% month(T) %>% factor(levels=rev(levels(.))),
                          site=object$Site
            )
            Horiz<-switch(by,
                          month=object$Date %>% year %>% factor,
                          site=object$Date %>%  format("%b-%y") %>% factor(levels=unique(.))
             )
            if(is.na(xname)) xname<-switch(by,
                          month="Year",
                          site="Date")
            
            if(is.na(yname)) yname<-switch(by,
                                           month="Month",
                                           site="Site")
           # return(list(Horiz=Horiz, Vert=Vert, fill=object$Value))
            OutPlot<-ggplot(object,aes(x=Horiz,y=Vert,fill=Value)) +
              geom_raster() +
              scale_fill_gradientn(colors=c("blue","white","red")) +
               labs(title=title,y=yname,x=xname)+
               theme_bw()+
              theme(panel.grid = element_blank(), panel.background=element_rect(fill="grey",color="grey"))
            
            ifelse(webplot, return(ggplotly(OutPlot) %>% plotly::config(displaylogo=F)),return(OutPlot))
})




