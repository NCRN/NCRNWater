#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getWData.R
#' @importFrom magrittr %>% 
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_point labs theme_bw theme element_blank geom_hline geom_line scale_color_discrete scale_shape_manual scale_x_continuous
#' @importFrom plotly ggplotly config

#' 
#' @title waterseries
#' 
#' @description Produces a time series plot from water data. 
#' 
#' @inheritParams getChars
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object, a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' 
#' @param charname Required if \code{object} is not a \code{data.frame}. Name(s), in quotes, of one or more \code{Characteristic}s whose data should be graphed.
#' @param by Indicates how the data for the plot should be grouped. A text variable in quotes. Choices are:
#' \describe{
#' \item{"site}{If more than one site is included in the input object, this will produce a different line for each site.}
#' \item{"park"}{If more than one park is included in the input object, this will produce a different line for each park}
#' \item{"char"}{If more than one characteristic is included in the input object, this will produce a different line for each one} 
#' \item{"none}{All data will be displayed as a single line. Used if you only have data from a single charactersitic from a single site}
#' }
#' 
#' @param assessment Vector indicating if assesment lines will be marked on the graph. See details below.
#' @param layers Defaults to c("points","line") Indicates which layers you wish to see on the plot. 
#' @param yname Text, defaults to \code{NA}. A label for the y-axis. If an \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the y-label will default to the Display Name and Units for the Charecteristic, unless overwitten by the \code{yname} argument. If a \code{data.frame} is passed then the y-label will either be the text from \code{yname} or blank if \code{yname} is left as \code{NA}.
#' @param xname Text, defaults to \code{NA}. A label for the x-axis. If a \code{Characteristic}, \code{Site}, or \code{Park} object is passed to \code{object}, then the x-label will default to whatever is indicated in \code{by}.unless overwitten by the \code{xname} argument. If a \code{data.frame} is passed then the x-label will either be the text from \code{xname} or blank if \code{xname} is left as \code{NA}.
#' 
#' @param labels A character vector indicating the labels for the data series, defaults to NA. If labels are provided (one for each series) they will be printed. If \code{object} is a \code{data.frame} and \code{labels} is \code{NA} then no labels will be printed. If \code{object} is a \code{Characteristic}, \code{Site}, or \code{Park} object, and \code{labels} is \code{NA} then the default will depend on the \code{by} argument. "site" willbe labled with the site name from the \code{Site}'s \code{Display Name} slot and "park" with the \code{Park}'s short name from the \code{ShortName} slot. 
#' @param title A title in the graph in quotes. Defaults to \code{NULL}, which indicates no title should be used. 
#' @param colors a length 3 character vector with the colors for the points, line connecting the points and assessment lines. 
#' @param sizes a length 3 numeric vector with the sizes for the points, line connecting the points and assessment lines.
#' 
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return Creates a time series plot.
#' 
#' @details  The \code{assessment} argument determines if lines representing the assessment values should be drawn on the graph. If \code{FALSE} then no lines will be drawn. If \code{TRUE}, the default, then the upper and lower points indicated in \code{object}'s \code{Character} objects will be used to draw the lines. Note that if there are multiple assessment points, for example if diffrerent parks have different points, or if there is both an upper and lower point, they will all be drawn. If a \code{vector} of numbers is passed to \code{assessemnt} instead then those will serve as the assessment values and lines will be drawn accordingly. Note that if \code{obejct} is a \code{data.frame} then the only way to draw assessment lines is by passing a numeric \code{vector} to \code{assessment}.
#' 
#' @export

setGeneric(name="waterseries",function(object,parkcode=NA, sitecode=NA,charname, by="none",assessment=TRUE,layers=c("points","line"), xname=NA,yname=NA,labels=NA,title=NULL,colors=c("blue","black","red"), sizes=c(3, 0.8, 1.1), webplot=FALSE,...){standardGeneric("waterseries")},signature=c("object") )


setMethod(f="waterseries", signature=c(object="NCRNWaterObj"),
          function(object,parkcode, sitecode, charname,by,assessment,xname,yname,labels,title,colors,sizes, webplot,...){
            PlotData<-getWData(object=object,parkcode=parkcode, sitecode=sitecode, charname=charname,...)
            if(is.na(yname)) yname<-paste0(getCharInfo(object=object, charname=charname, info="DisplayName")," (",
                                           getCharInfo(object=object, charname=charname, info="Units"),")") %>% unique
            if(is.na(xname)) xname<-"Date"
            if(is.na(labels)) labels<-switch(by,
                                             none="",
                                             char=getCharInfo(object,charname=charname, info="DisplayName"),
                                             park=getParkInfo(object, info="ParkShortName"),
                                             site=getSiteInfo(object, info="SiteName")
            )
            if(is.logical(assessment) & assessment) assessment<-c(getCharInfo(object=object,parkcode=parkcode, sitecode=sitecode, 
              charname=charname, info="LowerPoint"),getCharInfo(object=object,parkcode=parkcode, sitecode=sitecode, 
              charname=charname, info="UpperPoint")) %>% unlist %>% unique
            
            assessment<-assessment[!is.na(assessment)] # needed if there is no upper or lower assessment.
            callGeneric(object=PlotData, by=by, assessment=assessment, layers=layers, xname=xname, yname=yname,labels=labels, 
                        title=title,colors=colors, sizes=sizes, webplot=webplot, ...)
          })

setMethod(f="waterseries", signature=c(object="data.frame"),
          function(object,by,assessment,xname,yname,labels,title,colors,sizes, webplot,...){
            Grouper<-switch(by,
                            none="",
                            char=object$Characteristic %>% factor,
                            site=object$Site %>% factor,
                            park=object$Park %>% factor)
            if(all(is.na(yname))) yname<-""
            if(all(is.na(xname))) xname<-""
            if(all(is.na(labels))) labels<-switch(by,
                                                  none="",
                                                  char=object$Characteristic %>% unique,
                                                  site=object$Site %>% unique,
                                                  park=object$Park %>% unique)
            
            Xaxis<-if(by=="year") month(object$Date,label=F) else object$Date
        
            OutPlot<-ggplot(object,aes(Xaxis,Value,color=Grouper,shape=Grouper)) +
              {if ("points" %in% layers) geom_point(aes(color=colors[1]), size=sizes[1],pch=16) }+
              {if ("line" %in% layers) geom_line(color=colors[2], size=sizes[2])} +
              scale_color_discrete(name="legend",labels=labels) +
              scale_shape_manual(name="legend",values=1:nlevels(Grouper), labels=labels) +
              {if(by=="year") scale_x_continuous(name=xname,labels=Xaxis %>% unique %>% sort %>% month(T) %>% as.character, breaks=1:12)} +
              {if (is.numeric(assessment)) geom_hline(yintercept=assessment,color=colors[3],linetype="dashed",size=sizes[3])} +
              labs(title=title,y=yname,x=xname) +
              theme_bw() +
              theme(panel.grid = element_blank(), legend.title=element_blank(),legend.position=if(by=="none") "none" else "bottom")
            
            ifelse(webplot, return(ggplotly(OutPlot) %>% plotly::config(displaylogo=F)),return(OutPlot))
})




