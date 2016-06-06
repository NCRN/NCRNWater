#' @include getWData.R
#' @include waterbox.R
#' @importFrom magrittr %>% 
#' @importFrom lubridate year month floor_date
#' @importFrom dplyr left_join select mutate group_by summarize
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_gradientn element_rect labs theme_bw theme element_blank element_text scale_y_discrete
#' @importFrom plotly ggplotly config
#' 
#' @title waterheat
#' 
#' @description Produces heat maps from water data. 
#' 
#' @param object Required. Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param charname Required if \code{object} is not a \code{data.frame}. Name, in quotes, of a single \code{Characteristic} whose data should be graphed.
#' @param by Indicates how the data for the heatmnp should be grouped. A text variable in quotes. Choices are:
#' \describe{
#' \item{"month"}{ Will produce a heatmap where each row is a month and each column a year.}
#' \item{"sitey}{Will provide a heatmap where each row corresponds to a site, and each column is a year.}
#' \item{'sites}{will privde a heatmap where each row corresponds to a site and each column is a season-year combination sucah as Win-11. For this graph Winter is Dec-Feb, Spring is Mar-May, Summer is Jun-Aug and Fall is Sept-Nov}
#' \item{"sitem}{Will provide a heatmap where each row corresponds ot a site and each column is month X year combination - such as Jan-11, Feb-11 etc.}
#' }
#' @param yname Text, defaults to \code{NA}. A label for the y-axis. The y-label will default to whatever is chosen in \code{by}. unless overwitten by the \code{yname} argument.
#' 
#' @param xname Text, defaults to \code{NA}. A label for the x-axis. If left as \code{NA} and \code{by} is "month" then \code{xname} defaults to "year", if \code{by} is "Site" then \code{xname} will be "Date".
#' 
#' @param labels A character vector indicating the labels for the ticks on the y-axis. bars, If left as \code{NA}, and \code{by} is "month", the tick labels will be the three letter abbreviation for the month, if \code{by} is "site", and object is a \code{Site} or \code{Park} object, then the tick labels will be the site names.
#' 
#' @param title A title in the graph in quotes. Defauls to \code{NULL}, which indicates no title should be used. 
#' 
#' @param ltitle The title for the legend in quotes. Defaults to \code{NA}. If left as \code{NA} and \code{object} is a \code{Park}, \code{Site}, or \code{Characteristic} object, the title will be the characteristic name and units. 
#' 
#' @param stat Used to determine cell values is more than one observation corresponds to a single cell. One of four options:"mean","median",min" or "max", corresponing to the mean (the default), the meidan, the minimum or the maximum of all values for that cell.
#' 
#' @param siteorder The order in which sites should be displayed when \code{by} is "sitem" or "sitey". If left as \code{NA} then the order will be determined by the order in which \code{Park} objects are in \code{object}, and/or the order of the \code{Site} objects in each park. 
#'  
#' @param webplot If \code{TRUE} plot will be an interactive html plot by passing it to \code{\link{ggplotly}}? in the \code{plotly} package. 
#' 
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return Creates a heatmap
#' 
#' @export

setGeneric(name="waterheat",function(object,charname, by="sitem",yname=NA,xname=NA,labels=NA,title=NULL,ltitle=NA,stat="mean",siteorder=NA, webplot=FALSE,...){standardGeneric("waterheat")},signature=c("object") )


setMethod(f="waterheat", signature=c(object="NCRNWaterObj"),
          function(object,charname,by,yname,xname,labels,title,siteorder,webplot,...){
            PlotData<-getWData(object=object,charname=charname,...)
            if(all(is.na(labels))) labels<-switch(by,
                sitem=,sites=,sitey=if(class(object)!="Characterisitc") getSiteInfo(object,sitecode=PlotData$Site %>% 
                                                          unique,info = "SiteName") %>% rev else NA,
                  month=NA
            )
            
            if(is.na(ltitle)) ltitle <-paste0(getCharInfo(object=object, charname=charname, info="DisplayName")," (",
                                           getCharInfo(object=object, charname=charname, info="Units"),")") %>% unique
            
            if(all(is.na(siteorder))) siteorder<-PlotData$Site %>% unique %>% rev
              
            waterheat(object=PlotData,by=by,yname=yname,xname=xname,labels=labels,title=title,ltitle=ltitle,stat=stat,
                      siteorder=siteorder, webplot=webplot)
  })


setMethod(f="waterheat", signature=c(object="data.frame"),
  function(object,by,yname,xname,labels,title,ltitle,stat, siteorder,webplot){
    object<-object[order(object$Date),]
            
    
    MakeSeason<-function(x){
      Month<-month(x)
      Year<-format(x, "%y")
      SeasonVect<-c("Win","Win","Spr","Spr","Spr","Sum","Sum","Sum","Fall","Fall","Fall","Win")
      Season<-SeasonVect[Month]
      return(paste0(Season,"-",Year))
      
    }
    
    ###* make new data.frame for heat map ####
    HeatData<-data.frame(Horiz=switch(by,
          month=object$Date %>% year %>% factor,
          sitem=seq(from=object$Date %>% min %>% floor_date("month"), to=object$Date %>% max, by="month") %>% 
            format("%b-%y") %>% factor(levels=unique(.)) %>% rep(.,times=object$Site %>% unique %>% length),
          sites=seq(from=object$Date %>% min %>% floor_date("month"), to=object$Date %>% max, by="month") %>% MakeSeason %>% 
            factor(ordered=T, levels=unique(.)) %>% rep(.,times=object$Site %>% unique %>% length),
          sitey=seq(object$Date %>% min %>% floor_date("year"),  to =object$Date %>% max, by="year") %>% 
            year %>% factor %>% rep(.,times=object$Site %>% unique %>% length)
    ))
    
    

    HeatData$Vert=switch(by,
      month=object$Date %>% month(T) %>% factor(levels=rev(levels(.))),
      sitem=,sites=,sitey=rep(unique(object$Site), each=HeatData$Horiz %>% unique %>% length) %>% 
        factor(ordered=T, levels=siteorder)
    )
    
    HeatData <- switch(by,
      month=HeatData %>% 
        left_join(object %>% select(Date, Value ) %>% 
          mutate(Year=Date %>% year %>% factor, Date=Date %>% month(T) %>% factor(levels=levels(HeatData$Vert))),
          by=c("Horiz"="Year", "Vert"="Date")),
      sitem= HeatData %>% 
        left_join(object %>% select(Date, Site, Value ) %>%
          mutate(Date=format(Date,"%b-%y") %>% factor(levels=levels(HeatData$Horiz)), Site=factor(Site,levels=siteorder, ordered=T)), 
          by=c("Horiz"="Date","Vert"="Site")),
      sitey=HeatData %>% 
        left_join(object %>% select(Date,Site,Value) %>% 
           mutate(Date=Date %>% year %>% factor(levels=levels(HeatData$Horiz)), Site=factor(Site, levels=siteorder, ordered=T)),
           by=c("Horiz"="Date", "Vert"="Site"))
    )
    
    if(is.na(xname)) xname<-switch(by,
                          month=,sitey="Year",
                          sitem="Date")
            
    if(is.na(yname)) yname<-switch(by,
                         month="Month",
                         sitem=,sitey="Site")
           
    if(all(is.na(labels))) labels<-levels(HeatData$Vert)
    
    #### Make Graph ####
      OutPlot<-ggplot(HeatData %>% group_by(Horiz,Vert) %>% 
              summarize(Value=do.call(what=stat, args=list(x=Value, na.rm=T))),
              aes(x=Horiz,y=Vert,fill=Value)) +
        geom_raster() +
        scale_fill_gradientn(name=ltitle,colors=c("blue","white","red")) +
        labs(title=title,y=yname,x=xname) +
        scale_y_discrete(labels=labels) +
        theme_bw() +
        theme(panel.grid = element_blank(), panel.background=element_rect(fill="grey",color="grey"),
              axis.text.x=element_text(angle=75,hjust=1))
            
      if(webplot) return(ggplotly(OutPlot) %>% plotly::config(displaylogo=F)) else return(OutPlot)
})




