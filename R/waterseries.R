#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getWData.R
#' @importFrom dplyr arrange filter mutate pull slice
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_line geom_point ggplot labs scale_color_manual scale_shape_manual theme element_blank theme_bw
#' @importFrom lubridate month year
#' @importFrom magrittr %>% 
#' @importFrom plotly config ggplotly
#' @importFrom purrr map_chr
#' @importFrom viridis scale_color_viridis
#' @importFrom methods callGeneric
#' 
#' @title waterseries
#' 
#' @description Produces a time series plot from water data. 
#' 
#' @inheritParams getChars
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object, a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' 
#' @param charname Required if \code{object} is not a \code{data.frame}. Name(s), in quotes, of one or more \code{Characteristic}s 
#' whose data should be graphed.
#' @param by Indicates how the data for the plot should be grouped. A text variable in quotes. Choices are:
#' \describe{
#' \item{"park"}{If more than one park is included in the graph, this will produce a different line for each park.}
#' \item{"site}{If more than one site is included in the graph, this will produce a different line for each site.}
#' \item{"category"}{If more than one category is included in the graph, this will prodice a different line for each category.}
#' \item{"char"}{The default. If more than one characteristic is included in the graph, this will produce a different line for 
#' each one.} 
#' }
#' 
#' @param assessment Vector indicating if assessment lines will be marked on the graph. See details below.
#' @param layers Defaults to c("points","line") Indicates which layers you wish to see on the plot. 
#' @param yname Text, defaults to \code{NA}. A label for the y-axis. If an \code{Characteristic}, \code{Site}, or \code{Park} object 
#' is passed to \code{object}, then the y-label will default to the Display Name and Units for the Characteristic, unless overwritten by the 
#' \code{yname} argument. If a \code{data.frame} is passed then the y-label will either be the text from \code{yname} or blank 
#' if \code{yname} is left as \code{NA}.
#' @param xname Text, defaults to \code{NA}. A label for the x-axis. If a \code{Characteristic}, \code{Site}, or \code{Park} 
#' object is passed to \code{object}, then the x-label will default to whatever is indicated in \code{by}, 
#' unless overwritten by the \code{xname} argument. If a \code{data.frame} is passed then the x-label will either be the text 
#' from \code{xname} or blank if \code{xname} is left as \code{NA}.
#' 
#' @param censored If \code{FALSE} assumes all data points are true measurements. If \code{TRUE} plots the minimum or maximum 
#' quantification limit and color codes to distinguish true measurements from detection limits. Defaults to \code{FALSE}.
#' 
#' @param deseason If \code{FALSE} will plot all data points on the same figure. If \code{TRUE} will split plots by month. 
#' Defaults to \code{FALSE}. 
#' 
#' @param labels A character vector indicating the labels for the data series, defaults to \code{NA}. If labels are provided 
#' (one for each series) they will be printed. If \code{object} is a \code{data.frame} and \code{labels} is \code{NA} then no labels 
#' will be printed. If \code{object} is a \code{Characteristic}, \code{Site}, or \code{Park} object, and \code{labels} is \code{NA} 
#' then the default will depend on the \code{by} argument. "site" will be labeled with the site name from the \code{Site}'s 
#' \code{Display Name} slot and "park" with the \code{Park}'s short name from the \code{ShortName} slot. 
#' @param title A title in the graph in quotes. Defaults to \code{NULL}, which indicates no title should be used. 
#' @param colors a character vector with the colors for the points and lines. 
#' @param assesscolor a length one character vector with the color for the assessment lines.
#' @param sizes a length 3 numeric vector with the sizes for the points, line connecting the points and assessment lines.
#' @param legend  a vector indicating where the legend position. Can be: "none","left","right","top","bottom" or a two element 
#' numeric vector.
#' @param webplot  If TRUE, the plot is produced using ggploty from the ploty package. Will produce a html plot with interactive
#'  features.
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return Creates a time series plot.
#' 
#' @details  The \code{assessment} argument determines if lines representing the assessment values should be drawn on the graph. 
#' If \code{FALSE} then no lines will be drawn. If \code{TRUE}, the default, then the upper and lower points indicated in 
#' \code{object}'s \code{Character} objects will be used to draw the lines. Note that if there are multiple assessment points, 
#' for example if different parks have different points, or if there is both an upper and lower point, they will all be drawn. 
#' If a \code{vector} of numbers is passed to \code{assessment} instead then those will serve as the assessment values and lines will 
#' be drawn accordingly. Note that if \code{object} is a \code{data.frame} then the only way to draw assessment lines is by passing a 
#' numeric \code{vector} to \code{assessment}.
#' 
#' @export

setGeneric(name="waterseries",function(object, parkcode=NA, sitecode=NA,charname=NA, category=NA, by="char", 
                                       assessment=TRUE,
              layers=c("points","line"), xname=NA, yname=NA, labels=NA, title=NULL, colors=NA, assesscolor="red", 
              sizes=c(3, 0.8, 1.1), censored = FALSE, deseason = FALSE,
              legend="bottom", webplot=FALSE,...)
  {standardGeneric("waterseries")},signature=c("object") )


setMethod(f="waterseries", signature=c(object="NCRNWaterObj"),
  function(object, parkcode, sitecode, charname, category, by, assessment, layers, xname, yname,
           labels, title, colors, assesscolor, sizes, censored, deseason, legend, webplot,...){
          
    try(PlotData<-getWData(object=object,parkcode=parkcode, sitecode=sitecode, 
                       charname=charname, category=category, ...)) 
    
    if(!exists("PlotData") || nrow(PlotData)==0)stop("Function arguments did not return a data frame with records.")

    # Add months and censored info for later filter for plotting
    PlotData <- PlotData %>% mutate(year.dec = as.numeric(julian(Date)/365), 
                  month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% arrange(Date)
                                 

    PlotData <- PlotData %>% group_by(Category, Characteristic, Site, Park, month) %>% 
      mutate(num_meas = ifelse(censored, sum(!is.na(ValueCen)),sum(!is.na(Value))), 
             pct_true = sum(ifelse(!censored, 1, 0))/num_meas, #sum(!is.na(ValueCen)),
             adjValueCen = ifelse(censored == TRUE, max(ValueCen, na.rm = TRUE), Value)) %>% 
      ungroup() %>% arrange(month)
                
            if(is.na(yname)) yname<-paste0(getCharInfo(object=object, charname=charname, category=category, info="CategoryDisplay")," (",
                                        getCharInfo(object=object, charname=charname, category=category, info="Units"),")") %>% unique()
            
            if(is.na(xname)) xname<-"Date"
            if(all(is.na(labels))) labels<-switch(by,
                none="",
                char=unique(PlotData$Characteristic) %>% map_chr( ~unique(getCharInfo(object,charname=.x,info="DisplayName"))) %>% 
                  factor(.,levels=unique(.)),
                park=unique(PlotData$Park) %>% map_chr(~unique(getParkInfo(object, parkcode=.x, info="ParkShortName"))) %>%
                  `[`(order(unique(PlotData$Site))) %>% factor(.,levels=unique(.)),
               
                site=unique(PlotData$Site) %>% map_chr(~unique(getSiteInfo(object, sitecode=.x, info="SiteName"))) %>% 
                  `[`(order(unique(PlotData$Site))) %>% factor(.,levels=unique(.)),
                
                category=unique(PlotData$Category) %>% map_chr(~unique(getCharInfo(object, category=.x, info="CategoryDisplay"))) %>% 
                  `[`(order(unique(PlotData$Category))) %>% factor(.,levels=unique(.))
            )
            
            if(is.logical(assessment) & assessment) assessment<-c(getCharInfo(object=object,parkcode=parkcode, sitecode=sitecode, 
              charname=charname, category = category, info="LowerPoint"),getCharInfo(object=object,parkcode=parkcode, sitecode=sitecode, 
              charname=charname, category=category, info="UpperPoint")) %>% unlist %>% unique
            
            assessment<-assessment[!is.na(assessment)] # needed if there is no upper or lower assessment.
            
            callGeneric(object=PlotData, by=by, assessment=assessment, layers=layers, xname=xname, yname=yname,labels=labels, 
                        title=title,colors=colors, assesscolor=assesscolor, sizes=sizes, censored = censored, deseason = deseason,
                        legend=legend, webplot=webplot)
          })

setMethod(f="waterseries", signature=c(object="data.frame"),
          function(object, by, assessment, layers, xname, yname, labels, title, colors, assesscolor, 
                   sizes, censored, deseason, legend, webplot){
            
            Grouper<-switch(by,
                            char=object %>% pull(Characteristic) %>% factor(., levels=unique(.)),
                            site=object %>% pull(Site) %>% factor(., levels=unique(sort(.))),
                            park=object %>% pull(Park) %>% factor(., levels=unique(sort(.))),
                            category=object %>% pull(Category) %>% factor(., levels=unique(sort(.)))
                            )
            
            if(all(is.na(yname))) yname<-""
            if(all(is.na(xname))) xname<-""
            if(all(is.na(labels))) labels<-switch(by,
                                                  char=object$Characteristic %>% unique,
                                                  site=object$Site %>% unique,
                                                  park=object$Park %>% unique)
            
           Xaxis <- object$Date
          
           OutPlot<-
              if(censored == TRUE){ 
                if(deseason == FALSE){
                  ggplot(object, aes(x = Xaxis, y = adjValueCen, color = censored, group = censored))+
                    {if ("points" %in% layers) geom_point(size = sizes[1]) }+
                    {if (is.numeric(assessment)) geom_hline(yintercept = assessment, color = assesscolor, 
                                                            linetype = "dashed", size = sizes[3])} +
                    labs(title = title, y = yname, x = xname) +
                    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                                       labels = c("FALSE" = "True Value", "TRUE"= "Censored"))+
                    theme_bw() +
                    theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = legend)
                  
                } else if(deseason == TRUE){
                  
                  df_mon <- object %>% filter(num_meas>=6) %>% droplevels() %>% arrange(month)
                  
                  if(nrow(df_mon)==0)stop("Too few data points to plot.")

                  ggplot(df_mon, aes(x = Date, y = adjValueCen, color = censored, group = censored))+
                    geom_point(size = sizes[1])+
                    labs(title = title, y = yname, x = xname) +
                    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                                       labels = c("FALSE" = "True Value", "TRUE"= "Censored"))+
                    theme_bw() +
                    theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = legend)+
                    facet_wrap(~month)+
                  {if (is.numeric(assessment)) geom_hline(yintercept = assessment, color = assesscolor, 
                                                          linetype = "dashed", size = sizes[3])} 
                } # End of deseason = TRUE
                
              } else if (censored == FALSE){
                if(deseason == FALSE){
                  
                  ggplot(object, aes(x = Xaxis, y = Value))+
                    {if ("points" %in% layers) geom_point(aes(color = Grouper, shape = Grouper), size = sizes[1]) }+
                    {if ("line" %in% layers) geom_line(aes(color = Grouper), size = sizes[2])} +
                    {if (is.na(colors)) viridis::scale_color_viridis(name = "legend", labels = labels, 
                                                                     discrete = T)}+
                    {if (!is.na(colors)) scale_color_manual(name = "legend", labels = labels, values = colors)}+
                    scale_shape_manual(name = "legend", labels = labels, 
                                       values = c(16, 15, 17, 18, 1, 0, 2, 5, 6, 3, 4, 8, 13, 9, 12)[1:nlevels(Grouper)]) +
                    {if (is.numeric(assessment)) geom_hline(yintercept = assessment, color = assesscolor,
                                                            linetype = "dashed", size = sizes[3])} +
                    labs(title = title, y = yname, x = xname) +
                    theme_bw() +
                    theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = legend)#+

                  
                  } else if(deseason == TRUE){
                 
                  df_mon2 <- object %>% filter(num_meas>=6) %>% droplevels() %>% arrange(month)
                  
                  if(nrow(df_mon2)==0)stop("Too few data points to plot.")
                  
                  Grouper2<-switch(by,
                                  char = df_mon2 %>% pull(Characteristic) %>% factor(., levels=unique(.)),
                                  site = df_mon2 %>% pull(Site) %>% factor(., levels=unique(sort(.))),
                                  park = df_mon2 %>% pull(Park) %>% factor(., levels=unique(sort(.))),
                                  category = df_mon2 %>% pull(Category) %>% factor(., levels=unique(sort(.)))
                  )

                  ggplot(df_mon2, aes(x = Date, y = Value))+
                    geom_point(aes(color = Grouper2, shape = Grouper2), size = sizes[1])+
                    #geom_point(size = sizes[1])+
                    {if (is.na(colors)) viridis::scale_color_viridis(name = "legend", labels = labels, discrete = T)}+
                    {if (!is.na(colors)) scale_color_manual(name = "legend", labels = labels, values = colors)}+
                    scale_shape_manual(name = "legend", labels = labels,
                                       values = c(16, 15, 17, 18, 1, 0, 2, 5, 6, 3, 4, 8, 13, 9, 12)[1:nlevels(Grouper)]) +
                    {if (is.numeric(assessment)) geom_hline(yintercept = assessment, color = assesscolor,
                                                            linetype = "dashed", size = sizes[3])} +
                    labs(title = title, y = yname, x = xname) +
                    theme_bw() +
                    theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = legend)+
                    facet_wrap(~month)
                  
                  }
              }# End of censored == FALSE
            
            ifelse(webplot, return(ggplotly(OutPlot) %>% plotly::config(displaylogo=F)),return(OutPlot))
})




