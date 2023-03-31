#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getWData.R
#' @include getCharInfo.R
#' @importFrom dplyr arrange full_join mutate 
#' @importFrom ggplot2 ggplot geom_line geom_point labs scale_x_continuous theme  
#' @importFrom lubridate month year
#' @importFrom magrittr %>% 
#' @importFrom plotly ggplotly 
#' @importFrom methods callGeneric
#' 
#' @title watersite_comps
#' 
#' @description Produces a plot that compares multiple sites for a given year and parameter, with each site having a different color and shape.
#' 
#' @inheritParams getChars
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object, a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' 
#' @param charname Required if \code{object} is not a \code{data.frame}. Name(s), in quotes, of one or more \code{Characteristic}s 
#' whose data should be graphed.
#' @param year Year of data that will be plotted for all sites. Must be numeric and 4 digits.
#' @param months A numeric vector corresponding to months of the year. Only data from those months will be returned.
#' Ranges from 1 to 12. Default is c(5:10) for May to October.
#' @param param_name Text, defaults to \code{NA}. Used for plotly tooltips
#' @param unit Text, defaults to \code{NA}. Used for plotly tooltips
#' @param yname Text, defaults to \code{NA}. Used for y axis title
#' @param legend  a vector indicating where the legend position. Can be: "none","left","right","top","bottom" or a two element 
#' numeric vector.
#' @param layers Defaults to c("points","line") Indicates which layers you wish to see on the plot. 
#' @param assessment Vector indicating if assessment lines will be marked on the graph. If multiple thresholds exist among the sites plotted, all will be plotted. See details below.
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' 
#' @return Creates a plot that compares multiple sites for a given year. 
#' 
#' @details  The \code{assessment} argument determines if lines representing the assessment values should be drawn on the graph. 
#' If \code{FALSE} then no lines will be drawn. If \code{TRUE}, the default, then the upper and lower points indicated in 
#' \code{object}'s \code{Character} objects will be used to draw the lines. Note that if there are multiple assessment points, 
#' for example if different parks have different points, or if there is both an upper and lower point, they will all be drawn. 
#' If a \code{vector} of numbers is passed to \code{assessemnt} instead then those will serve as the assessment values and lines will 
#' be drawn accordingly. Note that if \code{object} is a \code{data.frame} then the only way to draw assessment lines is by passing a 
#' numeric \code{vector} to \code{assessment}.
#' 
#' @examples
#' 
#' netnwd <- importNCRNWater(Dir = "~/Data/NETN", 
#'                           Data = "Water Data.csv", MetaData = "VizMetaData.csv")
#' parkcode = "MABI"
#' charname = "pH"
#' site_list <- getSiteInfo(netnwd, parkcode = parkcode, info = "SiteCode")
#' 
#' watersite_comps(netnwd, parkcode = parkcode, sitecode = site_list, charname = charname,
#'                 year = 2019, months = c(5:10), legend = "none", 
#'                 layers = c("points", "line"), assessment = TRUE)
#'
#' 
#' @export

setGeneric(name = "watersite_comps", 
  function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA, 
           year = NA, months = c(5:10), param_name = NA, 
           unit = NA, yname = NA, legend = "bottom", layers = c("points", "line"), 
           assessment = TRUE, ...)
  {standardGeneric("watersite_comps")}, signature = c("object"))


setMethod(f = "watersite_comps", signature = c(object = "NCRNWaterObj"),
  function(object, parkcode, sitecode, charname, category, year, 
           months, param_name, unit, yname, legend, layers, assessment, ...){
    
    try(wdat <- getWData(object = object, parkcode = parkcode, sitecode = sitecode, 
                         charname = charname, category = category, months = months,
                         years = year, ...)) 
    
    
    if(!exists("wdat") || nrow(wdat) == 0){
      stop("Function arguments did not return a data frame with records.")}

    if(length(year)!=1 | nchar(year)!=4){stop("This function only works for a single year, and year must be 4 digits.")}
    
    # Add months and censored info for later filter for plotting
    wdat <- wdat %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE),
                            month_num = as.numeric(month),
                            year = lubridate::year(Date)) %>% 
                     arrange(Date)
    if(!exists("ValueCen", wdat)) wdat$ValueCen<-wdat$Value   
    
    active_sites <- wdat %>% select(Site) %>% unique()

    site.key <- data.frame(Site =  getSiteInfo(object, parkcode = parkcode, info = "SiteCode"),
                           SiteName =  getSiteInfo(object, parkcode = parkcode, info = "SiteName")) %>% 
                filter(Site %in% active_sites$Site)
          
    # Set up labels for axis and plotly tooltips
    if(is.na(param_name)){
      param_name <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                charname = charname, info = "CategoryDisplay") %>% unique()
      }

    if(is.na(unit)){
      unit <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                          charname = charname, info = "Units") %>% unique()
      }
    
    if(is.na(yname)){
      yname <- ifelse(charname != "pH", 
                      paste0(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                         charname = charname, info = "CategoryDisplay") %>% unique(), " (", 
                             getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                         charname = charname, info = "Units") %>% unique(),                           
                           ")"),
                      paste0(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                         charname = charname, info = "CategoryDisplay") %>% unique()))
      }
    

    wdat_final <- wdat %>% mutate(LowerPoint = ifelse(assessment == TRUE, 
                                                      getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                                                                  charname = charname, category = category, 
                                                                  info = "LowerPoint"), NA),
                                  UpperPoint = ifelse(assessment == TRUE, 
                                                      getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                                                                  charname = charname, category = category, 
                                                                  info = "UpperPoint"), NA)) %>% 
                  full_join(., site.key, by = "Site") %>% arrange(SiteName, Characteristic)
    
    if(nrow(wdat_final) == 0){
      stop(paste0("There are no data available to plot for the sites or parameters specified in year: ", year, "."))
    }
    
  callGeneric(object = wdat_final, parkcode = parkcode, sitecode = sitecode, charname = charname, 
              category = category, year = year, months = months, 
              param_name = param_name, unit = unit, yname = yname, legend = legend, layers = layers,
              assessment = assessment)
          })

setMethod(f = "watersite_comps", signature = c(object = "data.frame"),
          function(object, parkcode, sitecode, charname, category, year, 
                   months, param_name, unit, yname, legend, layers, assessment){

            # set up x axis labels based on months range
            xaxis_breaks <- c(min(months):max(months))
            xaxis_labels <- lapply(xaxis_breaks, function(x){as.character(lubridate::month(x, label = T))})
            numsites <- length(unique(object$Site))
            rep_symb <- ifelse(numsites <= 5, 1, numsites * 2) # adds buffer so won't run out.
             
            site_comp_plot <- suppressWarnings( 
              ggplot(data = object, aes(x = month_num, y = ValueCen, shape = SiteName, 
                                        color = SiteName))+
              scale_x_continuous(breaks = xaxis_breaks,
                                 labels = xaxis_labels)+
              {if("line" %in% layers) geom_line()}+  

              #{if("points" %in% layers) geom_point() }+  # Not sure why this is here
              {if("points" %in% layers){
                geom_point(aes(text = paste0(SiteName, "<br>", month, " ", year, 
                                             "<br>", param_name, ": ", round(ValueCen,1), 
                                             " ", unit)), 
                           size = 2)}}+
              viridis::scale_color_viridis(discrete = TRUE, option = "D")+
              scale_shape_manual(values = rep(c(16,17,18,19,20), times = rep_symb))+ 
              # Labels
              labs(y = yname, x = NULL, title = NULL) + 
  
              {if(assessment == TRUE) 
                geom_line(aes(y = LowerPoint,
                              text = paste("Lower", param_name, "threshold:", LowerPoint, unit)),
                          linetype = "dashed", color = "#212121",
                          guide = FALSE)}+
              {if(assessment == TRUE) 
                geom_line(aes(y = UpperPoint,
                              text = paste("Upper", param_name, "threshold:", UpperPoint, unit)),
                          linetype = "dashed", color = "#212121",
                          guide = FALSE)}+

              # Themes
              theme(axis.title.y = element_text(size = 10),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(color = '#696969', fill = 'white', size = 0.4),
                    axis.line.x = element_line(color = "#696969", size = 0.4),
                    axis.line.y = element_line(color = "#696969", size = 0.4),
                    axis.ticks = element_line(color = "#696969", size = 0.4),
                    legend.key = element_blank(),
                    legend.title = element_blank(),
                    legend.position = legend)
            )

            return(site_comp_plot)
            
            # site_comp_plotly <- tryCatch(ggplotly(site_comp_plot, tooltip = c("text"), showlegend = ifelse(legend == "none", FALSE, TRUE)), 
            #                      error = function(e) {
            #                      stop("Error: invalid dataset for plotting. Please check that arguments are spelled correctly and that the specified combination of parkcode, sitecode, and charname exists in the data.")})
            # 
            # return(site_comp_plotly)

})




