#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getWData.R
#' @include getCharInfo.R
#' @importFrom dplyr arrange between filter full_join group_by last mutate n rename select slice_min slice_max summarize ungroup
#' @importFrom ggplot2 element_line ggplot geom_line geom_point geom_ribbon labs scale_color_identity scale_x_continuous theme  
#' @importFrom lubridate month year
#' @importFrom magrittr %>% 
#' @importFrom plotly ggplotly 
#' @importFrom methods callGeneric
#' 
#' @title waterbands
#' 
#' @description Produces a plot that summarizes the range of historic data and compares with current measurements.
#' 
#' @inheritParams getChars
#' 
#' @param object Either a \code{data.frame} that is the output of \code{getWData}, a \code{Characteristic} object, a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' 
#' @param charname Required if \code{object} is not a \code{data.frame}. Name(s), in quotes, of one or more \code{Characteristic}s 
#' whose data should be graphed. 
#' @param assessment Vector indicating if assessment lines will be marked on the graph. See details below.
#' @param year_current Year that will be plotted separately. Must be numeric and 4 digits.
#' @param year_historic First year to include in historic range calculations. Last year will be the year prior to year_current. Must be numeric and 4 digits.
#' @param months A numeric vector corresponding to months of the year. Only data from those months will be returned.
#' Ranges from 1 to 12. Default is c(5:10) for May to October.
#' @param param_name Text, defaults to \code{NA}. Used for plotly tooltips
#' @param unit Text, defaults to \code{NA}. Note that the function currently can't handle special characters. Used for plotly tooltips
#' @param yname Text, defaults to \code{NA}. Used for y axis title
#' @param legend  a vector indicating where the legend position. Can be: "none","left","right","top","bottom" or a two element 
#' numeric vector.
#' @param ... Additional arguments used to select and filter data passed to \code{\link{getWData}}
#' @return Creates a plot that compares current with historic ranges. If the minimum or maximum values return a tie, the most recent year is returned.
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
#' sitecode = "NETN_MABI_PA00"
#' charname = "pH"
#'
#' waterbands(netnwd, parkcode = parkcode, sitecode = sitecode, charname = charname,
#'            year_historic = 2006, year_current = 2019, months = c(5:10), 
#'            assessment = TRUE)
#'
#' 
#' @export

setGeneric(name = "waterbands", 
  function(object, parkcode = NA, sitecode = NA, charname = NA, category = NA, 
           year_current = NA, year_historic = NA, months = c(5:10), 
           assessment = TRUE, param_name = NA, unit = NA, yname = NA, legend = "bottom", ...)
  {standardGeneric("waterbands")}, signature = c("object"))


setMethod(f = "waterbands", signature = c(object = "NCRNWaterObj"),
  function(object, parkcode, sitecode, charname, category, year_current, year_historic, 
           months, assessment, param_name, unit, yname, legend, ...){
    
    try(wdat <- getWData(object = object, parkcode = parkcode, sitecode = sitecode, 
                         charname = charname, category = category, months = months,
                         years = year_historic:year_current, ...)) 
    
    if(!exists("wdat") || nrow(wdat) == 0){
      stop("Function arguments did not return a data frame with records.")}

    # Add months and censored info for later filter for plotting
    wdat <- wdat %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE),
                            month_num = as.numeric(month),
                            year = lubridate::year(Date)) %>% 
                     #filter(between(month_num, min(months), max(months))) %>% 
                     arrange(Date)
    
    if(!exists("ValueCen", wdat)) wdat$ValueCen<-wdat$Value         
    wdat_hist <- wdat[wdat$year < year_current, ] 
    wdat_curr <- wdat[wdat$year == year_current, ]
    
    if(nrow(wdat_curr) == 0){
      stop(paste0("There are no data available to plot for year_current: ", year_current, "."))
    }
    
    if(nrow(wdat_hist) == 0){
      stop(paste0("There are no historic data available to plot for years: ", year_historic, ":", year_current - 1, "."))
    }
    
    wdat_min <- wdat_hist %>% group_by(Park, Site, Characteristic, month, month_num) %>% 
      slice_min(order_by = ValueCen, n = 1) %>% 
      summarize(year_min = last(year), .groups = "drop") %>% 
      select(Park, Site, Characteristic, month, month_num, year_min) %>% 
      ungroup() 
    
    wdat_range <- wdat_hist %>% group_by(Park, Site, Characteristic, month, month_num) %>% 
      slice_max(order_by = ValueCen, n = 1) %>% 
      summarize(year_max = last(year), .groups = "drop") %>% 
      select(Park, Site, Characteristic, month, month_num, year_max) %>%
      ungroup() %>% 
      full_join(., wdat_min, by = intersect(names(.), names(wdat_min)))
    
    wdat_sum <- wdat_hist %>% group_by(Park, Site, Characteristic, month, month_num) %>% 
      summarize(num_samps = n(),
                median_val = median(ValueCen, na.rm = TRUE),
                min_val = min(ValueCen, na.rm = TRUE),
                max_val = max(ValueCen, na.rm = TRUE),
                lower_100 = ifelse(num_samps >= 4, min(ValueCen, na.rm = T), NA),
                upper_100 = ifelse(num_samps >= 4, max(ValueCen, na.rm = T), NA),
                lower_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.025, na.rm = T), NA),
                upper_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.975, na.rm = T), NA),
                lower_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.25, na.rm = T), NA),
                upper_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.75, na.rm = T), NA),
                .groups = "drop") %>% 
      full_join(., wdat_range, by = intersect(names(.), names(wdat_range))) %>% 
      filter(!is.na(lower_50)) %>% # if lower_50 is NA, the rest will be NA too
      unique() %>% droplevels() %>% ungroup() 
    
    wdat_comb <- merge(wdat_sum, wdat_curr, by = intersect(names(wdat_sum), names(wdat_curr)),
                        all.x = TRUE, all.y = TRUE) %>% 
      rename(year_curr = year, ValueCen_curr = ValueCen)
    
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
    

    wdat_final <- wdat_comb %>% mutate(LowerPoint = ifelse(assessment == TRUE, 
                                                           getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                                                                charname = charname, category = category, 
                                                                info = "LowerPoint"), NA),
                                       UpperPoint = ifelse(assessment == TRUE, 
                                                           getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                                                                charname = charname, category = category, 
                                                                info = "UpperPoint"), NA),
                                        pcolor = ifelse(!is.na(UpperPoint) & ValueCen_curr > UpperPoint, "orange",
                                                        ifelse(!is.na(LowerPoint) & ValueCen_curr < LowerPoint, "orange",
                                                               "black"))
    )

  callGeneric(object = wdat_final, parkcode = parkcode, sitecode = sitecode, charname = charname, 
              category = category, year_current = year_current, year_historic = year_historic, 
              months = months, assessment = assessment, param_name = param_name, unit = unit,
              yname = yname, legend = legend)
          })

setMethod(f = "waterbands", signature = c(object = "data.frame"),
          function(object, parkcode, sitecode, charname, category, 
                   year_current, year_historic, months, assessment,
                   param_name, unit, yname, legend){

            # set up x axis labels based on months range
            xaxis_breaks <- c(min(months):max(months))
            xaxis_labels <- lapply(xaxis_breaks, function(x){as.character(lubridate::month(x, label = T))})

            monthly_plot <- suppressWarnings( 
              ggplot(data = object, aes(x = month_num, y = median_val))+
              scale_x_continuous(breaks = xaxis_breaks,
                                 labels = xaxis_labels)+
              
              # Min/max band
              geom_ribbon(aes(ymax = upper_100, ymin = lower_100), 
                          fill = "#76b4db", alpha = 0.2)+
              geom_line(aes(y = upper_100, 
                            text = paste0("Historic ", month, " Maximum: ", "<br>", 
                                          param_name, ": ", round(upper_100, 1), " ", unit,
                                          "<br>", "Year observed: ", year_max)),
                        color = "#76b4db", alpha = 0.2, lwd=3)+
              geom_line(aes(y = lower_100, 
                            text = paste0("Historic ", month, " Minimum: ", "<br>", 
                                          param_name, ": ", round(lower_100, 1), " ", unit,
                                          "<br>", "Year observed: ", year_min)),
                        color = "#76b4db", alpha = 0.2, lwd=3)+
              
              # 95% band
              geom_ribbon(aes(ymax = upper_95, ymin = lower_95), 
                          fill = "#76b4db", alpha = 0.4)+
              geom_line(aes(y = upper_95, 
                            text = paste0("Historic ", month, " Upper 95%: ", "<br>", 
                                          param_name, ": ", round(upper_95, 1), " ", unit)),
                        color = "#76b4db", alpha = 0.4, lwd=3)+
              geom_line(aes(y = lower_95, 
                            text = paste0("Historic ", month, " Lower 95%: ", "<br>", 
                                          param_name, ": ", round(lower_95, 1), " ", unit)),
                        color = "#76b4db", alpha = 0.4, lwd=3)+
              
              # 50% band
              geom_ribbon(aes(ymax = upper_50, ymin = lower_50), 
                          fill = "#76b4db", alpha = 0.85)+
              geom_line(aes(y = upper_50, 
                            text = paste0("Historic ", month, " Upper 50%: ", "<br>", 
                                          param_name, ": ", round(upper_50, 1), " ", unit)),
                        color = "#76b4db", alpha = 0.85, lwd=3)+
              geom_line(aes(y = lower_50, 
                            text = paste0("Historic ", month, " Lower 50%: ", "<br>", 
                                          param_name, ": ", round(lower_50, 1), " ", unit)),
                        color = "#76b4db", alpha = 0.85, lwd=3)+
              
              # Median
              geom_line(aes(y = median_val), color = "#1378b5")+
              geom_line(aes(y = median_val, 
                            text = paste0("Historic ", month, " Median ", "<br>", 
                                          param_name, ": ", round(median_val, 1), " ", unit)), 
                        color = "#1378b5")+
              
              # Current measurement
              geom_point(aes(y = ValueCen_curr, color = pcolor, 
                             text = paste0(month, " ", year_curr, "<br>", 
                                           param_name, ": ", round(ValueCen_curr, 1), " ", unit)))+
                
              scale_color_identity()+ # color codes points based on whether above/below thresholds
             
              # Upper and lower points 
              {if(assessment == TRUE) geom_line(aes(y = LowerPoint, text = paste("Lower", param_name, "threshold:", LowerPoint, unit)), 
                        linetype = "dashed", color = "#212121")}+
              {if(assessment == TRUE) geom_line(aes(y = UpperPoint, text = paste("Upper", param_name, "threshold:", UpperPoint, unit)),
                         linetype = "dashed", color = "#212121")}+
              
              # Labels
              labs(y = yname, x = NULL, title = NULL) + 

              # Themes
              theme(axis.title.y = element_text(size = 10),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(color = '#696969', fill = 'white', size = 0.4),
                    axis.line.x = element_line(color = "#696969", size = 0.4),
                    axis.line.y = element_line(color = "#696969", size = 0.4),
                    axis.ticks = element_line(color = "#696969", size = 0.4),
                    legend.key = element_blank(),
                    legend.position = legend)
            )
            return(monthly_plot)
# 
#              monthly_plotly <- tryCatch(ggplotly(monthly_plot, tooltip = c("text")), 
#                                  error = function(e) {
#                                  stop("Error: invalid dataset for plotting. Please check that arguments are spelled correctly and that the specified combination of parkcode, sitecode, and charname exists in the data.")})
#             
#             return(monthly_plotly)

})




