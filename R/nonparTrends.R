#' @include getWData.R
#' @include getCharInfo.R
#' 
#' @importFrom NADA cenken
#' @importFrom openair TheilSen
#' @importFrom dplyr %>% case_when group_by summarise mutate rename select
#' @importFrom purrr map 
#' @importFrom tidyr hoist nest
#' @importFrom lubridate month
#' 
#' @title nonparTrends
#' 
#' @description Uses non-parametric methods to test for trends in water quality data across years for each month. 
#' This function assumes that the data are composed of monthly observations that are correlated within a year. 
#' Note that this function is only appropriate for trends that are one-directional. 
#' 
#' @param object A \code{Park} object or a \code{list} of such objects.
#' @param charname Name, in quotes, of a single \code{Characteristic} whose data should be analyzed. Either this or \code{category} is required.
#' @param category Name, in quotes of a single cateogry of charactersitcs whose data should be analyzed. Either this or \code{charname} is required.

#' @param ... Additional commands passed to \code{\link{getWData}} for filtering or subsetting the data.
#' @param censored Either \code{TRUE} or \code{FALSE}. If FALSE (default), function runs the openair::TheilSen test with deseason = TRUE. 
#' If TRUE, function runs a separate NADA::cenken test for for each month in the dataset. 
#' 
#' @examples 
#' waterobj <- importNCRNWater(Dir = "./Data/",
#'                             Data = "Water Data.csv",
#'                             MetaData = "VizMetaData.csv")
#' trends<- nonparTrends(waterobj, parkcode = "ACAD", sitecode = "NETN_ACAD_LONG", charname = "pH", censored = FALSE)
#' trends_cen<- nonparTrends(waterobj, parkcode = "ACAD", sitecode = "NETN_ACAD_LONG", charname = "NO2+NO3_mgL", censored = TRUE)
#' 
#' @return The results of a Theil-Sen analysis for censored or non-censored data with a row for month in the analysis and a column for
#' the month, slope, intercept and p-value (pval) from each test.
#' 
#' @export
#' 

nonparTrends <- function(object, parkcode, sitecode, charname=NA, category=NA, censored = FALSE, ...){

  
  if(!requireNamespace("openair", quietly = TRUE)){
    stop("Package 'openair' needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("NADA", quietly = TRUE)){
    stop("Package 'NADA' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(is.na(charname) & is.na(category)) stop("Must specify a charname or a category.")
  
  df <- getWData(object, parkcode = parkcode, sitecode = sitecode, charname = charname, category=category,...)
  
  if(nrow(df)==0) stop("The specified site and measurement returned a data frame with 0 records.")
  
  if("ValueCen" %in% colnames(df)==FALSE){
    stop("Must have a field named 'ValueCen' to run this version of the function.")
  }

  df$date = df$Date # to make openair happy
  
  units <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode, charname = charname, category=category, info = "Units")[1] 
  
  displayname<-getCharInfo(object, parkcode = parkcode, sitecode = sitecode, charname = charname, category=category, info = "CategoryDisplay")[1]
  
  df <- suppressWarnings(df %>% mutate(year.dec = julian(Date)/365, 
                                       month = as.factor(lubridate::month(Date, label = TRUE, abbr=FALSE)))) 
  
  
  if(censored == FALSE){
   trends_fun <- function(df){
     suppressWarnings(openair::TheilSen(mydata = df, pollutant = "ValueCen", autocor = FALSE,
                       #deseason = TRUE, 
                       modeled = FALSE, type = "month", plot = FALSE, silent = TRUE))} 
   # type = "month" breaks data up by month, so don't need deseason=T, which breaks
   # down when you have <6 months represented (eg only June and August data).
   
   trends_test <- tryCatch(suppressMessages(trends_fun(df)), 
                     error = function(e) {"Error"}, 
                     warning = function(w) {"Warning"})
   
   if(all(trends_test!="Error")){
     trends1 <- trends_fun(df)
     trends2 <- trends1$data$main.data %>% 
       group_by(month) %>% summarise(slope = mean(slope, na.rm=T),
                                     intercept = mean(intercept, na.rm=T),
                                     pval = mean(p, na.rm=T))
    
    # Assumes a p-value = 0 means the model didn't converge.
      results <- suppressWarnings(trends2 %>% 
                    mutate(message = case_when(pval > 0 & pval < 0.05 & slope > 0 ~ 
                      paste0("There was a significant increasing trend of ", 
                      paste0(round(slope,4)), " ", paste0(units), " of ",  paste0(displayname), 
                        " per year for ", paste0(month), "."),
                      pval > 0 & pval < 0.05 & slope < 0 ~ 
                      paste0("There was a significant decreasing trend of ", 
                      paste0(round(slope,4)), " ", paste0(units), " of ",  paste0(displayname),
                        " per year for ", paste0(month), "."),
                      pval == 0 | pval > 0.05 ~ paste0("no trend"),
                      is.na(pval) ~ paste0("Too few data points.")),
                      modeled = TRUE))
   
      } else if(trends_test=="Error"){
     
     results <- data.frame(month = NA, slope = NA, intercept = NA, pval = NA,
                       message = paste0("Too few data points."),
                       modeled = FALSE)
   } 
    
  } else if(censored == TRUE) {
    
    if(any(is.na(df$ValueCen))){
      num_nas<- nrow(df)- nrow(df %>% filter(!is.na(ValueCen)))
      print(paste0("Warning: There were ", num_nas, 
                   " missing values in the ValueCen field that were deleted before running this function. This may have baised the analysis."))
    }
    
    cenken_month<-function(df){  
        NADA::cenken(y = df$ValueCen, ycen= df$Censored, x=df$year.dec) # function to map by month
    }
    
    
    df2<- df %>% group_by(Category, Site, Park, month) %>% 
      filter(!is.na(ValueCen)) %>% 
      mutate(num_meas=length(ValueCen), 
             pct_true= sum(ifelse(Censored==FALSE,1,0))/num_meas,
             adjValueCen = ifelse(Censored==TRUE, max(ValueCen), Value)) %>% ungroup()
    
    month_drops<- suppressWarnings(df2 %>% filter(num_meas<5 | pct_true<0.5) %>% 
      group_by(Category, Characteristic, Site, Park, month) %>% 
      summarise(slope = NA,
                intercept = NA,
                pval = NA,
                modeled = FALSE,
                message = paste0("Too few data points.")) %>% 
      ungroup() %>% droplevels() %>% 
      select(month, slope, intercept, pval, message, modeled) %>% unique())
      
    month_include<- df2 %>% filter(num_meas>=5 & pct_true>=0.5) %>% droplevels()
                                                                
    df_mon <- suppressWarnings(month_include %>% group_by(month) %>% nest())
    
    trends_mon <- suppressWarnings(df_mon %>% mutate(cenken = map(data, cenken_month)) %>% 
                                     select(month, cenken)) %>% ungroup() 
    
    trends <- trends_mon %>% hoist(cenken, slope = "slope", intercept = "intercept", pval="p") %>% 
      select(month, slope, intercept, pval) %>%  
      mutate(message = ifelse(pval>0 & pval <0.05 & slope > 0, 
                              paste0("There was a significant increasing trend of ", 
                                     paste0(round(slope,4)), " ", paste0(units), " of ",  paste0(displayname), " per year for ", month, "."),
                              ifelse(pval>0 & pval <0.05 & slope < 0, 
                                     paste0("There was a significant decreasing trend of ", 
                                            paste0(round(slope,4)), " ", paste0(units), " of ",  paste0(displayname), " per year for ", month, "."),
                                     paste0("no trend"))),
                              modeled = TRUE)
    
    results <- if(is.na(month_drops$month) && nrow(trends)>0){
                  trends
                } else if(is.na(month_drops$month) && nrow(trends)==0){
                  data.frame(slope = NA, intercept = NA, pval = NA,
                             message = paste0("Too few data points."),
                             modeled = FALSE)
                } else if(!is.na(month_drops$month) && nrow(trends)>0){
                  rbind(trends, month_drops)
                } else if(!is.na(month_drops$month) && nrow(trends)==0){
                  month_drops
   }

  }

  results_final <- results %>% droplevels() %>% arrange(month)
  return(results_final)
}
