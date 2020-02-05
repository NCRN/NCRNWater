#' @include getWData.R
#' 
#' @importFrom NADA cenken
#' @importFrom openair TheilSen
#' @importFrom dplyr %>% group_by summarise mutate case_when rename select
#' @importFrom purrr map 
#' @importFrom tidyr hoist nest
#' 
#' @title nonparTrends
#' 
#' @description Uses non-parametric methods to test for trends in water quality data across years for each month. 
#' This function assumes that the data are composed of monthly observations that are correlated within a year. 
#' Note that this function is only appropriate for trends that are one-directional. 
#' 
#' @param object A \code{Park} object or a \code{list} of such objects.
#' @param ... Additional commands passed to \code{\link{getWData}} for filtering or subsetting the data.
#' @param censored Either \code{TRUE} or \code{FALSE}. If FALSE (default), function runs the openair::TheilSen test with deseason = TRUE. 
#' If TRUE, function runs a separate NADA::cenken test for for each month in the dataset. 
#' 
#' @examples 
#' waterobj <- importNCRNWater(Dir = "./Data/,
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

nonparTrends <- function(object, parkcode, sitecode, charname, censored = FALSE){
  if(!requireNamespace("tidyr", quietly = TRUE)){
    stop("Package 'tidyr' needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("openair", quietly = TRUE)){
    stop("Package 'openair' needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("NADA", quietly = TRUE)){
    stop("Package 'NADA' needed for this function to work. Please install it.", call. = FALSE)
  }
  
  df <- getWData(object, parkcode = parkcode, sitecode = sitecode, charname = charname)
  df$date = df$Date
  
  if(censored == FALSE){
    trends <- openair::TheilSen(mydata = df, pollutant = "Value", autocor = FALSE, deseason = TRUE, plot = FALSE, type="month")
    results<- trends$data$main.data %>% group_by(month) %>% summarise(slope = mean(slope, na.rm=T),
                                                                      intercept = mean(intercept, na.rm=T),
                                                                      pval = mean(p, na.rm=T))
    
  } else {
    
    df <- df %>% mutate(ycen = ifelse(grepl("\\*", df$TextValue), TRUE, FALSE),
                        year.dec = julian(df$date)/365, #Gives slope in change/year, but doesn't account for leap years
                        value = as.numeric(case_when(grepl("\\*", df$TextValue) & !is.na(MDL) ~ paste(MDL), 
                                                     grepl("\\*", df$TextValue) & !is.na(UDL) ~ paste(UDL),
                                                     TRUE ~ paste(TextValue))),
                        month_grp = months(Date)) 
                                                                
    df_mon <- df %>% group_by(month_grp) %>% nest()
    
    cenken_month<-function(df){NADA::cenken(y = df$value, ycen= df$ycen, x=df$year.dec)} # function to map by month
    
    df_mon <-df %>% group_by(month_grp) %>% nest() 
    
    trends_mon <- suppressWarnings(df_mon %>% mutate(cenken = map(data, cenken_month)) %>% 
                                     select(month_grp, cenken)) %>% ungroup() 
    
    results <- trends_mon %>% hoist(cenken, slope = "slope", intercept = "intercept", pval="p") %>% 
      select(month_grp, slope, intercept, pval) %>% rename(month = "month_grp")
    
  }
  return(results)
}
