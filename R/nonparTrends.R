#' @include getWData.R
#' @include getCharInfo.R
#' 
#' @importFrom dplyr %>% case_when group_by summarise mutate rename select
#' @importFrom purrr map 
#' @importFrom tidyr hoist nest
#' @importFrom lubridate month
#' @importFrom methods getGeneric
#' 
#' @title nonparTrends
#' 
#' @description Uses non-parametric methods to test for trends in water quality data across years for each month. 
#' This function assumes that the data are composed of monthly observations that are correlated within a year. 
#' Note that this function is only appropriate for trends that are one-directional and requires at least 6 datapoints 
#' within a given month to test for trends. 
#' 
#' @param object A \code{Park} object or a \code{data.frame} such as that produced by getWData.
#' @param charname Name, in quotes, of a single \code{Characteristic} whose data should be analyzed. Either this or \code{category} is required.
#' @param category Name, in quotes of a single category of characteristics whose data should be analyzed. Either this or \code{charname} is required.
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

setGeneric(name = "nonparTrends", function(object, parkcode = NA, sitecode = NA, 
                                        charname = NA, category = NA, info = NA,
                                        censored = FALSE, ...)
  {standardGeneric("nonparTrends")}, signature = c("object"))


setMethod(f = "nonparTrends", signature = c(object = "NCRNWaterObj"), 
          function(object, parkcode, sitecode, charname, category, info, censored, ...){
            
            if(is.na(charname) && is.na(category)) stop("Must specify a charname or a category.")
            
            
            watdata <- getWData(object, parkcode = parkcode, sitecode = sitecode, 
                           charname = charname, category = category, ...)
            
            if(nrow(watdata) == 0) stop("The specified site and measurement returned a data 
              frame with 0 records.")
            if("ValueCen" %in% colnames(watdata) == FALSE){
              stop("Must have a field named 'ValueCen' to run this version of the function.")
            }
            
            
            watdata$date = watdata$Date # to make openair happy
  
            paramunits <- paste(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                 charname = charname, category = category, info = "Units")[1] %>% unique())
            
            displayname <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                       charname = charname, category = category,
                                       info = "CategoryDisplay")[1]

            callGeneric(object = watdata, parkcode = parkcode, sitecode = sitecode, 
                        charname = charname, category = category, info = info, paramunits = paramunits, 
                        displayname = displayname)
                      })

setMethod(f="nonparTrends", signature = c(object = "data.frame"),
          function(object, parkcode, sitecode, charname, category, info, censored, paramunits, displayname, ...){
            
  if(!requireNamespace("openair", quietly = TRUE)){
    stop("Package 'openair' needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  
  if(!requireNamespace("NADA", quietly = TRUE)){
    stop("Package 'NADA' needed for this function to work. Please install it.", 
         call. = FALSE)
  }
            
  
     
  if(is.na(charname) && is.na(category)) stop("Must specify a charname or a category.")
  if(nrow(object)==0) stop("The specified site and measurement returned a data frame with 0 records.")    
  
  df <- object %>% mutate(year.dec = as.numeric(julian(Date)/365),
                      month = lubridate::month(Date, label = TRUE,
                                                         abbr = FALSE))
  
  if(censored == FALSE){
    trends_fun <- function(df){
      openair::TheilSen(mydata = df, pollutant = "ValueCen", autocor = FALSE,
                       #deseason = TRUE,
                        modeled = FALSE, type = "month", plot = FALSE, silent = TRUE)}
   # type = "month" breaks data up by month, so don't need deseason=T, which breaks
   # down when you have <6 months represented (eg only June and August data).

   trends_test <- tryCatch((trends_fun(df)),
                     error = function(e) {"Error"},
                     warning = function(w) {"Warning"})

   if(all(!trends_test %in% c("Error", "Warning"))){
     trends1 <- trends_fun(df)
     trendsres <- subset(trends1$data[[2]], !is.na(conc))
     names(trendsres)[names(trendsres) == 'p'] <- 'pval'
     
     num_samps <- df %>% group_by(month) %>% summarise(num_samps = sum(!is.na(ValueCen)), .groups = 'drop')
     trendsres2 <- merge(trendsres, num_samps, by = "month", all.x = TRUE, all.y = TRUE)
     
     results <- trendsres2 %>% mutate(message = ifelse(num_samps >= 6, 
                                                  case_when(!is.na(pval) & pval > 0 & pval < 0.05 & slope > 0 ~ 
                                                              paste0("There was a significant increasing trend of ", 
                                                              paste0(round(slope, 4)), " ", paste0(units), " of ",  
                                                              paste0(displayname), " per year for ", paste0(month), "."),
                                                            !is.na(pval) & pval > 0 & pval < 0.05 & slope < 0 ~ 
                                                              paste0("There was a significant decreasing trend of ", 
                                                              paste0(round(slope,4)), " ", paste0(units), " of ",  
                                                              paste0(displayname), " per year for ", paste0(month), "."),
                                                            !is.na(pval) & (pval == 0 | pval > 0.05) ~ paste0("no trend"),
                                                            is.na(pval) ~ paste0("Too few data points.")),
                                                    paste0("Too few data points")),
                                      modeled = TRUE)

      } else if(any(trends_test %in% c("Error", "Warning"))){

     results <- data.frame(month = NA, slope = NA, intercept = NA, pval = NA,
                       message = paste0("Too few data points."),
                       modeled = FALSE)
   }

  } else if(censored == TRUE) {

    if(any(is.na(df$ValueCen))){
      num_nas<- nrow(df)- nrow(df %>% filter(!is.na(ValueCen)))
      print(paste0("Warning: There were ", num_nas,
                   " missing values in the ValueCen field that
                   were deleted before running this function.
                   This may have baised the analysis."))
    }

    cenken_month<-function(df){
        NADA::cenken(y = df$ValueCen, ycen= df$Censored, x=df$year.dec) # function to map by month
    }


    df2<- df %>% group_by(Category, Site, Park, month) %>%
      filter(!is.na(ValueCen)) %>%
      mutate(num_meas=sum(!is.na(ValueCen)),
             pct_true= sum(ifelse(Censored==FALSE,1,0))/num_meas,
             adjValueCen = ifelse(Censored==TRUE, max(ValueCen, na.rm = TRUE),
                                  Value)) %>% ungroup()

    month_drops<- df2 %>% filter(num_meas<6 | pct_true<0.5) %>%
      group_by(Category, Characteristic, Site, Park, month) %>%
      summarise(slope = NA,
                intercept = NA,
                pval = NA,
                modeled = FALSE,
                message = paste0("Too few data points.")) %>%
      ungroup() %>% droplevels() %>%
      select(month, slope, intercept, pval, message, modeled) %>% unique()

    month_include<- df2 %>% filter(num_meas >= 6 & pct_true >= 0.5) %>% droplevels()

    df_mon <- month_include %>% group_by(month) %>% nest()

    trends_mon <- df_mon %>% mutate(cenken = map(data, cenken_month)) %>%
                             select(month, cenken) %>% ungroup()

    trends <- trends_mon %>% hoist(cenken, slope = "slope", intercept = "intercept", pval="p") %>%
                             select(month, slope, intercept, pval) %>%
                             mutate(message = ifelse(pval  >0 & pval <0.05 & slope > 0,
                                                paste0("There was a significant increasing trend of ",
                                                paste0(round(slope, 4)), " ", paste0(paramunits), " of ",  
                                                paste0(displayname), " per year for ", month, "."),
                                                  ifelse(pval > 0 & pval <0.05 & slope < 0,
                                                    paste0("There was a significant decreasing trend of ",
                                                    paste0(round(slope,4)), " ", paste0(paramunits), " of ",  
                                                    paste0(displayname), " per year for ", month, "."),
                                              paste0("no trend"))),
                               modeled = TRUE)

    results <- if(all(is.na(month_drops$month)) & nrow(trends)>0){
                  trends
                } else if(all(is.na(month_drops$month)) & nrow(trends)==0){
                  data.frame(slope = NA, intercept = NA, pval = NA,
                             message = paste0("Too few data points."),
                             modeled = FALSE)
                } else if(!any(is.na(month_drops$month)) & nrow(trends)>0){
                  rbind(trends, month_drops)
                } else if(!any(is.na(month_drops$month)) & nrow(trends)==0){
                  month_drops
    }

  }

  results_final <- results %>% droplevels() %>% arrange(month)
  return(results_final)
})
