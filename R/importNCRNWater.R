#' @include NCRNWater_Park_Class_def.R
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @title importNCRNWater
#' 
#' @description This function imports data from a .csv files exporterd from NPStoret and saves it as \code{Park} objects. 
#' 
#' @param Dir The directory where the data is found
#' 
#' @return Returns 11 \code{Park} objects, one for each park, as a \code{list}.
#' 
#' @importFrom dplyr mutate rename select filter distinct filter_ ungroup
#' @importFrom lubridate mdy
#' @importFrom magrittr %>%
#' @importFrom purrr invoke_rows map map2 by_row
#' @importFrom readr read_csv
#' 
#' @export
#' @export %>%


importNCRNWater<-function(Dir){
  #### Read in Data ####
  
  OldDir<-getwd()
  setwd(Dir)

  
  Indata<-read_csv("Water Data.csv", col_types="ccccc") %>% 
    rename(SiteCode=StationID, Date=`Visit Start Date`,Value=`Result Value/Text`, Characteristic=`Local Characteristic Name`)
  
  MetaData<-read_csv("MetaData.csv")
  setwd(OldDir)
  
  
  #### Get data ready to make into objects ####
  
  Indata$Date<-mdy(Indata$Date)

  #### Create Data part of each charactersitic ####
  MetaData$Data<-MetaData %>% 
    by_row(..f=function(x) filter_(Indata, .dots=list(~SiteCode==x$SiteCode, ~Characteristic==x$DataName)) %>% 
             dplyr::select(Date,Value), .labels=FALSE, .to="Data" ) %>% unlist(recursive=FALSE)

  #### Change numeric data to numeric, but the leave the rest as character ####
  NumDat<-MetaData$DataType=="numeric"
  MetaData[NumDat,]$Data<-MetaData[NumDat,]$Data %>% map(.f=function(x) mutate(x,Value = as.numeric(Value) )) 
#### Create Characteristic objects ####
  MetaData$Characteristics<-invoke_rows(.d=MetaData %>% 
    dplyr::select(CharacteristicName, DisplayName, Units, LowerPoint, UpperPoint, LowerDescription, 
              UpperDescription, AssessmentDetails, Data),
              .f=new, Class="Characteristic", .labels=F)$.out

  #### Create Site objects with correct Characteristic objects ####
  AllSites<-MetaData %>% group_by(ParkCode, SiteCode, SiteName,Type) %>% 
    summarize(Characteristics=list(Characteristics)) %>% 
    ungroup



#### Make Site list for each Park ####

  AllSites<-MetaData %>% group_by(ParkCode, SiteCode, SiteName,Type) %>%
    summarize(Characteristics=list(Characteristics) %>% list) %>%
    ungroup

  
  #### Create Park objects ####
  
  Parks<-invoke_rows(.d=distinct(.data=MetaData, Network, ParkCode, ShortName, LongName ), .f=new, 
                     .labels=F, Class="Park")[[1]] %>% unlist
  
  
  PSites<-map(Parks, function(Park){
    SiteDf<-filter(AllSites, ParkCode==getParkInfo(Park, info="ParkCode") ) %>% dplyr::select(-ParkCode)
    SiteList<-invoke_rows(.d=SiteDf,.f=mapply, FUN=new, Class="Site" )$.out %>% unlist
  })
  
  Parks<-map2(.x=Parks, .y=PSites, .f=function(x,y) {x@Sites<-y
  x} )
  
  return(Parks)
}
