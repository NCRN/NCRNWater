#' @include NCRNWater_Park_Class_def.R
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @title importNCRNWater
#' 
#' @description This function imports data from a .csv files exporterd from NPStoret and saves it as \code{Park} objects. 
#' 
#' @param Dir The directory where the data is found
#' @param Data The data file. Defaults to "Water Data.csv"
#' @param MetaData The metadata file. Defaults to "MetaData.csv"
#' 
#' @return Returns \code{Park} objects, one for each park, as a \code{list}.
#' 
#' @importFrom dplyr mutate rename select filter distinct filter_ ungroup
#' @importFrom lubridate mdy
#' @importFrom magrittr %>%
#' @importFrom purrr  map map2
#' @importFrom readr read_csv
#' @importFrom purrrlyr invoke_rows
#' 
#' @export
#' @export %>%


importNCRNWater<-function(Dir, Data="Water Data.csv", MetaData="MetaData.csv"){
  #### Read in Data ####
  
  OldDir<-getwd()
  setwd(Dir)

  
  Indata<-read_csv(Data, col_types="ccccc") %>% 
    rename(SiteCode=StationID, Date=`Visit Start Date`,Value=`Result Value/Text`, Characteristic=`Local Characteristic Name`)
  
  MetaData<-read_csv(MetaData)
  setwd(OldDir)
  
  
  #### Get data ready to make into objects ####
  
  Indata$Date<-mdy(Indata$Date)

  #### Create Data part of each charactersitic ####
  MetaData$Data<-MetaData %>% select(SiteCode, DataName) %>% 
    pmap(.f=function(SiteCode, DataName) {
    filter(Indata, SiteCode==!!SiteCode, Characteristic==DataName) %>% 
              dplyr::select(Date,Value)  })
  return(MetaData)
  #### Change numeric data to numeric, but the leave the rest as character ####
  NumDat<-MetaData$DataType=="numeric"
  MetaData[NumDat,]$Data<-MetaData[NumDat,]$Data %>% map(.f=function(x) mutate(x,Value = as.numeric(Value) )) 
#### Create Characteristic objects ####
  MetaData$Characteristics<-invoke_rows(.d=MetaData %>% 
    dplyr::select(CharacteristicName, DisplayName, Units, LowerPoint, UpperPoint, LowerDescription, 
              UpperDescription, AssessmentDetails, Data),
              .f=new, Class="Characteristic", .labels=F)$.out

  #### Create Site objects with correct Characteristic objects ####
  AllSites<-MetaData %>% group_by(ParkCode, SiteCode, SiteName,Lat,Long,Type) %>% 
    summarize(Characteristics=list(Characteristics)) %>% 
    ungroup


#### Make Site list for each Park ####

  AllSites<-MetaData %>% group_by(ParkCode, SiteCode, SiteName,Lat,Long,Type) %>%
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
