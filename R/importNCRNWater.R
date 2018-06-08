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
#' @importFrom dplyr distinct mutate filter rename select ungroup
#' @importFrom lubridate mdy
#' @importFrom magrittr %>%
#' @importFrom purrr  map map2 pmap
#' @importFrom readr read_csv
#' 
#' @export
#' @export %>%


importNCRNWater<-function(Dir, Data="Water Data.csv", MetaData="MetaData.csv"){
  
#### Read in Data ####
  Indata<-read_csv(paste(Dir,Data, sep="/"), col_types="ccccc") %>% 
    rename(SiteCode=StationID, Date=`Visit Start Date`,Value=`Result Value/Text`, Characteristic=`Local Characteristic Name`)
  
  MetaData<-read_csv(paste(Dir,MetaData, sep="/"))
  
  
#### Get data ready to make into objects ####
  
  Indata$Date<-mdy(Indata$Date)

#### Create Data part of each charactersitic ####
  MetaData$Data<-MetaData %>% dplyr::select(SiteCode, DataName) %>% 
    pmap(.f=function(SiteCode, DataName) {
    filter(Indata, SiteCode==!!SiteCode, Characteristic==DataName) %>% 
              dplyr::select(Date,Value)  })

#### Change numeric data to numeric, but the leave the rest as character ####
  NumDat<-MetaData$DataType=="numeric"
  MetaData[NumDat,]$Data<-MetaData[NumDat,]$Data %>% map(.f=function(x) mutate(x,Value = as.numeric(Value) )) 

#### Create Characteristic objects ####
  MetaData$Characteristics<-MetaData %>% 
    dplyr::select(CharacteristicName, DisplayName, Units, LowerPoint, UpperPoint, LowerDescription, 
          UpperDescription, AssessmentDetails, Data) %>% 
    pmap(.f=new, Class="Characteristic")


#### Create a df of sites with correct Characteristic objects ####

  AllSites<-MetaData %>% group_by(ParkCode, SiteCode, SiteName,Lat,Long,Type) %>%
    summarize(Characteristics=list(Characteristics) %>% list) %>%
    ungroup

  
#### Create Park objects ####
  
   Parks<-MetaData %>% dplyr::select(Network, ParkCode, ShortName, LongName) %>% distinct() %>% 
    pmap(.f=new, Class="Park")

  
###### Make a list of sites with each park a nested list
  PSites<-map(Parks, function(Park){
    SiteDf<-filter(AllSites, ParkCode==getParkInfo(Park, info="ParkCode") ) %>% dplyr::select(-ParkCode)
    SiteList<-pmap(.l=SiteDf, .f=new, Class="Site" )
  })
  
  
### Join Park objects with the sites  
  Parks<-map2(.x=Parks, .y=PSites, .f=function(x,y) {x@Sites<-y
  x} )
  
  return(Parks)
}
