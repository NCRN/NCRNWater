#' @title compileNETNdata
#' 
#' @description Compile flat files from NETN Access Database for R package 
#' 
#' @section Warning:
#' 
#' @importFrom RODBC odbcConnect sqlFetch sqlTables odbcClose
#' @importFrom stats setNames
#' @importFrom tidyr pivot_longer separate gather spread
#' @importFrom dplyr  distinct case_when contains
#' @importFrom purrr map
#' 
#' @param export  \code{TRUE} or  \code{FALSE}. Export csv files to "Data" folder. Defaults to \code{TRUE}.
#' @param surface   \code{TRUE} or  \code{FALSE}. Return only measurements representing the stream surface or lake epilimnion. 
#' If  \code{TRUE}, the median of the surface measurements from the top 2m of sampling are returned. Defaults to  \code{TRUE}.
#' 
#' @return Returns a list of two dataframes (WaterData and MetaData) formatted for import into NCRNwater R package.Exports data to folder by default.
#' @details This function create flat files from NETN's water database. The function returns 2 data frames used for the 
#' NCRNWater R package (Water Data and Meta Data).
#' Currently the Water Data list element can include (1) measurements at all sampled depths (for plotting profiles) AND the 
#'  the median value of measurements taken within the top 2m of the water surface when \code{surface == FALSE} or 
#'  (2) just the median values of  surface measurements within the top 2m of the water surface  \code{surface == TRUE}.
#' The latter is to enable examining patterns in the stream surface or lake epilimnion over time and is currently (as of 11/1/2019) the data working 
#' in the NCRNwater package. 
#' 
#' @export


compileNETNdata<-function(export = TRUE, surface = TRUE){

#########################
#Import water data from Access database:

# Connect to database BE ti bring in tables

con <- odbcConnect("NETNWQ")

# grab all tables name from DB
tableList<-sqlTables(con)$TABLE_NAME %>% as.vector()

# BUILD LIST OF tables from DB connection
dfList <- map(tableList, function(x) sqlFetch(con, sqtable=  x, rows_at_time = 1 ))
dfList <- setNames(dfList, tableList) # name each of the list tables
               
odbcClose(con) # close ODBC connection

#put a dataframe for each DB table from the list object  in the Global Environment:
list2env(dfList, envir=.GlobalEnv) # extract separate df's to environment

##### create Water Data.csv ----
#create data frame in the template 'Water Data.csv' for the NCRN Water R package and Rshiny WaterViz:

#prep ancillary data for merging:
Samples <- Sample %>%
	select(c('PK_Sample', 'FK_Event'))
Events <- Event %>%
	select(c('PK_Event', 'FK_Location', 'StartDate'))
Locations <- Location %>%
	select(c('PK_Location', 'NPStoretSiteCode', 'LocationType'))

#set column order for 'Water data.csv', long form:
col_order <- c("NPSTORET.Org.ID.Code", "StationID", "Visit.Start.Date", "SampleDepth", "Depth.Units", "Local.Characteristic.Name", "Result.Value.Text", "Lower.Quantification.Limit", "Upper.Quantification.Limit")

#### Build Chemistry table ----
#select variables of interest from Chemistry table:
Chem <- Chemistry %>%
	select(-c('SEC2016', 'LabCode', 'SampleTime', 'SampleStation', 'SampleType', 'FK_WaterQualityMethod', 'Project', 'pH_Lab', 'eqPH', 'AppColorFlag', 'AppColor_PCU', 'TrueColor_PCU', 'TColor_Flag', 'pH_Lab_Method', 'CONDMETH', 'COLORMETH', 'ALKMETH', 'ChemComments')) 
	
#merge in ancillary info on date and site:
Chem <- merge(Chem, Samples, by.x="FK_Sample", by.y = "PK_Sample")
Chem <- merge(Chem, Events, by.x="FK_Event", by.y = "PK_Event") 
Chem <- merge(Chem, Locations, by.x="FK_Location", by.y = "PK_Location") 
#format Date
Chem$StartDate <- as.Date(Chem$StartDate, format = "%m/%d/%y")
#filter out everything but QCtype = "ENV", rename columns, re-code Sample Depth:
Chem <- Chem %>%
	filter(QCtype == "ENV") %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate, SampleDepth = SampleDepth_m) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion')) %>% 
  arrange(StationID, Visit.Start.Date)
Chem <- Chem[order(Chem$StationID, Chem$Visit.Start.Date),]


#check to be sure there is only measurement (representing the epilimnion) per visit. This takes about 15 seconds to run.
### function to count the number of unique levels of a character variable:
how.many <- function(x){length(unique(x))}
try(if(max(as.vector(tapply(Chem$SampleDepth, list(Chem$Visit.Start.Date, Chem$StationID), how.many)), na.rm=T) > 1) stop ('More than one sampling depth per site visit'))

#split into two dataframes - measurments and quality flags.
Chem_dat <- select(Chem,NPSTORET.Org.ID.Code, StationID, Visit.Start.Date, SampleDepth,-contains("Flag"),contains("mgL"), contains("ugL"), contains("eqL"))

Chem_flag <- select(Chem, StationID, Visit.Start.Date, contains("Flag"))

#convert both dataframes to long form and remove rows with NA for observation value

Chem_dat_long <- Chem_dat %>%
  pivot_longer(cols= c(-StationID, -NPSTORET.Org.ID.Code, -SampleDepth, -Visit.Start.Date), names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text") %>%
  mutate(Depth.Units = NA) %>% 
  arrange(StationID, Visit.Start.Date,Local.Characteristic.Name )

Chem_flag_long <- Chem_flag %>%
  pivot_longer(cols = c( -StationID,-Visit.Start.Date), names_to =  "Flag.Characteristic.Name", values_to = "Flag.Value")%>%
  rename(Visit.Start.Date.F = Visit.Start.Date, StationID.F = StationID) %>% 
  arrange(StationID.F, Visit.Start.Date.F,Flag.Characteristic.Name )
	
#merge together:
Chem_long <- cbind(Chem_dat_long, Chem_flag_long)

#remove NA and create Upper and Lower Quantification Limit columns:
test <- Chem_long %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Upper.Quantification.Limit = NA) %>% 
  mutate(Flag.Value = as.character(Flag.Value))

#extract Lower and Upper limits from Flag.Value column and replace Result.Value.Text with character string if value was outside of qunatification limits:
#unique(test$Flag.Value)
#lower MRL
LDL_rows <- setdiff(grep("MRL", test$Flag.Value), grep("E", test$Flag.Value)) #determine rows with flags
LDL_vals <-sapply(strsplit(test$Flag.Value[LDL_rows], ""), "[",2)
test$Lower.Quantification.Limit <- replace(test$Lower.Quantification.Limit, LDL_rows, LDL_vals)
test$Result.Value.Text[!is.na(test$Lower.Quantification.Limit)] <- '*Present <QL'

#replace lower MDL values (until the codes are standardized in the ACCESS database):
test$Lower.Quantification.Limit <- ifelse(
	test$Flag.Value == 'MDL<0.005', 0.005, ifelse(
	test$Flag.Value == '<MDL 0.005', 0.005, ifelse(
	test$Flag.Value == '<MDL 1', 1, ifelse(
	test$Flag.Value == 'MDL<1', 1, ifelse(
	test$Flag.Value == '<MDL 0.73', 0.73,test$Lower.Quantification.Limit)))))

#upper QL
UDL_rows <- union(grep("UDL", test$Flag.Value), grep("ULQ", test$Flag.Value)) # find rows with UQL
UDL_vals <- rep(2.9, length(UDL_rows))# create vector of UQL; this will need to be changed when there are >1 UQL!
test$Upper.Quantification.Limit <- replace(test$Upper.Quantification.Limit, UDL_rows, UDL_vals) # add in UQL values
test$Result.Value.Text[!is.na(test$Upper.Quantification.Limit)] <- '*Present >QL'

#clean up
Chem_long <- test %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	select(-c(StationID.F, Visit.Start.Date.F, Flag.Characteristic.Name, Flag.Value)) %>% 
  select(col_order)

#### Build WQ Insitu table ----

#select variables of interest from WQInSitu table:
WQ <- WQInSitu %>%
    	select(PK_WQInSitu, FK_Sample,SampleDepth=  Depth_m,BP_mmHg, DOsat_pct, DO_mgL, pH, SpCond_uScm,Temp_C, QCType) %>%
    	filter(QCType == "0") %>% #Remove calibratin samples.
    	select(-QCType) %>% 
      left_join(., Samples, by=c("FK_Sample"= "PK_Sample"))%>% # add in ancillary data
      left_join(.,Events,    by=c("FK_Event" = "PK_Event")) %>% 
      left_join(.,Locations, by=c("FK_Location" = "PK_Location")) %>% 
      mutate(StartDate = as.Date(StartDate, format = "%m/%d/%y")) %>% 
    	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%#clean up column names, remove rows with SampleDepth = NA
    	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
    	filter(!is.na(SampleDepth)) %>%
    	select(-c(FK_Location, FK_Event, FK_Sample, PK_WQInSitu)) %>% 
      arrange(StationID, Visit.Start.Date)

#make long, remove rows with NA for observation value, record Depth units
WQ_long <- WQ %>%
	select(-LocationType) %>%
	pivot_longer(names_to = "Local.Characteristic.Name", values_to =  "Result.Value.Text", cols= -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = "m") %>% 
  select(col_order)

#calculate median value for all depths 2m or less to represent the stream/epilimnion value.	
temp <- WQ %>%
	filter(SampleDepth >= 2) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	group_by(StationID, Visit.Start.Date, Local.Characteristic.Name) %>%
	dplyr::summarize(Result.Value.Text = median(as.numeric(Result.Value.Text))) %>%
	ungroup() 	%>%
	spread(key = Local.Characteristic.Name, value = Result.Value.Text) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN")	%>% 
  left_join(., Locations, by =c("StationID" = "NPStoretSiteCode")) # bind on location type (stream or Lake)

temp <- temp %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion')) %>%
	select(-c(PK_Location, LocationType))

#make long, remove rows with NA for observation value, add column for Depth.Units:
temp_long <- temp %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA) %>% 
  select(col_order)
  
#add summaries to WQ_long df:
WQ_long <- rbind(WQ_long, temp_long)

#### Handle Discharge data ----
#select variables of interest from StreamDischarge table:

SD <- StreamDischarge %>%
	select(c('PK_StreamDischarge', 'FK_Sample', 'Discharge_cfs')) %>% 
  left_join(., Samples, by=c("FK_Sample" = "PK_Sample")) %>% 
  left_join(., Events, by =c("FK_Event"= "PK_Event")) %>% 
  left_join(., Locations, by=c("FK_Location"= "PK_Location")) %>% 
  mutate(StartDate= as.Date(StartDate, format = "%m/%d/%y")) %>% 
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = NA) %>%
	select(-c(FK_Location, FK_Event, FK_Sample, PK_StreamDischarge, LocationType)) %>% 
  arrange(StationID, Visit.Start.Date)


#make long and remove rows with NA for observation value
SD_long <- SD %>%
	pivot_longer(names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text", cols=-c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA) %>% 
  select(col_order)

##### Turbidity table ----
#select variables of interest from Turbidity table:
Turb <- Turbidity %>%
  	select(c('PK_Turbidity', 'FK_Sample', 'Turbidity_NTU')) %>% 
    left_join(.,Samples, by =c("FK_Sample"= "PK_Sample")) %>% #merge in ancillary info on date and site:
    left_join(., Events, by=c("FK_Event" = "PK_Event")) %>% 
    left_join(., Locations, by=c("FK_Location" = "PK_Location")) %>% 
    mutate(StartDate = as.Date(StartDate, format = "%m/%d/%y") ) %>% #format Date
  	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
  	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
  	mutate(SampleDepth = NA) %>%
  	select(-c(FK_Location, FK_Event, FK_Sample, PK_Turbidity, LocationType)) %>% 
    arrange(StationID, Visit.Start.Date)

#make long and remove rows with NA for observation value
Turb_long <- Turb %>%
	pivot_longer(names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text", cols= -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA) %>% 
  select(col_order)

#### Secchi Depth ----
#select variables of interest from Secchi table:
Sec <- Secchi %>%
	select(c('PK_Secchi', 'FK_Sample','SDepth1_m', 'Bot_SD1')) %>% 
  left_join(.,Samples, by =c("FK_Sample"= "PK_Sample")) %>% #merge in ancillary info on date and site:
  left_join(., Events, by=c("FK_Event" = "PK_Event")) %>% 
  left_join(., Locations, by=c("FK_Location" = "PK_Location")) %>% 
  mutate(StartDate = as.Date(StartDate, format = "%m/%d/%y") ) %>% #format Date
  mutate(Lower.Quantification.Limit = ifelse(Bot_SD1 == 'B', SDepth1_m, NA)) %>% 
  mutate(SDepth1_m = ifelse(Bot_SD1 == 'B', "*Present <QL", SDepth1_m)) %>% 
	rename(StationID= NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion',
	 LocationType == 'Bottom' ~ 'bottom')) %>%
	select(-c(FK_Location, FK_Event, FK_Sample, PK_Secchi, LocationType, Bot_SD1)) %>% 
  arrange(StationID, Visit.Start.Date)


#make long and remove rows with NA for observation value
Sec_long <- Sec %>%
	pivot_longer(names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text", cols= -c(SampleDepth,
                Visit.Start.Date, StationID, NPSTORET.Org.ID.Code, Upper.Quantification.Limit, Lower.Quantification.Limit)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Depth.Units = NA) %>% 
  select(col_order)


#### Light Penetration ----
#select variables of interest from LightPenetration table:
Light <- LightPenetration %>%
	select(c('PK_LightPenetration', 'FK_Sample', 'Depth_m', 'PenetrationRatio')) %>%
  left_join(.,Samples, by =c("FK_Sample"= "PK_Sample")) %>% #merge in ancillary info on date and site:
  left_join(., Events, by=c("FK_Event" = "PK_Event")) %>% 
  left_join(., Locations, by=c("FK_Location" = "PK_Location")) %>% 
  mutate(StartDate = as.Date(StartDate, format = "%m/%d/%y") ) %>% #format Date
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate, SampleDepth = Depth_m) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	select(-c(FK_Location, FK_Event, FK_Sample, PK_LightPenetration)) %>% 
  arrange(StationID, Visit.Start.Date)

#make long and remove rows with NA for observation value
Light_long <- Light %>%
	select(-LocationType) %>%
	pivot_longer(names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text", cols= -c(SampleDepth, Visit.Start.Date, StationID, 
                            NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = "m") %>% 
  select(col_order)

#calc median light penetration in top 2m:
  ## Note : using gather since multiple data types in the value field (for pivot_longer these need to be specified by value ptypes))
temp2 <- Light %>%
	filter(SampleDepth >= 2) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	group_by(StationID, Visit.Start.Date, Local.Characteristic.Name) %>%
	dplyr::summarize(Result.Value.Text = median(as.numeric(Result.Value.Text))) %>%
	ungroup() 	%>%
	spread(key = Local.Characteristic.Name, value = Result.Value.Text) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN")	%>% 
  left_join(., Locations, by= c("StationID"= "NPStoretSiteCode")) %>% # bind liocation type
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	LocationType == 'Pond' ~ 'epilimnion', 
	LocationType == 'Lake' ~ 'epilimnion')) %>%
	select(-c(PK_Location, LocationType))


#make long, remove rows with NA for observation value, add column for Depth.Units:
temp2_long <- temp2 %>%
	pivot_longer(names_to  = "Local.Characteristic.Name", values_to =  "Result.Value.Text", cols= -c(SampleDepth, Visit.Start.Date, StationID,
        NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA) %>% 
  select(col_order)

#add median to Light_long df:
Light_long <- rbind(Light_long, temp2_long)

#### Final binding and clean-up of data to create NETN_Water_Data.csv -----
#merge primary observations together
#First, make 'NETN_Water_Data.csv' (full datset with all depths and aggregations). 
#Next, make 'Water Data.csv' (with data for depth profiles removed so that only samples that represent the stream/epilimnion remain) for the NCRN Water data and WaterViz packages.

waterDat <- Reduce(function(x,y) rbind(x,y), list(Chem_long, WQ_long, SD_long, Turb_long, Sec_long, Light_long))


# write data to new object to use for constructing metadata file below so that subsetting by surface measurements won't affect output and colname changing isn't a problem.
waterDatBkup<-waterDat

### Retain or exclude measurements by depth? If TRUE, only the median surface value from the top 2m are returned.

if(surface == TRUE){
  #select only 'stream', 'epilimnion', or NA Sample Depths.
  waterDat <- waterDat %>%
    filter(SampleDepth == 'stream' | SampleDepth == 'epilimnion' | is.na(SampleDepth))
}

#replace "." with " " or "/" in column names to match what importNCRNWater expects:
names(waterDat) <- c('Network', 'StationID', 'Visit Start Date', 'SampleDepth', 'Depth Units','Local Characteristic Name', 'Result Value/Text', 'Lower Quantification Limit', 'Upper Quantification Limit')

#### Create Metadata.csv ----
#Each row represents a variable (characteristic) at a location.

## create look-up table of sites and their location metadata
sites <- unique(waterDatBkup$StationID)
sites.temp <- merge(Location, tluParkCode, by = "ParkCode", all.x=T, all.y=F)
sites_tlu <- sites.temp %>%
  rename(ShortName = PARKNAME, Type = LocationType, SiteCode = NPStoretSiteCode, SiteName = ShortSiteName, Lat = StartLat_DD, Long = StartLon_DD) %>%
  mutate(Network = "NETN") %>%
  mutate(LongName = paste(ShortName, PARKTYPE, sep = " ")) %>%
  mutate(Type = case_when(Type == "Pond" ~ "Lake", ##change Pond to Lake for further handling; have to change all values
                         Type == "Lake" ~ "Lake",
                         Type == "Stream" ~ "Stream")) %>% 
  dplyr::filter(SiteCode %in% sites) %>%
  select(c(Network, ParkCode, ShortName, LongName, SiteCode, SiteName, Lat, Long, Type))

#create rows only for the variables measured at each site, extract units, and add in site names

MD<-waterDatBkup %>% select(SiteCode = StationID, CharacteristicName= Local.Characteristic.Name) %>% 
  mutate(SiteCode = as.character(SiteCode)) %>% 
  group_by(SiteCode) %>% 
  distinct() %>%
  separate(CharacteristicName, c("DataName","Units"), sep="_", remove= FALSE) %>%
  left_join(sites_tlu,.,by= "SiteCode") # join in site metadata


#create Display names
MD <- MD %>%
	mutate(DisplayName = case_when(
	CharacteristicName == "Al_ugL" ~ "Aluminium",
	CharacteristicName == "ANC_ueqL" ~ "Acid Neutralizing Capacity",
	CharacteristicName == "Ca_ueqL" ~ "Calcium",
	CharacteristicName == "Cl_ueqL" ~ "Chloride",
	CharacteristicName == "DOC_mgL" ~ "Dissolved Organic Carbon",
	CharacteristicName == "K_ueqL" ~ "Potassium",
	CharacteristicName == "Mg_ueqL" ~ "Magnesium",
	CharacteristicName == "Na_ueqL" ~ "Sodium",
	CharacteristicName == "NH3_mgL" ~ "Ammonia",
	CharacteristicName == "NH4_mgL" ~ "Ammonium",
	CharacteristicName == "NO3_ueqL" ~ "Nitrate",
	CharacteristicName == "SO4_ueqL" ~ "Sulfate",
	CharacteristicName == "TN_mgL" ~ "Total Nitrogen",
	CharacteristicName == "BP_mmHg" ~ "Air Pressure",
	CharacteristicName == "DOsat_pct" ~ "Dissolved Oxygen",
	CharacteristicName == "DO_mgL" ~ "Dissolved Oxygen",
	CharacteristicName == "pH" ~ "pH",
	CharacteristicName == "SpCond_uScm" ~ "Specific Conductance",
	CharacteristicName == "Temp_C" ~ "Water Temperature",
	CharacteristicName == "SDepth1_m" ~ "Secchi Depth",
	CharacteristicName == "ChlA_ugL" ~ "Chlorophyll A",
	CharacteristicName == "PO4_ugL" ~ "Phosphate",
	CharacteristicName == "TotDissN_mgL" ~ "Total Dissolved Nitrogen",
	CharacteristicName == "TotDissP_ugL" ~ "Total Dissolved Phosphorus",
	CharacteristicName == "TP_ugL" ~ "Total Phosphorus",
	CharacteristicName == "PenetrationRatio" ~ "Light Penetration Ratio",
	CharacteristicName == "NO2+NO3_mgL" ~ "Nitrate + Nitrite",
	CharacteristicName == "Discharge_cfs" ~ "Discharge",
	CharacteristicName == "Turbidity_NTU" ~ "Turbidity",
	CharacteristicName == "NO2_mgL" ~ "Nitrite"	
	))

#create extra columns. Column for Data Type: these are all numeric, so I took a shortcut. Will need to be changed if factor or ordinal data are added.
MD$DataType <- "numeric"
MD$LowerPoint <- 0 #needs to be Num
MD$UpperPoint <- 100 #needs to be Num
MD$LowerDescription <- 'testing'
MD$UpperDescription <- 'testing2'
MD$AssessmentDetails <- 'testing3'
MD$Category <- MD$DataName  
MD$CategoryDisplay <- MD$DisplayName



if (export == TRUE) {

  #write out data as 'Water Data.csv':
  write.csv(waterDat,"./Data/NETN/Water Data.csv" , row.names=FALSE)
  
  #write out Metadata in wide form...
  write.csv(MD,"./Data/NETN/Metadata.csv" , row.names=FALSE)

}else{
    return(list(waterDat,MD))
  }
}
