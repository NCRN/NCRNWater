#' @title compileNETNdata
#' 
#' @description Compile flat files from NETN Access Database for R package 
#' 
#' @section Warning:
#' 
#' @importFrom RODBC odbcConnect sqlFetch sqlTables odbcClose odbcDataSources
#' @importFrom stats setNames
#' @importFrom tidyr pivot_longer separate gather spread
#' @importFrom dplyr  distinct case_when contains filter select mutate rename arrange left_join summarize group_by 
#' @importFrom purrr map
#' @importFrom stringr str_extract str_detect
#' 
#' @param path Quoted path to export csv files to. Defaults to "Data" folder within current project.
#' @param export \code{TRUE} or \code{FALSE}. Export csv files to specified path. Defaults to \code{TRUE}.
#' @param surface \code{TRUE} or \code{FALSE}. Return only measurements representing the stream surface or lake epilimnion. 
#' If \code{TRUE}, the median of the surface measurements from the top 2m of sampling are returned. Defaults to  \code{TRUE}.
#' @param active \code{TRUE} or  \code{FALSE}. If \code{TRUE} only compiles metrics that are actively collected.
#' If \code{FALSE} compiles all metrics stored in the database. Defaults to \code{TRUE}.
#' @param cleanEnv \code{TRUE} or  \code{FALSE}. Allows you to clean the global environment so that 
#'  only waterDat and MD are kept. Defaults to \code{TRUE}.
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
#' @examples
#' # compile NETN water data for surface measurements and active metrics only,
#' # and export to C:/Data.
#' compileNETNdata(path = "C:/Data/", export = TRUE, surface = TRUE, active = TRUE)
#' 
#' @export

compileNETNdata <- function(path = "./Data/", export = TRUE, surface = TRUE, active = TRUE,
           cleanEnv = TRUE) {
    
  if(!requireNamespace("RODBC", quietly = TRUE)){
    stop("Package 'RODBC' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("stringr", quietly = TRUE)){
    stop("Package 'stringr' is needed for this function to work. Please install it.", call. = FALSE)
  }
  
  # Error handling for specified path
  path <- if (substr(path, nchar(path), nchar(path)) != "/") {
    paste0(path, "/")
  } else {(paste0(path))}
  
  # Check that specified path exists on computer
  if(export == TRUE & dir.exists(path) == FALSE){
    stop(paste0("The specified path: ", path, " does not exist"))
  }
  
  # Check that NETNWQ is a named user or system DSN
 
  if(nrow(data.frame(driver=(RODBC::odbcDataSources())) %>% 
    mutate(dsn_name = row.names(.)) %>% 
    filter(dsn_name == "NETNWQ"))==0) 
    stop ('Compile function failed. There is no DSN named "NETNWQ".')
  
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
col_order <- c("NPSTORET.Org.ID.Code", "StationID", "Visit.Start.Date", 
               "SampleDepth", "Depth.Units", "Local.Characteristic.Name", 
               "Result.Value.Text", "Lower.Quantification.Limit", 
               "Upper.Quantification.Limit")

#### Build Chemistry table ----
#select variables of interest from Chemistry table:
Chem <- Chemistry %>%
	select(-c('SEC2016', 'LabCode', 'SampleTime', 'SampleStation', 
	          'SampleType', 'FK_WaterQualityMethod', 'Project', 'pH_Lab', 
	          'eqPH', 'AppColorFlag', 'AppColor_PCU', 'TrueColor_PCU', 
	          'TColor_Flag', 'pH_Lab_Method', 'CONDMETH', 'COLORMETH', 
	          'ALKMETH', 'ChemComments')) 
	
#merge in ancillary info on date and site:
Chem <- merge(Chem, Samples, by.x="FK_Sample", by.y = "PK_Sample")
Chem <- merge(Chem, Events, by.x="FK_Event", by.y = "PK_Event") 
Chem <- merge(Chem, Locations, by.x="FK_Location", by.y = "PK_Location") 

#format Date
Chem$StartDate <- as.POSIXct(Chem$StartDate, format = "%Y-%m-%d")
#filter out everything but QCtype = "ENV", rename columns, re-code Sample Depth:
Chem <- Chem %>%
	filter(QCtype == "ENV") %>%
	rename(StationID=NPStoretSiteCode, 
	       Visit.Start.Date=StartDate, 
	       SampleDepth = SampleDepth_m) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	                               LocationType == 'Pond' ~ 'epilimnion', 
	                               LocationType == 'Lake' ~ 'epilimnion')) %>% 
  arrange(StationID, Visit.Start.Date)
Chem <- Chem[order(Chem$StationID, Chem$Visit.Start.Date),]

#check to be sure there is only measurement (representing the epilimnion) per visit. This takes about 15 seconds to run.
### function to count the number of unique levels of a character variable:
how.many <- function(x){length(unique(x))}

try(if(max(as.vector(tapply(
  Chem$SampleDepth,
  list(Chem$Visit.Start.Date, Chem$StationID),
  how.many)), na.rm = T) > 1)
  stop ('More than one sampling depth per site visit')
)

#split into two dataframes - measurments and quality flags.
Chem_dat <- select(Chem,NPSTORET.Org.ID.Code, StationID, 
                   Visit.Start.Date, SampleDepth,-contains("Flag"),
                   contains("mgL"), contains("ugL"), contains("eqL"))

Chem_flag <- select(Chem, StationID, Visit.Start.Date, contains("Flag"))

#convert both dataframes to long form and remove rows with NA for observation value

Chem_dat_long <- Chem_dat %>%
  pivot_longer(cols= c(-StationID, -NPSTORET.Org.ID.Code, -SampleDepth, -Visit.Start.Date), 
               names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text") %>%
  mutate(Depth.Units = NA) %>% 
  arrange(StationID, Visit.Start.Date, Local.Characteristic.Name )

Chem_flag_long <- Chem_flag %>%
  pivot_longer(cols = c( -StationID,-Visit.Start.Date), names_to = "Flag.Characteristic.Name", 
               values_to = "Flag.Value")%>%
  rename(Visit.Start.Date.F = Visit.Start.Date, StationID.F = StationID) %>% 
  arrange(StationID.F, Visit.Start.Date.F, Flag.Characteristic.Name )
	
#merge together:
Chem_long <- cbind(Chem_dat_long, Chem_flag_long)

#remove NA and create Upper and Lower Quantification Limit columns:
test <- Chem_long %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Upper.Quantification.Limit = NA) %>% 
  mutate(Flag.Value = as.character(Flag.Value))

#extract Lower and Upper limits from Flag.Value column and replace Result.Value.Text with character string if value was outside of qunatification limits:
test <- suppressWarnings(test %>% mutate(limit = str_extract(Flag.Value, "\\-*\\d+\\.*\\d*"), #extracts the numbers
                        Lower.Quantification.Limit = as.numeric(ifelse(str_detect(Flag.Value, "<"), paste(limit), NA)), 
                        Upper.Quantification.Limit = as.numeric(ifelse(str_detect(Flag.Value, ">"), paste(limit), NA)),
                        Result.Value.Text = case_when(!is.na(Lower.Quantification.Limit) ~ paste0("*Present <QL"),
                                                      !is.na(Upper.Quantification.Limit) ~ paste0("*Present >QL"),
                                                      TRUE ~ paste0(Result.Value.Text))) %>% 
                 select(-limit))

#clean up
Chem_long <- test %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%  select(col_order)

#### Build WQ Insitu table ----

#select variables of interest from WQInSitu table:
WQ <- WQInSitu %>%
    	select(PK_WQInSitu, FK_Sample, SampleDepth = Depth_m, BP_mmHg, 
    	       DOsat_pct, DO_mgL, pH, SpCond_uScm,Temp_C, QCType) %>%
    	filter(QCType == "0") %>% #Remove calibratin samples.
    	select(-QCType) %>% 
      left_join(., Samples, by=c("FK_Sample"= "PK_Sample"))%>% # add in ancillary data
      left_join(.,Events,    by=c("FK_Event" = "PK_Event")) %>% 
      left_join(.,Locations, by=c("FK_Location" = "PK_Location")) %>% 
      mutate(StartDate =  as.POSIXct(StartDate, format = "%Y-%m-%d")) %>% 
    	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%#clean up column names, remove rows with SampleDepth = NA
    	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
    	filter(!is.na(SampleDepth)) %>%
    	select(-c(FK_Location, FK_Event, FK_Sample, PK_WQInSitu)) %>% 
      arrange(StationID, Visit.Start.Date)

#make long, remove rows with NA for observation value, record Depth units
WQ_long <- WQ %>%
	select(-LocationType) %>%
	pivot_longer(names_to = "Local.Characteristic.Name", 
	             values_to = "Result.Value.Text", 
	             cols= -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = "m") %>% 
  select(col_order) %>% filter(!is.na(StationID)) #Removed records missing primary key
  # 3 sites in WQInSitu table missing site data.


#calculate median value for all depths 2m or less to represent the stream/epilimnion value.	
temp <- suppressWarnings(
  WQ %>% filter(SampleDepth <= 2) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, 
	       -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	group_by(StationID, Visit.Start.Date, Local.Characteristic.Name) %>%
	dplyr::summarize(Result.Value.Text = median(as.numeric(Result.Value.Text))) %>%
	ungroup()	%>%
	spread(key = Local.Characteristic.Name, value = Result.Value.Text) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN")	%>% 
  left_join(., Locations, by =c("StationID" = "NPStoretSiteCode")) # bind on location type (stream or Lake)
)

temp <- temp %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion')) %>%
	select(-c(PK_Location, LocationType))

#make long, remove rows with NA for observation value, add column for Depth.Units:
temp_long <- temp %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, 
	       -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
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
  mutate(StartDate=  as.POSIXct(StartDate, format = "%Y-%m-%d")) %>% 
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = "stream") %>%
	select(-c(FK_Location, FK_Event, FK_Sample, PK_StreamDischarge, LocationType)) %>% 
  arrange(StationID, Visit.Start.Date)


#make long and remove rows with NA for observation value
SD_long <- SD %>%
	pivot_longer(names_to = "Local.Characteristic.Name", 
	             values_to = "Result.Value.Text", 
	             cols=-c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
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
    mutate(StartDate =  as.POSIXct(StartDate, format = "%Y-%m-%d")) %>% #format Date
  	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
  	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
  	mutate(SampleDepth = "stream") %>%
  	select(-c(FK_Location, FK_Event, FK_Sample, PK_Turbidity, LocationType)) %>% 
    arrange(StationID, Visit.Start.Date)

#make long and remove rows with NA for observation value
Turb_long <- Turb %>%
	pivot_longer(names_to = "Local.Characteristic.Name", 
	             values_to = "Result.Value.Text", 
	             cols= -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
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
  mutate(StartDate =  as.POSIXct(StartDate, format = "%Y-%m-%d")) %>% #format Date
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
	pivot_longer(names_to = "Local.Characteristic.Name", values_to = "Result.Value.Text", 
	             cols= -c(SampleDepth, Visit.Start.Date, StationID, 
	                      NPSTORET.Org.ID.Code, Upper.Quantification.Limit, 
	                      Lower.Quantification.Limit)) %>%
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
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d")) %>% #format Date
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate, SampleDepth = Depth_m) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	select(-c(FK_Location, FK_Event, FK_Sample, PK_LightPenetration)) %>% 
  arrange(StationID, Visit.Start.Date)

#make long and remove rows with NA for observation value
Light_long <- Light %>%
	select(-LocationType) %>%
	pivot_longer(names_to = "Local.Characteristic.Name", 
	             values_to = "Result.Value.Text", 
	             cols= -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = "m") %>% 
  select(col_order)

#calc median light penetration in top 2m:
  ## Note : using gather since multiple data types in the value field (for pivot_longer these need to be specified by value ptypes))
temp2 <- suppressWarnings(
  Light %>%
	filter(SampleDepth <= 2) %>%
	gather(key = Local.Characteristic.Name, 
	       value = Result.Value.Text, 
	       -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	group_by(StationID, Visit.Start.Date, Local.Characteristic.Name) %>%
	dplyr::summarize(Result.Value.Text = median(as.numeric(Result.Value.Text))) %>%
	ungroup() %>%
	spread(key = Local.Characteristic.Name, value = Result.Value.Text) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN")	%>% 
  left_join(., Locations, by= c("StationID"= "NPStoretSiteCode")) %>% # bind liocation type
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	                               LocationType == 'Pond' ~ 'epilimnion', 
	                               LocationType == 'Lake' ~ 'epilimnion')) %>%
	select(-c(PK_Location, LocationType))
  )

#make long, remove rows with NA for observation value, add column for Depth.Units:
temp2_long <- temp2 %>%
	pivot_longer(names_to  = "Local.Characteristic.Name", 
	             values_to =  "Result.Value.Text", 
	             cols= -c(SampleDepth, Visit.Start.Date, StationID,
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

waterDat <- Reduce(function(x,y) rbind(x,y), 
                   list(Chem_long, WQ_long, SD_long, Turb_long, Sec_long, Light_long))

waterDat <- waterDat %>% filter(!is.na(Result.Value.Text) & Result.Value.Text!="NA") # Dataset created a lot of site/param combinations that don't have data

### Retain or exclude measurements by depth? If TRUE, only the median surface value from the top 2m are returned.

# write data to new object to use for constructing metadata file below so that subsetting by surface measurements won't affect output and colname changing isn't a problem.
waterDatBkup <- waterDat

if(surface == TRUE){
  #select only 'stream', 'epilimnion', NA Sample Depths, or depths <2m.
  waterDat <- waterDat %>%
    filter(SampleDepth == 'stream' | SampleDepth == 'epilimnion' ) #| is.na(SampleDepth) | as.numeric(SampleDepth) <2)
} # MIMA sites have number recorded instead of factor level

#replace "." with " " or "/" in column names to match what importNCRNWater expects:
names(waterDat) <- c('Network', 'StationID', 'Visit Start Date', 
                     'SampleDepth', 'Depth Units','Local Characteristic Name', 
                     'Result Value/Text', 'Lower Quantification Limit', 
                     'Upper Quantification Limit')

#### Create Metadata.csv ----
#Each row represents a variable (characteristic) at a location.

## create look-up table of sites and their location metadata
sites <- unique(waterDatBkup$StationID)
sites.temp <- merge(Location, tluParkCode, by = "ParkCode", all.x=T, all.y=F)
sites_tlu <- sites.temp %>%
  rename(ShortName = PARKNAME, Type = LocationType, 
         SiteCode = NPStoretSiteCode, SiteName = ShortSiteName, 
         Lat = StartLat_DD, Long = StartLon_DD) %>%
  mutate(Network = "NETN") %>%
  mutate(LongName = paste(ShortName, PARKTYPE, sep = " ")) %>%
  mutate(Type = case_when(Type == "Pond" ~ "Lake", ##change Pond to Lake for further handling; have to change all values
                          Type == "Lake" ~ "Lake",
                          Type == "Stream" ~ "Stream")) %>% 
  dplyr::filter(SiteCode %in% sites) %>%
  select(c(Network, ParkCode, ShortName, LongName, SiteCode, SiteName, Lat, Long, Type))

#create rows only for the variables measured at each site, extract units, and add in site names

MD <- suppressWarnings(
  waterDatBkup %>% select(SiteCode = StationID, CharacteristicName= Local.Characteristic.Name) %>% 
  mutate(SiteCode = as.character(SiteCode)) %>% 
  group_by(SiteCode) %>% 
  distinct() %>%
  separate(CharacteristicName, c("CategoryDisplay","Units"), sep="_", remove= FALSE) %>%
  left_join(sites_tlu,.,by= "SiteCode") # join in site metadata
)

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

MD$LowerPoint <- as.numeric(NA) #needs to be Num
MD$UpperPoint <- as.numeric(NA) #needs to be Num
MD$DataName <- MD$CharacteristicName
MD$Category <- MD$DisplayName

#-------
# Lower and upper assessment points based on Tables in 2015 reports
# Create matrix that is joined to MD
Reg8 <- c("ACAD", "MABI", "SAGA")

MD <- MD %>% mutate(
  LowerPoint = case_when(ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "SDepth1_m" ~ 0.93,
                         ParkCode == "MABI" & Type == "Stream" & CharacteristicName == "DO_mgL" ~ 7,
                         ParkCode == "MABI" & Type == "Stream" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "MABI" & Type == "Lake" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "SAGA" & Type == "Stream" & CharacteristicName == "DO_mgL" ~ 6,
                         ParkCode == "SAGA" & Type == "Lake" & CharacteristicName == "DO_mgL" ~ 5,
                         ParkCode == "SAGA" & Type == "Stream" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "SAGA" & Type == "Lake" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "MIMA" & SiteName == "Concord River" & 
                           CharacteristicName == "DO_mgL" ~ 6.0,
                         ParkCode == "MIMA" & SiteName != "Concord River" & 
                           CharacteristicName == "DO_mgL" ~ 5.0,
                         ParkCode == "MIMA" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "SAIR" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "SARA" & CharacteristicName == "DO_mgL" ~ 4.0,
                         ParkCode == "SARA" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "ROVA" & CharacteristicName == "DO_mgL" ~ 4.0,
                         ParkCode == "ROVA" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "WEFA" & CharacteristicName == "DO_mgL" ~ 5.0,
                         ParkCode == "WEFA" & CharacteristicName == "pH" ~ 6.5,
                         ParkCode == "WEFA" & CharacteristicName == "SDepth1_m" ~ 4.5,
                         ParkCode == "MORR" & CharacteristicName == "DO_mgL" ~ 7.0,
                         ParkCode == "MORR" & CharacteristicName == "pH" ~ 6.5),
  
  UpperPoint = case_when(ParkCode %in% Reg8 & Type == "Stream" & CharacteristicName == "TP_ugL" ~ 10.0,
                         ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "TP_ugL" ~ 8.0,
                         ParkCode %in% Reg8 & Type == "Stream" & CharacteristicName == "TN_mgL" ~ 0.38,
                         ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "TN_mgL" ~ 0.24,
                         ParkCode %in% Reg8 & Type == "Stream" & CharacteristicName == "ChlA_ugL" ~ 0.63,
                         ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "ChlA_ugL" ~ 2.43,
                         ParkCode == "MABI" & Type == "Stream" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "MABI" & Type == "Lake" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "SAGA" & Type == "Stream" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "SAGA" & Type == "Lake" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "MIMA" & SiteName == "Concord River" &
                           CharacteristicName == "Temp_C" ~ 28.3,
                         ParkCode == "MIMA" & SiteName != "Concord River" &
                           CharacteristicName == "Temp_C" ~ 20.3,
                         ParkCode == "MIMA" & CharacteristicName == "pH" ~ 8.3,
                         ParkCode == "MIMA" & CharacteristicName == "TP_ugL" ~ 31.25,
                         ParkCode == "MIMA" & CharacteristicName == "TN_mgL" ~ 0.71,
                         ParkCode == "SAIR" & CharacteristicName == "pH" ~ 8.3,
                         ParkCode == "SAIR" & CharacteristicName == "TP_ugL" ~ 31.25,
                         ParkCode == "SAIR" & CharacteristicName == "TN_mgL" ~ 0.71,
                         ParkCode == "SAIR" & CharacteristicName == "Temp_C" ~ 28.3,
                         ParkCode == "SARA" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "SARA" & CharacteristicName == "TN_mgL" ~ 0.54,
                         ParkCode == "ROVA" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "ROVA" & CharacteristicName == "TP_ugL" ~ 33.0,
                         ParkCode == "ROVA" & CharacteristicName == "TN_mgL" ~ 0.54,
                         ParkCode == "WEFA" & CharacteristicName == "Temp_C" ~ 29.4,
                         ParkCode == "WEFA" & CharacteristicName == "pH" ~ 8.0,
                         ParkCode == "WEFA" & CharacteristicName == "TP_ugL" ~ 8.0,
                         ParkCode == "WEFA" & CharacteristicName == "TN_mgL" ~ 0.32,
                         ParkCode == "WEFA" & CharacteristicName == "ChlA_ugL" ~ 2.9,
                         ParkCode == "MORR" & CharacteristicName == "Temp_C" ~ 22.0,
                         ParkCode == "MORR" & CharacteristicName == "pH" ~ 8.5,
                         ParkCode == "MORR" & CharacteristicName == "TP_ugL" ~ 36.56,
                         ParkCode == "MORR" & CharacteristicName == "TN_mgL" ~ 0.69,
                         ParkCode == "MORR" & CharacteristicName == "SO4_ueqL" ~ 5200,
                         ParkCode == "MORR" & CharacteristicName == "Turbidity_NTU" ~ 50),
  Units = ifelse(CharacteristicName == "pH", paste0("(pH units) "), paste0(Units))
)

MD$LowerDescription <- ifelse(!is.na(MD$LowerPoint),
                              paste0("Acceptable ", MD$DisplayName, 
                                     " is above ", MD$LowerPoint, " ", 
                                     ifelse(MD$DisplayName != "pH", paste0(MD$Units,"."), paste0("."))
                                    ), paste0(NA))
#                              paste0("Acceptable lower limits have not been established for this parameter."))
MD$UpperDescription <- ifelse(!is.na(MD$UpperPoint),
                              paste0("Acceptable ", MD$DisplayName, 
                                     " is below ", MD$UpperPoint, " ", 
                                     ifelse(MD$DisplayName != "pH", paste0(MD$Units,"."), paste0("."))
                              ), paste0(NA))
#                              paste0("Acceptable upper limits have not been established for this parameter."))

MD$AssessmentDetails <- ifelse(!is.na(MD$LowerPoint) | !is.na(MD$UpperPoint),
                               paste0("See Table 3 of Gawley et al. 2016 for more details."), 
                               NA)

#------
MD <- MD %>% select(Network,
                    ParkCode,
                    ShortName,
                    LongName,
                    SiteCode,
                    SiteName,
                    Lat,
                    Long,
                    Type,
                    CharacteristicName,
                    DisplayName,
                    DataName,
                    Category,
                    CategoryDisplay,
                    Units,
                    LowerPoint,
                    UpperPoint,
                    DataType,
                    LowerDescription,
                    UpperDescription,
                    AssessmentDetails)

# Identify and handle metrics within a park that have censored data
waterDat <- waterDat %>% mutate(
    `STORET Characteristic Name` = `Local Characteristic Name`,
    `Visit Start Date` = format(`Visit Start Date`, "%m/%d/%Y")) %>%
  select(
    StationID,
    `Visit Start Date`,
    `Local Characteristic Name`,
    SampleDepth,
    `Depth Units`,
    `Result Value/Text`,
    `Lower Quantification Limit`, 
    `Upper Quantification Limit`) %>% 
  filter(!is.na(`Result Value/Text`) & `Result Value/Text`!="NA") %>% 
  droplevels() %>% 
  rename(MQL = `Lower Quantification Limit`,
         UQL = `Upper Quantification Limit`)

waterDat <- waterDat %>% 
  mutate(Censored = ifelse(grepl("\\*", `Result Value/Text`) & 
                                                    (!is.na(MQL) | !is.na(UQL)), TRUE, FALSE),
                                
         ValueCen = case_when(!grepl("\\*", `Result Value/Text`) ~ paste(`Result Value/Text`),
                               grepl("\\*", `Result Value/Text`) & !is.na(MQL) ~ paste(MQL),
                               grepl("\\*", `Result Value/Text`) & !is.na(UQL) ~ paste(UQL)))

active_metrics <- c('AL_ugL', 'ANC_ueqL', 'BP_mmHg', 'Ca_ueqL', 'ChlA_ugL', 
                    'Cl_ueqL', 'Discharge_cfs', 'DO_mgL', 'DOC_mgL', 
                    'DOsat_pct', 'K_ueqL', 'Mg_ueqL', 'Na_ueqL', 'NH4_mgL',
                    'NO3_ueqL', 'PenetrationRatio', 'pH', 'SDepth1_m',
                    'SO4_ueqL', 'SpCond_uScm', 'Temp_C','TN_mgL',
                    'TP_ugL')

if(active == TRUE){
  waterDat <- waterDat %>% filter(`Local Characteristic Name` %in% active_metrics) %>% droplevels()
} else {waterDat}

if(active == TRUE){
  MD <- MD %>% filter(CharacteristicName %in% active_metrics) %>% droplevels()
} else {MD}

assign("waterDat", waterDat, .GlobalEnv)
assign("MD", MD, .GlobalEnv)

if (export == TRUE) {
  #write out data as 'Water Data.csv':
  write.csv(waterDat, paste0(path, "Water Data.csv"),
            row.names = FALSE)
  
  #write out Metadata in wide form...
  write.csv(MD, paste0(path, "VizMetaData.csv"),
            row.names = FALSE)
  cat(paste0("Water Data.csv and VizMetaData.csv successfully saved to: ",
               path))
} else {
  cat("NETN water data successfully compiled as 
      waterDat and MD in global environment.")
} 
 if(cleanEnv == TRUE){
   objs <- ls(pos = ".GlobalEnv")
   objs <- objs[!objs %in% c("waterDat","MD")]
   rm(list = objs, pos = ".GlobalEnv")
 }

#return(list(waterDat, MD))
} #end of function
