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
#' @importFrom dplyr rename select filter
#' @importFrom lubridate mdy
#' @importFrom magrittr %>%
#' 
#' @export
#' @export %>%



importNCRNWater<-function(Dir){
  importEnv<-new.env(parent=emptyenv())
  OldDir<-getwd()
  setwd(Dir)

  
  Indata<-read.csv("Water Data.csv", header = T, as.is=T) %>% rename(SiteCode=StationID, Date=Visit.Start.Date,
                                                                     Value=Result.Value.Text, Characteristic=Local.Characteristic.Name)
  setwd(OldDir)
  
  Indata$Date<-mdy(Indata$Date)
  
  
  ANTI<-new("Park",
            ParkCode="ANTI",
            ShortName="Antietam",
            LongName="Antietam National Battlefield",
            Network="NCRN"
  )
  ANTI<-addSite(park=ANTI,SiteCode="NCRN_ANTI_SHCK",SiteName = "Shaprsburg Creek",Coordinates=numeric(),Type="Stream")
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                          Units="\u03bceq/l",
                          Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="ANC" ) %>% 
                            dplyr::select(Date,Value), 
                          LowerPoint=600)
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                          Units="%",
                          Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="DO (%)" ) %>% 
                            dplyr::select(Date,Value) 
                          )
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                          Units="mg/l",
                          Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="DO (mg/L)" ) %>% 
                            dplyr::select(Date,Value), 
                          LowerPoint=5.0)
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="Nitrate", DisplayName="Nitrate",
                          Units="mg/l",
                          Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="Nitrate 2007" ) %>% 
                            dplyr::select(Date,Value), 
                          UpperPoint=0.31)
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="pH", DisplayName="pH",
                          Units="pH Units",
                          Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="pH" ) %>% 
                            dplyr::select(Date,Value), 
                          LowerPoint=6.0, UpperPoint=9.0)
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="SC", DisplayName="Specific Conductance",
                          Units="\u03bcS/cm",
                          Data=get("Indata",sys.frame(1)) %>% 
                            filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="Specific conductance") %>%
                            dplyr::select(Date,Value), 
                          UpperPoint=171)
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                          Units="mg/l",
                          Data=get("Indata",sys.frame(1)) %>%
                            filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                            dplyr::select(Date,Value), 
                          UpperPoint=0.01)
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                          Units="\u00b0C",
                          Data=get("Indata", sys.frame(1)) %>% 
                            filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="Water Temperature" ) %>% 
                            dplyr::select(Date,Value), 
                          UpperPoint=32)
  
  
  CATO<-new("Park", 
            ParkCode="CATO", 
            ShortName="Catoctin", 
            LongName="Catoctin Mountain Park", 
            Network="NCRN"
  ) 
    CATO<-addSite(park=CATO,SiteCode="NCRN_CATO_BGHC",SiteName = "Big Hunting Creek", Coordinates=numeric(),Type="Stream")
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value))
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.31)
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% 
                    filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>%
                    filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.01)
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% 
                    filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=20)
    
    CATO<-addSite(park=CATO,SiteCode="NCRN_CATO_OWCK",SiteName = "Owens Creek", Coordinates=numeric(),Type="Stream")
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value))
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.31)
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>%
                    filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.01)
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=20)
    
    CATO<-addSite(park=CATO,SiteCode="NCRN_CATO_WHST",SiteName = "Blue Blazes Creek", Coordinates=numeric(),Type="Stream")
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.31)
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% 
                    filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.01)
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% 
                    filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=20)
  GREE<-new("Park", 
                ParkCode="GREE", 
                ShortName="Greenbelt", 
                LongName="Greenbelt Park", 
                Network="NCRN"
  ) 
    GREE<-addSite(park=GREE,SiteCode="NCRN_NACE_STCK",SiteName = "Still Creek", Coordinates=numeric(),Type="Stream")
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% 
                    filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="Specific conductance") %>% dplyr::select(Date,Value), 
                  UpperPoint=171)
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      GREE<-addChar(park=GREE, site="NCRN_NACE_STCK", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% 
                    filter(SiteCode=="NCRN_NACE_STCK",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
      
  
  GWMP<-new("Park", 
            ParkCode="GWMP", 
            ShortName="GW Parkway", 
            LongName="George Washington Memorial Parkway", 
            Network="NCRN"
  )
    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_MICR",SiteName = "Minnehaha Creek", Coordinates=numeric(),Type="Stream")
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    
    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_MIRU",SiteName = "Mine Run", Coordinates=numeric(),Type="Stream")
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MIRU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MIRU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)

    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_PIRU",SiteName = "Pimmit Run", Coordinates=numeric(),Type="Stream")
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_PIRU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_PIRU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_TURU",SiteName = "Turkey Run", Coordinates=numeric(),Type="Stream")
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
    GWMP<-addChar(park=GWMP, site="NCRN_GWMP_TURU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_TURU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
  HAFE<-new("Park", 
            ParkCode="HAFE", 
            ShortName="Harpers Ferry", 
            LongName="Harpers Ferry National Histroical Park", 
            Network="NCRN"
  )
    HAFE<-addSite(park=HAFE,SiteCode="NCRN_HAFE_FLSP",SiteName = "Flowing Springs Run", Coordinates=numeric(),Type="Stream")
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=600)
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.31)
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.01)
      HAFE<-addChar(park=HAFE, site="NCRN_HAFE_FLSP", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_HAFE_FLSP",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)

  MANA<-new("Park",
            ParkCode="MANA",
            ShortName="Manassas",
            LongName="Manassas National Battlefield Park", 
            Network="NCRN"
  )
    MANA<-addSite(park=MANA,SiteCode="NCRN_MANA_YOBR",SiteName = "Young's Branch", Coordinates=numeric(),Type="Stream")
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      MANA<-addChar(park=MANA, site="NCRN_MANA_YOBR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MANA_YOBR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
  
  MONO<-new("Park",
            ParkCode="MONO",
            ShortName="Monocacy",
            LongName="Monocacy National Battlefield", 
            Network="NCRN"
  )
    MONO<-addSite(park=MONO,SiteCode="NCRN_MONO_BUCK",SiteName = "Bush Creek", Coordinates=numeric(),Type="Stream")
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=600)
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      MONO<-addChar(park=MONO, site="NCRN_MONO_BUCK", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_BUCK",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    MONO<-addSite(park=MONO,SiteCode="NCRN_MONO_GAMI",SiteName = "Gambrill Mill Creek", Coordinates=numeric(),Type="Stream")
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=600)
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      MONO<-addChar(park=MONO, site="NCRN_MONO_GAMI", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_MONO_GAMI",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    

  NACE<-new("Park",
            ParkCode="NACE",
            ShortName="Nat.Cap.Parks - East",
            LongName="National Captial Parks-East", 
            Network="NCRN"
  )
    NACE<-addSite(park=NACE,SiteCode="NCRN_NACE_HECR",SiteName = "Henson Creek", Coordinates=numeric(),Type="Stream")
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      NACE<-addChar(park=NACE, site="NCRN_NACE_HECR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_HECR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    NACE<-addSite(park=NACE,SiteCode="NCRN_NACE_OXRU",SiteName = "Oxon Run", Coordinates=numeric(),Type="Stream")
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      NACE<-addChar(park=NACE, site="NCRN_NACE_OXRU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_NACE_OXRU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)

  PRWI<-new("Park",
            ParkCode="PRWI",
            ShortName="Prince William",
            LongName="Prince William Forest Park", 
            Network="NCRN"
  )
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_BONE",SiteName = "Boneyard Run", Coordinates=numeric(),Type="Stream")
     PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_BONE", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_BONE",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_CARU",SiteName = "Carters Run", Coordinates=numeric(),Type="Stream")
     PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_CARU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_CARU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_MARU",SiteName = "Mawavi Run", Coordinates=numeric(),Type="Stream")
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_MARU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_MARU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_NFQC",SiteName = "Quantico Creek", Coordinates=numeric(),Type="Stream")
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_NFQC", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_NFQC",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_ORRU",SiteName = "Orenda Run", Coordinates=numeric(),Type="Stream")
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_ORRU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_ORRU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_SFQC",SiteName = "South Fork Quantico Creek", Coordinates=numeric(),Type="Stream")
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen (mg/l) ",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="DO" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SFQC", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SFQC",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_SORU",SiteName = "Sow Run", Coordinates=numeric(),Type="Stream")
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_SORU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_SORU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_TARU",SiteName = "Taylor Run", Coordinates=numeric(),Type="Stream")
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      PRWI<-addChar(park=PRWI, site="NCRN_PRWI_TARU", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_PRWI_TARU",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)

  
  ROCR<-new("Park",
            ParkCode="ROCR",
            ShortName="Rock Creek",
            LongName="Rock Creek Park", 
            Network="NCRN"
  )
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_BAKE",SiteName = "Battery Kemble Creek", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BAKE", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data= get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BAKE",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_BRBR",SiteName = "Broad Branch", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_BRBR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_BRBR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_DUOA",SiteName = "Dumbarton Oaks", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_DUOA", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_DUOA",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
  
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_FEBR",SiteName = "Fenwick Branch", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_FEBR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_FEBR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_HACR",SiteName = "Hazen Creek", Coordinates=numeric(),Type="Stream")
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
    ROCR<-addChar(park=ROCR, site="NCRN_ROCR_HACR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_HACR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_KLVA",SiteName = "Klingle Valley", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_KLVA", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_KLVA",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_LUBR",SiteName = "Luzon Branch", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_LUBR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_LUBR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_NOST",SiteName = "Normanstone Branch", Coordinates=numeric(),Type="Stream")
     ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_NOST", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_NOST",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_PHBR",SiteName = "Pinehurst Branch", Coordinates=numeric(),Type="Stream")
     ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_PHBR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_PHBR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_ROC3",SiteName = "Rock Creek at Dumbarton Oaks", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_ROC3", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_ROC3",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
    
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_SVPS",SiteName = "Soapstone Valley Stream", Coordinates=numeric(),Type="Stream")
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                  Units="\u03bceq/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="ANC" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=200)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=2.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="pH", DisplayName="pH",
                  Units="pH Units",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="pH" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=6.0, UpperPoint=9.0)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="SC", DisplayName="Specific Conductance",
                  Units="\u03bcS/cm",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="Specific conductance") %>%
                    dplyr::select(Date,Value), 
                  UpperPoint=171)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      ROCR<-addChar(park=ROCR, site="NCRN_ROCR_SVPS", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ROCR_SVPS",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=32)
  
  
  WOTR<-new("Park",
            ParkCode="WOTR",
            ShortName="Wolf Trap",
            LongName="Wolf Trap National Park for the Performing Arts", 
            Network="NCRN"
  )
    WOTR<-addSite(park=WOTR,SiteCode="NCRN_WOTR_CHCK",SiteName = "Courthouse Creek", Type="Stream")
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                Units="\u03bceq/l",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="ANC" ) %>% 
                  dplyr::select(Date,Value), 
                LowerPoint=200)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                Units="%",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="DO (%)" ) %>% 
                  dplyr::select(Date,Value) )
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                Units="mg/l",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="DO (mg/L)" ) %>% 
                  dplyr::select(Date,Value), 
                LowerPoint=5.0)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="Nitrate", DisplayName="Nitrate",
                Units="mg/l",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="Nitrate 2007" ) %>% 
                  dplyr::select(Date,Value), 
                UpperPoint=2.0)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="pH", DisplayName="pH",
                Units="pH Units",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="pH" ) %>% 
                  dplyr::select(Date,Value), 
                LowerPoint=6.0, UpperPoint=9.0)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="SC", DisplayName="Specific Conductance",
                Units="\u03bcS/cm",
                Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="Specific conductance") %>%
                  dplyr::select(Date,Value), 
                UpperPoint=171)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                Units="mg/l",
                Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                  dplyr::select(Date,Value), 
                UpperPoint=0.037)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_CHCK", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                Units="\u00b0C",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_CHCK",Characteristic=="Water Temperature" ) %>% 
                  dplyr::select(Date,Value), 
                UpperPoint=32)
  
    WOTR<-addSite(park=WOTR,SiteCode="NCRN_WOTR_WOTR",SiteName = "Wolf Trap Creek", Coordinates=numeric(),Type="Stream")
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="ANC", DisplayName="Acid Neutralizing Capacity",
                Units="\u03bceq/l",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="ANC" ) %>% 
                  dplyr::select(Date,Value), 
                LowerPoint=200)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen",
                Units="%",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="DO (%)" ) %>% 
                  dplyr::select(Date,Value) )
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen",
                Units="mg/l",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="DO (mg/L)" ) %>% 
                  dplyr::select(Date,Value), 
                LowerPoint=5.0)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                Units="mg/l",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="Nitrate 2007" ) %>% 
                  dplyr::select(Date,Value), 
                UpperPoint=2.0)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="pH", DisplayName="pH",
                Units="pH Units",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="pH" ) %>% 
                  dplyr::select(Date,Value), 
                LowerPoint=6.0, UpperPoint=9.0)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="SC", DisplayName="Specific Conductance",
                Units="\u03bcS/cm",
                Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="Specific conductance") %>%
                  dplyr::select(Date,Value), 
                UpperPoint=171)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="Phosphorus", DisplayName="Total Phosphorus",
                Units="mg/l",
                Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="Total Phosphorus 2009" ) %>% 
                  dplyr::select(Date,Value), 
                UpperPoint=0.037)
      WOTR<-addChar(park=WOTR, site="NCRN_WOTR_WOTR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                Units="\u00b0C",
                Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_WOTR_WOTR",Characteristic=="Water Temperature" ) %>% 
                  dplyr::select(Date,Value), 
                UpperPoint=32)
  
  NCRN<-as.list(c(ANTI,CATO,GREE,GWMP,HAFE,MANA,MONO,NACE,PRWI,ROCR,WOTR))
  names(NCRN)<-c("ANTI","CATO","GREE","GWMP","HAFE","MANA","MONO","NACE","PRWI","ROCR","WOTR")
  return(NCRN)
}
