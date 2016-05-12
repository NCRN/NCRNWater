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
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen (%)",
                          Units="%",
                          Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="DO (%)" ) %>% 
                            dplyr::select(Date,Value) 
                          )
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen (mg/l) ",
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
  ANTI<-addChar(park=ANTI, site="NCRN_ANTI_SHCK", CharacteristicName="Phosporus", DisplayName="Total Phosphorus",
                          Units="mg/l",
                          Data=get("Indata",sys.frame(1)) %>%
                            filter(SiteCode=="NCRN_ANTI_SHCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                            dplyr::select(Date,Value), 
                          UpperPoint=0.037)
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
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="DOper", DisplayName="Dissolved Oxygen (%)",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value))
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen (mg/l) ",
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
      CATO<-addChar(park=CATO, site="NCRN_CATO_BGHC", CharacteristicName="Phosporus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>%
                    filter(SiteCode=="NCRN_CATO_BGHC",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
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
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="DOper", DisplayName="Dissolved Oxygen (%)",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value))
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen (mg/l) ",
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
      CATO<-addChar(park=CATO, site="NCRN_CATO_OWCK", CharacteristicName="Phosporus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>%
                    filter(SiteCode=="NCRN_CATO_OWCK",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
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
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="DOper", DisplayName="Dissolved Oxygen (%)",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen (mg/l) ",
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
      CATO<-addChar(park=CATO, site="NCRN_CATO_WHST", CharacteristicName="Phosporus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_CATO_WHST",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
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
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="DOper", DisplayName="Dissolved Oxygen (%)",
                  Units="%",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="DO (%)" ) %>% 
                    dplyr::select(Date,Value) )
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="DOmg", DisplayName="Dissolved Oxygen (mg/l) ",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="DO (mg/L)" ) %>% 
                    dplyr::select(Date,Value), 
                  LowerPoint=5.0)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="Nitrate", DisplayName="Nitrate",
                  Units="mg/l",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Nitrate 2007" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.31)
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
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="Phosporus", DisplayName="Total Phosphorus",
                  Units="mg/l",
                  Data=get("Indata",sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Total Phosphorus 2009" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=0.037)
      GWMP<-addChar(park=GWMP, site="NCRN_GWMP_MICR", CharacteristicName="WaterTemp", DisplayName="Water Temperature",
                  Units="\u00b0C",
                  Data=get("Indata", sys.frame(1)) %>% filter(SiteCode=="NCRN_GWMP_MICR",Characteristic=="Water Temperature" ) %>% 
                    dplyr::select(Date,Value), 
                  UpperPoint=20)
    
    
    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_MIRU",SiteName = "Mine Run", Coordinates=numeric(),Type="Stream")
    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_PIRU",SiteName = "Pimmit Run", Coordinates=numeric(),Type="Stream")
    GWMP<-addSite(park=GWMP,SiteCode="NCRN_GWMP_TURU",SiteName = "Turkey Run", Coordinates=numeric(),Type="Stream")
  
  HAFE<-new("Park", 
            ParkCode="HAFE", 
            ShortName="Harpers Ferry", 
            LongName="Harpers Ferry National Histroical Park", 
            Network="NCRN"
  )
    HAFE<-addSite(park=HAFE,SiteCode="NCRN_HAFE_FLSP",SiteName = "Flowing Springs Run", Coordinates=numeric(),Type="Stream")

  MANA<-new("Park",
            ParkCode="MANA",
            ShortName="Manassas",
            LongName="Manassas National Battlefield Park", 
            Network="NCRN"
  )
    MANA<-addSite(park=MANA,SiteCode="NCRN_MANA_YOBR",SiteName = "Young's Branch", Coordinates=numeric(),Type="Stream")
  
  MONO<-new("Park",
            ParkCode="MONO",
            ShortName="Monocacy",
            LongName="Monocacy National Battlefield", 
            Network="NCRN"
  )
    MONO<-addSite(park=MONO,SiteCode="NCRN_MONO_BUCK",SiteName = "Bush Creek", Coordinates=numeric(),Type="Stream")
    MONO<-addSite(park=MONO,SiteCode="NCRN_MONO_GAMI",SiteName = "Gambrill Mill Creek", Coordinates=numeric(),Type="Stream")

  NACE<-new("Park",
            ParkCode="NACE",
            ShortName="Nat.Cap.Parks - East",
            LongName="National Captial Parks-East", 
            Network="NCRN"
  )
    NACE<-addSite(park=NACE,SiteCode="NCRN_NACE_HECR",SiteName = "Henson Creek", Coordinates=numeric(),Type="Stream")
    NACE<-addSite(park=NACE,SiteCode="NCRN_NACE_OXRU",SiteName = "Oxon Run", Coordinates=numeric(),Type="Stream")
  
  PRWI<-new("Park",
            ParkCode="PRWI",
            ShortName="Prince William",
            LongName="Prince William Forest Park", 
            Network="NCRN"
  )
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_BONE",SiteName = "Boneyard Run", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_cARU",SiteName = "Carters Run", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_MARU",SiteName = "Mawavi Run", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_NFQC",SiteName = "Quantico Creek", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_ORRU",SiteName = "Orenda Run", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_SFQC",SiteName = "South Fork Quantico Creek", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_SORU",SiteName = "Sow Run", Coordinates=numeric(),Type="Stream")
    PRWI<-addSite(park=PRWI,SiteCode="NCRN_PRWI_TARU",SiteName = "Taylor Run", Coordinates=numeric(),Type="Stream")

  
  ROCR<-new("Park",
            ParkCode="ROCR",
            ShortName="Rock Creek",
            LongName="Rock Creek Park", 
            Network="NCRN"
  )
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_BAKE",SiteName = "Battery Kemble Creek", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_BRBR",SiteName = "Broad Branch", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_DUOA",SiteName = "Dumbarton Oaks", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_FEBR",SiteName = "Fenwick Branch", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_HACR",SiteName = "Hazen Creek", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_KLVA",SiteName = "Klingle Valley", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_LUBR",SiteName = "Luzon Branch", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_NOST",SiteName = "Normanstone Branch", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_PHBR",SiteName = "Pinehurst Branch", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_ROC3",SiteName = "Rock Creek at Dumbarton Oaks", Coordinates=numeric(),Type="Stream")
    ROCR<-addSite(park=ROCR,SiteCode="NCRN_ROCR_SVPS",SiteName = "Soapstone Valley Stream", Coordinates=numeric(),Type="Stream")
  
  
  WOTR<-new("Park",
            ParkCode="WOTR",
            ShortName="Wolf Trap",
            LongName="Wolf Trap National Park for the Performing Arts", 
            Network="NCRN"
  )
  WOTR<-addSite(park=WOTR,SiteCode="NCRN_WOTR_CHCK",SiteName = "Courthouse Creek", Type="Stream")
  WOTR<-addSite(park=WOTR,SiteCode="NCRN_WOTR_WOTR",SiteName = "Wolf Trap Creek", Coordinates=numeric(),Type="Stream")
  
  NCRN<-as.list(c(ANTI,CATO,GREE,GWMP,HAFE,MANA,MONO,NACE,PRWI,ROCR,WOTR))
  names(NCRN)<-c("ANTI","CATO","GREE","GWMP","HAFE","MANA","MONO","NACE","PRWI","ROCR","WOTR")
  return(NCRN)
}
