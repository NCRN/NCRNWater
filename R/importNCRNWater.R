#' @title importNCRNWater
#' 
#' @description This function imports data from a .csv files exporterd from NPStoret and saves it as \code{Park} objects. 
#' 
#' @param Dir The directory where the data is found
#' 
#' @return Returns 11 \code{Park} objects, one for each park, as a \code{list}.
#' 
#' @export

importNCRNWater<-function(Dir){
  OldDir<-getwd()
  setwd(Dir)
  
  
  Indata<-NULL
  
  setwd(OldDir)
  
  ANTI<-new("Park",
            ParkCode="ANTI",
            ShortName="Antietam",
            LongName="Antietam National Battlefield",
            Network="NCRN"
  )
  
  CATO<-new("Park", 
            ParkCode="CATO", 
            ShortName="Catoctin", 
            LongName="Catoctin Mountain Park", 
            Network="NCRN"
  ) 

  CHOH<-new("Park", 
            ParkCode="CHOH", 
            ShortName="C&O Canal", 
            LongName="Chesapeake & Ohio Canal National Historical Park", 
            Network="NCRN"
  ) 

  
  GWMP<-new("Park", 
            ParkCode="GWMP", 
            ShortName="GW Parkway", 
            LongName="George Washington Memorial Parkway", 
            Network="NCRN"
  )
            
  
  HAFE<-new("Park", 
            ParkCode="HAFE", 
            ShortName="Harpers Ferry", 
            LongName="Harpers Ferry National Histroical Park", 
            Network="NCRN"
  )

  MANA<-new("Park",
            ParkCode="MANA",
            ShortName="Manassas",
            LongName="Manassas National Battlefield Park", 
            Network="NCRN"
  )
  
  MONO<-new("Park",
            ParkCode="MONO",
            ShortName="Monocacy",
            LongName="Monocacy National Battlefield", 
            Network="NCRN"
  )

  NACE<-new("Park",
            ParkCode="NACE",
            ShortName="Nat.Cap.Parks - East",
            LongName="National Captial Parks-East", 
            Network="NCRN"
  )
  
  PRWI<-new("Park",
            ParkCode="PRWI",
            ShortName="Prince William",
            LongName="Prince William Forest Park", 
            Network="NCRN"
  )

  
  ROCR<-new("Park",
            ParkCode="ROCR",
            ShortName="Rock Creek",
            LongName="Rock Creek Park", 
            Network="NCRN"
  )
            
  
  WOTR<-new("Park",
            ParkCode="WOTR",
            ShortName="Wolf Trap",
            LongName="Wolf Trap National Park for the Performing Arts", 
            Network="NCRN"
  )
  
  return(c(ANTI,CATO,CHOH,GWMP,HAFE,MANA,MONO,NACE,PRWI,ROCR,WOTR))
}