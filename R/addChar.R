#' @include NCRNWater_Park_Class_def.R
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @title addChar
#' 
#' @description This function adds a water quality characteristic to an existing \code{Site} object.
#' 
#' @importFrom methods new
#' 
#' @param park A \code{Park} object
#' @param site If \code{park} is not specified, then a \code{Site} object. If \code{park} is specified, then the SiteCode corresponding to a site object conatined in the park object.
#' @param CharacteristicName The name of the water quality characteristic. Stored as a length 1 character vector. 
#' @param DisplayName A name for the characteristic suitable for display on graphs, tables etc. Stored as a length 1 character vector. 
#' @param SampleFraction The sample fraction of the characteristic. Stored as a length 1 character vector.
#' @param Substrate The substrate of the characteristic, stored as a length 1  character vector. 
#' @param Category The category for the characteristic. Stored as a length 1 character vector.
#' @param Details A description of the characteristic as needed. Stored as a length 1 character vector. 
#' @param Units  The units of measurement of the characteristic. Stored as a length 1 character vector. 
#' @param Data  A \code{data.frame} containing the water quality data. Should have columns representing the date, measurement, any QAQC flags etc. for each measurement.
#' @param LowerPoint,UpperPoint Indicates the assessment points. Values lower than the lower point are considered to have failed the assessment, whereas values higher than the higher point are considered failures. Can either be a length 1 numerical vector, or a \code{data.frame}  giving the lower assessment value for each measurement. The vector method should be used for assessments that do not change, whereas the \code{data.frame} is for assmesent that change based on the time of the year or other factors.
#' @param LowerType,UpperType The type of assessment indicated by the lower and upper points. A length 1 character vector.
#' @param LowerDescription,UpperDescription A description of the lower and upper assessments point. Stored as a length 1 character vector.
#' @param AssessmentDetails Additional description of the assessment point. Stored as a length 1 character vector.
#' 
#' 
#' @return If no park is specified,returns the site object with the characteristic added. If a park is specified, returns the park object with the characteristic added to the site.
#' 
#' @details This command can be used to add a \code{Characteristic} object to the list of existing characteristics in an existing \code{Site} object. The \code{CharacteristicName} will be used to name the characteristeic in the list. If a \code{park} object is provided \code{addCharacteristic} will look for the site in that park.
#' 
#' @export
 

 addChar<-function(park,site,CharacteristicName,DisplayName,Substrate,SampleFraction=NA,Category,Details,Units,Data,LowerPoint,LowerType,
                     LowerDescription, UpperPoint, UpperType, UpperDescription,AssessmentDetails){
  
   tryCatch(missing(site),  error=function(err) stop("You need to specify a site", call.=FALSE))
   XCall<-match.call() #figure out what args user put in.
   XCharacteristic<-list(
     do.call("new", 
      c(Class="Characteristic",as.list(XCall)[-1][!names(XCall[-1]) %in% c("park","site")]) #passes all args but "park" & "site' to new()
     )
   )
   
   names(XCharacteristic)<-CharacteristicName
   if(is.null(park)){
     site@Characteristics<-c(site@Characterstics,XCharacteristic)
     return(site)
   } else{
      park@Sites[[site]]@Characteristics<-c(park@Sites[[site]]@Characteristics,XCharacteristic)
   return(park)
   }
   
 }
 

