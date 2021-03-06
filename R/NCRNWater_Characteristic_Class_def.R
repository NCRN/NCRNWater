
setClassUnion(name="Num_or_DF",members= c("numeric","data.frame"))
#' @include NCRNWater_Park_Class_def.R
#' @include NCRNWater_Site_Class_def.R
#' @title S4 Class Definition for Characteristic object in  NCRNWater
#' 
#' @description An S4 class that contains the data from water monitoring for a single water quality characteristic as a single site in a single park.   
#' 
#' @slot CharacteristicName The name of the water quality characteristic. Stored as a length 1 character vector. 
#' @slot DisplayName A name for the characteristic suitable for display on graphs, tables etc. Stored as a length 1 character vector.
#' @slot Substrate The substrate of the characteristic, stored as a length 1 character vector. 
#' @slot SampleFraction The sample fraction of the characteristic. Stored as a length 1 character vector. 
#' @slot Category The category for the characteristic. Stored as a length 1 character vector.
#' @slot CategoryDisplay The display name for the category, suitable for graphs, tables etc. 
#' @slot Details A description of the characteristic as needed. Stored as a length 1 character vector. 
#' @slot Units  The units of measurement of the characteristic. Stored as a length 1 character vector. 
#' @slot Data  A \code{data.frame} containing the water quality data. Should have columns representing the date, measurement, any QAQC flags etc. for each measurement.
#' @slot LowerPoint,UpperPoint Indicates the assessment points. Values lower than the lower point are considered to have failed the assessment, 
#' whereas values higher than the higher point are considered failures. Can either be a length 1 numerical vector, or a \code{data.frame} 
#' giving the lower assessment value for each measurement. The vector method should be used for assessments that do not change, whereas the \code{data.frame} is for 
#' assessment that change based on the time of the year or other factors.
#' @slot LowerType,UpperType The type of assessment indicated by the lower and upper points. A length 1 character vector.
#' @slot LowerDescription,UpperDescription A description of the lower and upper assessments point. Stored as a length 1 character vector.
#' @slot AssessmentDetails Additional description of the assessment point. Stored as a length 1 character vector.
#' 
#' @exportClass Characteristic


#setOldClass("data.frame")


setClass(Class="Characteristic",
          slots=c(CharacteristicName="character",
                 DisplayName="character",
                 Substrate="character",
                 SampleFraction="character",
                 Category="character",
                 CategoryDisplay="character",
                 Details="character",
                 Units="character",
                 Data="data.frame",
                 LowerPoint="Num_or_DF",
                 LowerType="character",
                 LowerDescription="character",
                 UpperPoint="Num_or_DF",
                 UpperType="character",
                 UpperDescription="character",
                 AssessmentDetails="character"
                ),
         
          prototype=list(CharacteristicName=character(), 
                        DisplayName=character(),
                        Substrate=character(), 
                        SampleFraction=character(),
                        Category=character(),
                        CategoryDisplay=character(),
                        Details=character(),
                        Units=character(),
                        Data=data.frame(),
                        LowerPoint=numeric(),
                        LowerType=character(),
                        LowerDescription=character(),
                        UpperPoint=numeric(),
                        UpperType=character(),
                        UpperDescription=character(),
                        AssessmentDetails=character()
         )
)

