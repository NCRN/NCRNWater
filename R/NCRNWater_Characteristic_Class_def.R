setClassUnion(name="Num_or_DF",members= c("numeric","data.frame"))

#' @title S4 Class Definition for Characteristic object in  NCRNWater
#' 
#' @description An S4 class that contains the data from water monitoring for a single water quality characteristic as a single site in a single park.   
#' 
#' @slot CharacteristicName The name of the water quality characteristic. Stored as a length 1 character vector. 
#' @slot DisplayName A name for the characteristic suitable for dispaly on graphs, tables etc. Stored as a length 1 character vector. 
#' @slot SampleFraction The sample fraction of the characteristic. Stored as a length 1 character vector. 
#' @slot Category The category for the charactersitic. Stored as a length 1 character vector.
#' @slot Details A description of the characterisitc as needed. Stored as a length 1 character vector. 
#' @slot Units  The units of measurement of the characteristic. Stored as a length 1 character vector. 
#' @slot Data  A \code{data.frame} containing the water quality data. Should have cloumns representing the date, measurment, any QAQC flags etc. for each measurement.
#' @slot LowerPoint,Upperpoint Indicates the assessmen points. Values lower than the lower point are considered to have failed the assessement, whereas values higher than the higher point are considered failures. Can either be a length 1 numerical vector, or a \code{data.frame}  giving the lower assessment value for each measurement. The vector method should be used for assessments that do not change, whereas the \code{data.frame} is for assmesent that change based on the time of the year or other factors.
#' @slot LowerType,UpperType The type of assessment indicated by the lower and upper points. A length 1 character vector.
#' @slot LowerDescription,UpperDescription A description of the lowerand upper assesements point. Stored as a length 1 character vector.
#' @slot AssessmentDetails Additional description of the assessement point. Stored as a length 1 character vector.
#' 
#' @exportClass Characteristic


#setOldClass("data.frame")


setClass(Class="Characteristic",
          slots=c(CharacteristicName="character",
                 DisplayName="character",
                 Substrate="character",
                 SampleFraction="character",
                 Category="character",
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
         
          prototype=list(ChacateristicName=character(), 
                        DisplayName=character(),
                        Substrate=character(), 
                        SampleFraction=character(),
                        Category=character(),
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