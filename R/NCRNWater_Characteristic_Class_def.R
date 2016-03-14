#' @title S4 Class Definition for Chacteristic object in  NCRNWater
#' 
#' @description
#' An S4 class that contains the data from water monitoring for a single water quality characteristic as a single site in a single park.   
#' @slot ParkCode A short code to designate the park, typically an NPS 4 letter code. Stored as a length 1 character vector. 
#' @slot ShortName A short name for the park. Stored as a length 1 character vector. 
#' @slot LongName  A long, formal name for the park. Stored as a length 1 character vector. 
#' @slot Network The code for the Inventory & Montoirng network (or other network) the park belongs to. Stored as a length 1 character vector.
#' @slot Sites A list of \code{Site} objects associated with the park. 

#' 
#' @exportClass Park

setClassUnion("Num_or_DF", c("numeric","data.frame"))

setClass(Class="Chacteristic",
          slots=c(ChacteristicName="character",
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