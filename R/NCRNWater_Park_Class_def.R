#' @title S4 Class Definition for Park object in NCRNWater
#' 
#' @description An S4 class that contains the data from water monitoring from a single park. Data on sites will be stored as one or more S4 objects in  each park object  
#' @slot ParkCode A short code to designate the park, typically an NPS 4 letter code. Stored as a length 1 character vector. 
#' @slot ShortName A short name for the park. Stored as a length 1 character vector. 
#' @slot LongName  A long, formal name for the park. Stored as a length 1 character vector. 
#' @slot Network The code for the Inventory & Monitoring network the park belongs to. Stored as a length 1 character vector.
#' @slot Sites A list of \code{Site} objects associated with the park. 
#' 
#' @exportClass Park

setClass(Class="Park",
         slots=c(
           ParkCode="character",
           ShortName="character",
           LongName="character",
           Network="character",
           Sites="list"
          ),
         
         prototype=list(ParkCode=character(), 
                        ShortName=character(),
                        LongName=character(), 
                        Network=character(), 
                        Sites=list()
                        )
)