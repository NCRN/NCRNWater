setClassUnion(name="NCRNWaterObj", members=c("Park","Site","Characteristic","list"))

#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R 
#' @include NCRNWater_Characteristic_Class_def.R
#' 
#' @title S4 Class Definition for the class union \code{NCRNWaterObj}
#' 
#' @description This is an virtual class combining \code{Park}, \code{Site},\code{Characteristic} and \code{list}. It is used for convenience for writing methods for the \code{NCRNWater} package. 
#'  
#' @exportClass NCRNWaterObj
