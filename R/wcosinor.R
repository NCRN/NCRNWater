#' @include NCRNWater_Park_Class_def.R 
#' @include NCRNWater_Site_Class_def.R
#' @include NCRNWater_Characteristic_Class_def.R
#' @include NCRNWater_NCRNWaterObj_Class_def.R 
#' @include getWData.R
#' @importFrom season cosinor yrfraction
#' @importFrom methods callGeneric
#' @title wcosinor
#' 
#' @description Does a cosinor analytsis on water quality data .
#' 
#' @param object Either a \code{Characteristic} object a \code{Site} object, a \code{Park} object or a \code{list} of such objects.
#' @param ... Additional commands passed to \code{\link{getWData}} for filtering or subsetting hte data.
#' 
#' @return The results of a cosinor analysis.
#' 
#' @export

setGeneric(name="wcosinor",function(object,...){standardGeneric("wcosinor")},signature=c("object") )

setMethod(f="wcosinor", signature =c(object="NCRNWaterObj"), 
    function(object, ...){
      object<-getWData(object,...)
      callGeneric()
      
    })


setMethod(f="wcosinor", signature=c(object="data.frame"),
  function(object,...){
    ####  Add warning if nobs<24 #### 
    if({!is.na(object$Value)} %>% sum <24) warning("There are fewer than 24 observations, the seasonal analysis may be misleading.")
    
    #### Current outputs from old funciton ####
    CosOut<-vector(mode="list", length=4)
    names(CosOut)<-c("Analysis","CDates","PredLine","Outliers")
    
    
    Reg1<-cosinor(Value~Date, date="Date", data=object,type="daily")
           
    if (summary(Reg1)$significant == TRUE ) {           #true indicates consinor is a better fit - seasonal data
      Frac<-yrfraction(object$Date,type="daily")    #for cosw and sinw  to determine where in the year you are
      object$cosw<-cos(Frac*2*pi)                         #for cosinor glm
      object$sinw<-sin(Frac*2*pi)                          # for cosinor glm
      TempReg<-glm(Value~Date+sinw+cosw, data=object, na.action=na.exclude)  #re-does the cosinor, outside of cosinor function
    } else {
      TempReg<-lm(Value~Date, data=object, na.action=na.exclude)     #if there is no seasonal pattern, then do a lm
    }  
    
    Rstud<-rstudent(TempReg)                   #studentized residuals
    object$NewMeas<-object$Value           #copy result values
    object$Outie<-FALSE #Will mark if there are any Outliers
     
    if(max(abs(Rstud), na.rm=TRUE)>=4){      #check to see if any studentized residual >= 4,
      object$NewMeas[which(abs(Rstud)>=4)]<-NA #put in NA for the big outliers identified above
      object$Outie[which(abs(Rstud)>=4)]<-TRUE #marks outliers
    }  
   
   #### redoing analysis after excluding the outliers - need to make this optional, give control to size of outlier
   
   Reg2<-cosinor(NewMeas~Date, date="Date", data=object, type="daily")
   
   if (summary(Reg2)$significant == TRUE ) {              #if cosinor is still the better choice do it.
     Frac<-yrfraction(object$Date,type="daily")    #for cosw and sinw
     object$cosw<-cos(Frac*2*pi)                         #for cosinor glm
     object$sinw<-sin(Frac*2*pi)
     TempCos2<-glm(NewMeas~Date+sinw+cosw, data=object, na.action=na.exclude)    #now do cosinor via glm for predictions
     
     #Make predicted values for plotting
     PreLen<-as.numeric(length(seq(from=min(object$Date,na.rm=TRUE),to=max(object$Date,na.rm=TRUE),by=1))) #length of the data to be used for predicitons
     PreDates<-data.frame(matrix(ncol=4,nrow=PreLen))        #data.frame for predicitons
     colnames(PreDates)<-c("Date","Frac","sinw","cosw")
     PreDates$Date<-seq(from=min(object$Date,na.rm=TRUE), to=max(object$Date,na.rm=TRUE), by=1)          #add in dates with data
     PreDates$Frac<-yrfraction(PreDates$Date,type="daily")
     PreDates$sinw<-sin(PreDates$Frac*2*pi)
     PreDates$cosw<-cos(PreDates$Frac*2*pi)
     Preds<-predict(TempCos2,newdata=PreDates,na.action=na.exclude)                                                 #make predicitons
     
     CosOut$Analysis<-Reg2
     CosOut$Cdates<-object[!is.na(object$NewMeas),"Date"]
     CosOut$PredLine<-data.frame(PreDates=PreDates,Preds=Preds)
     CosOut$Outliers<-object[object$Outie==TRUE,c("Date","Value")]
     return(CosOut)
   }  else {
     
     Reg3<-lm(NewMeas~Date, data=object, na.action=na.exclude)
     CosOut$Analysis<-Reg3
     CosOut$CDates<-object[!is.na(object$NewMeas),"Date"]
     CosOut$Outliers<-object[object$Outie==TRUE,c("Date","Value")]
     return(CosOut)
   }

})


