###Function shoud take in dataframe, date, measurement, formula and output a regression/cosinor

library(season)
library(mgcv)

WaterCosinor<-function(DataIn, DateVar, Measure, Formula) {       #This function takes in a datset, the name of the column with the date and the measurements, and the formula for the cosinor analysis  
  
   CosOut<-vector(mode="list", length=4)
   names(CosOut)<-c("Analysis","CDates","PredLine","Outliers")            #CosOut is the output of the function - a list
  
   
# if(sum(!is.na(DataIn[eval(Measure)]))<24)  #this checks to make sure there is enough data - 24 measurememnts
if(sum(!is.na(DataIn[[Measure]]))<24)
      return(NA)                #returns NA if there is not enough data

  else { 
    #Reg1<-cosinor(formula=Formula, date=DataIn[,DateVar], data=DataIn,type="daily") #does first test to see if consinor or lm is a better fit 
    Reg1<-cosinor(formula=Formula, date=DateVar, data=DataIn, type="daily")  # new version works for R 3.x
    
    if (summary(Reg1)$significant == TRUE ) {           #if this statement is true, then consinor is a better fit - seasonal data
      Frac<-yrfraction(DataIn[[DateVar]],type="daily")    #for cosw and sinw  to determine where in the year you are
      DataIn$cosw<-cos(Frac*2*pi)                         #for cosinor glm
      DataIn$sinw<-sin(Frac*2*pi)                          # for cosinor glm
      TempCos<-glm(formula=update.formula(Formula, ~.+sinw+cosw), data=DataIn, na.action=na.exclude)  #re-does the cosinor, outside of cosinor function
      Rstud<-rstudent(TempCos)                   #studentized residuals
      DataIn$NewMeas<-DataIn[[Measure]]            #copy result values   to new column for redoing analysis minus outliers
      DataIn$Outie<-FALSE                       #Will mark if there are any Outliers
     
      if(max(abs(Rstud), na.rm=TRUE)>=4){      #check to see if any studentized residuals >= 4,
        DataIn$NewMeas[which(abs(Rstud)>=4)]<-NA  # replaces outliers with NA 
        DataIn$Outie[which(abs(Rstud)>=4)]<-TRUE   #Marks Outliers
      } 
    
    }
    
    
    else{
      Reg1<-lm(formula=Formula, data=DataIn, na.action=na.exclude)     #if there is no seasonal pattern, then do a lm
      Rstud<-rstudent(Reg1)                   #studentized residuals
      DataIn$NewMeas<-DataIn[[(Measure)]]           #copy result values
      DataIn$Outie<-FALSE                       #Will mark if there are any Outliers
         
      if(max(abs(Rstud), na.rm=TRUE)>=4){      #check to see if any studentized residual >= 4,
        DataIn$NewMeas[which(abs(Rstud)>=4)]<-NA #put in NA for the big outliers identified above
        DataIn$Outie[which(abs(Rstud)>=4)]<-TRUE #marks outliers
       }  
    
    }
  
  #redo analayis and feed to output
    Formula2<-update.formula(Formula, eval(NewMeas)~.)                               #now do analysis with updated data
  #  Reg2<-cosinor(formula=Formula2, date=DataIn[,DateVar], data=DataIn, type="daily") #does second test to see if consinor or lm is a better fit
  Reg2<-cosinor(formula=Formula2, date=DateVar, data=DataIn, type="daily") #new version works for R 3.x
  if (summary(Reg2)$significant == TRUE ) {              #if cosinor is the better choice do it.
        Frac<-yrfraction(DataIn[[DateVar]],type="daily")    #for cosw and sinw
        DataIn$cosw<-cos(Frac*2*pi)                         #for cosinor glm
        DataIn$sinw<-sin(Frac*2*pi)
        TempCos2<-glm(formula=update.formula(Formula2, ~.+sinw+cosw), data=DataIn, na.action=na.exclude)    #now do cosinor via glm
        
        #Make predicted values for plotting
        PreLen<-as.numeric(length(seq(from=min(DataIn[[DateVar]],na.rm=TRUE),to=max(DataIn[[DateVar]],na.rm=TRUE),by=1)))       #length of the data to be used for predicitons
        PreDates<-data.frame(matrix(ncol=4,nrow=PreLen))                                                                    #data from for predicitons
        colnames(PreDates)<-c(eval(DateVar),"Frac","sinw","cosw")
        PreDates[[DateVar]]<-seq(from=min(DataIn[[DateVar]],na.rm=TRUE), to=max(DataIn[[DateVar]],na.rm=TRUE), by=1)          #add in dates with data
        PreDates$Frac<-yrfraction(PreDates[[DateVar]],type="daily")
        PreDates$sinw<-sin(PreDates$Frac*2*pi)
        PreDates$cosw<-cos(PreDates$Frac*2*pi)
        Preds<-predict(TempCos2,newdata=PreDates,na.action=na.exclude)                                                 #make predicitons
        
        CosOut$Analysis<-Reg2
        CosOut$Cdates<-DataIn[!is.na(DataIn$NewMeas),DateVar]
        CosOut$PredLine<-data.frame(PreDates=PreDates,Preds=Preds)
        CosOut$Outliers<-DataIn[DataIn$Outie==TRUE,c(DateVar,eval(Measure))]
        return(CosOut)
     }
    
    else{
      Reg3<-lm(formula=Formula2, data=DataIn, na.action=na.exclude)
      CosOut$Analysis<-Reg3
      CosOut$CDates<-DataIn[!is.na(DataIn$NewMeas),DateVar]
      CosOut$Outliers<-DataIn[DataIn$Outie==TRUE,c(DateVar,eval(Measure))]#[,c(eval(Measure),DateVar)]
      return(CosOut)
    }
  
  }
}
