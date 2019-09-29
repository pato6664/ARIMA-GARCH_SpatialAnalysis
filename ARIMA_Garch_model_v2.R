### This script is for fitting ARIMA model for weekly taxi and tnc rideship data
# Function to create a list of automatic ARIMA fits from a dataframe of univariate time series. 
rm(list=ls())
library(astsa)
library('forecast')
library('tseries')
library(plyr)
library(fGarch)


## read taxi and tnc data
path = 'C:\\Users\\jiz13007\\OneDrive - University of Connecticut\\Rideshare\\Latest Version\\Data\\Modal Data\\'
taxi.data = data.frame(readRDS(paste0(path,'//Weekly Data//taxi_weekly_subset.rds')))
tnc.data =data.frame(readRDS(paste0(path,'//Weekly Data//tnc_weekly_subset.rds')))
dim(tnc.data); dim(taxi.data)


# read exogenous variables
precip.data=data.frame(readRDS(paste0(path,'//Weekly Data//precip_weekly.rds')))
holidays.data=data.frame(readRDS(paste0(path,'//Weekly Data//holiday_weekly.rds')))
events.data=data.frame(readRDS(paste0(path,'//Weekly Data//event_weekly.rds')))

precip.data$pickup_datetime = rownames(precip.data)
holidays.data$pickup_datetime = rownames(holidays.data)
events.data$pickup_datetime = rownames(events.data)

tnc.data$pickup_datetime = rownames(tnc.data); taxi.data$pickup_datetime = rownames(taxi.data)

mylist <- list( one=precip.data, two=holidays.data, three=events.data )
exog.data <- join_all( mylist, by="pickup_datetime", type="full" )
exog.data <- exog.data[,-c(which(colnames(exog.data)=='pickup_datetime'))]; dim(exog.data)

exog.data <- as.matrix(exog.data)
head(exog.data) 

#################################################################################################################
####################################  ARIMA / GARCH MODEL #######################################################
#################################################################################################################
zone.arima=function(zone.dat=NULL,exog.mat=NULL,fourier=F,lambda.set=NULL,ic.set=NULL, Garch.set = T, s.max, m.max){
  
  mod.list=list()
  mod.garch.list = list()
  resid.data = c()
  resid.garch.data=c()
  Ljung.list = list()
  Li.list = list()
  Ljung.Plist = c()
  Li.Plist = c()
  Ljung.garch.list = list()
  Li.garch.list = list()
  Ljung.garch.Plist = c()
  Li.garch.Plist = c()
  clean.residual = c()
  clean.zoneids = c()
  zonelist = colnames(zone.dat)
  garch_k = 1
  for(k in 1:ncol(zone.dat)){
    
    print(paste0("=================================",colnames(zone.dat)[k],"================================="))  
    # Fit using auto.arima
    arima.temp=auto.arima((log(zone.dat[,k])),seasonal = T,xreg = exog.mat,
                          lambda = lambda.set,ic=ic.set,biasadj = F)
    checkresiduals(arima.temp, 12, plot=FALSE)
    Ptest=Box.test(arima.temp$residuals, lag=12, type="Ljung", fitdf = length(arima.temp$coef))
    Ptest2=Box.test(arima.temp$residuals^2, lag=12, type="Ljung", fitdf = length(arima.temp$coef))
    
    
    Ljung.list[[k]]<- Ptest
    names(Ljung.list)[k]=colnames(zone.dat)[k]
    Li.list[[k]] <- Ptest2
    names(Li.list)[k]=colnames(zone.dat)[k]
    Ljung.Plist<-c(Ljung.Plist,Ptest$p.value)
    Li.Plist<-c(Li.Plist,Ptest2$p.value)
    
    mod.list[[k]]=arima.temp
    names(mod.list)[k]=colnames(zone.dat)[k]
    resid.data<-cbind(resid.data, arima.temp$residuals)
    
    if((Ptest$p.value>=0.05) & (Ptest2$p.value>=0.05)){ ## temporally clean after arima model
      clean.residual = cbind(clean.residual, arima.temp$residuals)
      clean.zoneids = c(clean.zoneids, zonelist[k] )
    }
    
    else if((Ptest2$p.value<0.05 & Garch.set==T)){
      tmp.garch = garchFit(~garch(1,1), data=arima.temp$residuals, trace = FALSE)
      
      mod.garch.list[[garch_k]]=tmp.garch
      names(mod.garch.list)[garch_k]=colnames(zone.dat)[k]
      
      print('Garch residual Test')
      resid.garch.data<-cbind(resid.data, tmp.garch@residuals)
      Ptest.garch=Box.test(tmp.garch@residuals, lag=12, type="Ljung")
      Ptest2.garch=Box.test(tmp.garch@residuals^2, lag=12, type="Ljung")
      print (sum(is.na(tmp.garch@residuals)))
      
      Ljung.garch.list[[garch_k]]<- Ptest.garch
      names(Ljung.garch.list)[garch_k]=colnames(zone.dat)[k]
      Li.garch.list[[garch_k]] <- Ptest2.garch
      names(Li.garch.list)[garch_k]=colnames(zone.dat)[k]
      Ljung.garch.Plist<-c(Ljung.garch.Plist,Ptest.garch$p.value)
      Li.garch.Plist<-c(Li.garch.Plist,Ptest2.garch$p.value)
        
        if((Ptest$p.value>=0.05)&(Ptest.garch$p.value>=0.05)){#temporally clean after Garch model
          clean.residual = cbind(clean.residual, tmp.garch@residuals)
          clean.zoneids = c(clean.zoneids, zonelist[k])
        }
      garch_k=garch_k+1
      }
    }
  
  colnames(clean.residual) = clean.zoneids
  
  return(structure(list(mod.list = mod.list, Ljung.list = Ljung.list, resid.data =resid.data,
                        resid.garch.data=resid.garch.data,
                        Li.list = Li.list, Ljung.Plist =Ljung.Plist, Li.Plist=Li.Plist,
                        Li.garch.list = Li.garch.list, Ljung.garch.list = Ljung.garch.list,
                        Li.garch.Plist = Li.garch.Plist, Ljung.garch.Plist=Ljung.garch.Plist,
                        clean.zoneids = clean.zoneids, clean.residual = clean.residual
                        )))
}

#################################################################################################################
####################################  Fit Taxi #######################################################
#################################################################################################################

zonelist.taxi = colnames(taxi.data); length(zonelist)
zonelist.taxi =zonelist.taxi[zonelist.taxi!='pickup_datetime']; length(zonelist.taxi)
taxi.data.ts=ts(taxi.data[,zonelist.taxi], frequency = 4); dim(taxi.data.ts)

# taxi_model = zone.arima(taxi.data.ts,exog.mat=exog.data,fourier=F,lambda.set=NULL,ic.set=NULL,Garch.set = T, s.max = 1, m.max=1)
# #View(taxi_model)
# sum(taxi_model$Ljung.Plist<0.05);sum(taxi_model$Li.Plist<0.05)
# sum(taxi_model$Ljung.garch.Plist>0.05);sum(taxi_model$Li.garch.Plist>0.05)
# View(taxi_model$clean.residual)

taxi_model2 = zone.arima(taxi.data.ts,exog.mat=NULL,fourier=F,lambda.set=NULL,ic.set=NULL,Garch.set = T, s.max = 1, m.max=1)
#View(taxi_model)
sum(taxi_model2$Ljung.Plist<0.05);sum(taxi_model2$Li.Plist<0.05)
sum(taxi_model2$Ljung.garch.Plist>0.05);sum(taxi_model2$Li.garch.Plist>0.05)
dim(taxi_model2$clean.residual)



#################################################################################################################
####################################  Fit TNC #######################################################
#################################################################################################################

zonelist.tnc = colnames(tnc.data);
zonelist.tnc =zonelist.tnc[zonelist.tnc!='pickup_datetime']; length(zonelist.tnc)
tnc.data.ts=ts(tnc.data[,zonelist.tnc], frequency = 4); dim(tnc.data.ts)


# tnc_model = zone.arima(tnc.data.ts,exog.mat=exog.data,fourier=F,lambda.set=NULL,ic.set=NULL,Garch.set = T, s.max = 1, m.max=1)
# #View(taxi_model)
# sum(tnc_model$Ljung.Plist<0.05);sum(tnc_model$Li.Plist<0.05)
# sum(tnc_model$Ljung.garch.Plist>0.05);sum(tnc_model$Li.garch.Plist>0.05)
# dim(tnc_model$clean.residual)

tnc_model2 = zone.arima(tnc.data.ts,exog.mat=NULL,fourier=F,lambda.set=NULL,ic.set=NULL,Garch.set = T, s.max = 1, m.max=1)
#View(taxi_model)
sum(tnc_model2$Ljung.Plist<0.05);sum(tnc_model2$Li.Plist<0.05)
sum(tnc_model2$Ljung.garch.Plist>0.05);sum(tnc_model2$Li.garch.Plist>0.05)
dim(tnc_model2$clean.residual)


#################################################################################################################
######################################  SAVE RESULT #######################################################
#################################################################################################################
saveRDS(tnc_model2, paste0(path, 'Model Result\\tnc_model_result_v2.rds'))
saveRDS(taxi_model2, paste0(path, 'Model Result\\taxi_model_result_v2.rds'))
