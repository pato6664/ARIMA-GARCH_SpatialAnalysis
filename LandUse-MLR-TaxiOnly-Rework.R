# MLR on weekly residuals 
rm(list=ls())
wd=getwd()
library(tidyverse)
library(reshape2) # Give us the 'melt' funcion-similar to stack()
library(stringr)
library(MASS)
library(broom)

# Load residuals data (residuals from Jingyues models)
taxi_weekly_residuals=readRDS(paste0(wd,"/taxi_weekly_residuals.rds"))
taxi_weekly_residuals$pickup_date=as.Date(taxi_weekly_residuals$pickup_date)
colnames(taxi_weekly_residuals)=gsub(pattern='X',replacement = "ID",x=colnames(taxi_weekly_residuals))
taxi_weekly_residuals$Year=format(as.Date(taxi_weekly_residuals$pickup_date, format="%d/%m/%Y"),"%Y")

# Demographic data 
ACS15=data.frame(readRDS(paste0(wd,"/Demographic and Land Use-09-18-2019/ACS15.tz.rds")))
ACS16=data.frame(readRDS(paste0(wd,"/Demographic and Land Use-09-18-2019/ACS16.tz.rds")))
ACS17=data.frame(readRDS(paste0(wd,"/Demographic and Land Use-09-18-2019/ACS17.tz.rds")))
ACS16=ACS16[,-c(which(colnames(ACS16)=="tot_pop15num"))] # need to remvove this as it causes problems later on-look for clean way to find it

# landuse data 
PLUTO15=data.frame(readRDS(paste0(wd,"/Demographic and Land Use-09-18-2019/PLUTO15.rds")))
PLUTO16=data.frame(readRDS(paste0(wd,"/Demographic and Land Use-09-18-2019/PLUTO16.rds")))
PLUTO17=data.frame(readRDS(paste0(wd,"/Demographic and Land Use-09-18-2019/PLUTO17.rds")))

# Insert a new column to for year to act as a key for a merge
ACS15$Year=PLUTO15$Year="2015"
ACS16$Year=PLUTO16$Year="2016"
ACS17$Year=PLUTO17$Year="2017"

# Put dataframes into lists for convenience
demog.list=list(ACS15,ACS16,ACS17)
land.use.list=list(PLUTO15,PLUTO16,PLUTO17)
names(land.use.list)=names(demog.list)=c("2015","2016","2017")
for(i in 1:length(demog.list)){
  
  demog.list[[i]][,"LocationID"]=factor(paste0("ID",as.character(demog.list[[i]][,"LocationID"])))
  land.use.list[[i]][,"LocationID"]=factor(paste0("ID",as.character(land.use.list[[i]][,"LocationID"])))
  
}


# Need to fix labeling
year.str=c("15","16","17")
str.rm=c("num",".ynum","ynum")

for(i in 1:length(demog.list)){
  
  
  for(k in 1:length(year.str))
  
    for(j in 1:length(str.rm)){
      
      colnames(demog.list[[i]])=str_replace_all(colnames(demog.list[[i]]),paste0(year.str[k],pattern=str.rm[j]),replacement = "")
      
    }
  
}

for(i in 1:length(land.use.list)){
  
  for(j in 1:length(year.str)){
    
    colnames(land.use.list[[i]])=str_replace_all(colnames(land.use.list[[i]]),year.str[j],replacement = "")
    
  }
  
}
# Take a subset of the demographic variable
# Population older 16 that is employed
# Full time employed
# Median Earings
# Median Age
# Total Population 
# Some age variables-not uniform across the years so a little more work in setting up DF
demog.var.names=c("LocationID","Year","tot_pop","pop_over16_emp","fulltime_emp","median_earnings","median_age")
colnames(demog.list$`2015`)
demog.list.new=list()
for(i in 1:length(demog.list)){
  
    
    demog.list.new[[i]]=demog.list[[i]][,which(colnames(demog.list[[i]]) %in% demog.var.names)]
    names(demog.list.new)[[i]]=names(demog.list)[[i]]
  
  
  
}

# Names check out and now can just rbind these things
colnames(demog.list.new$`2015`)==colnames(demog.list.new$`2016`)
colnames(demog.list.new$`2016`)==colnames(demog.list.new$`2017`)
demog.combined.df=rbind(demog.list.new$`2015`,demog.list.new$`2016`,demog.list.new$`2017`)

# Make a column that for total area=(lot area) + (building area)-will allow us to handle ID8 and ID43 more easily
for(i in 1:length(land.use.list)){
  
  land.use.list[[i]]$TotArea=land.use.list[[i]][,"LotArea"]+land.use.list[[i]][,"BldgArea"]
  print(sum(land.use.list[[i]]$TotArea==0))
}
demog.combined.df$tot_pop
demog.combined.df[is.na(demog.combined.df)]=0

# Create a dataframe for exogenous predictors that is seperate from the original dataframes
# Function to create land use percentages-note that I made it to take vectors from some sort of dataframe that has a LocationID to name things

land.use.combined=rbind(land.use.list$`2015`,land.use.list$`2016`,land.use.list$`2017`)
land.use.pct.df=cbind.data.frame("LocationID"=land.use.combined[,"LocationID"],"Year"=land.use.combined[,"Year"],
                                 round(land.use.combined[,4:11]/land.use.combined$TotArea,3))

colnames(land.use.pct.df)=gsub("Area","Pct",colnames(land.use.pct.df))

UseType=rep(NA,nrow(land.use.pct.df))
UseTypeLevels=c(colnames(land.use.pct.df)[3:10])
UseType[which(land.use.pct.df$LocationID %in% c("ID8","ID43"))]="Park"


for(i in 1:length(UseTypeLevels)){
  
  UseType[land.use.pct.df[,UseTypeLevels[i]]>0.5]=UseTypeLevels[i]
}

UseType[is.na(UseType)]="Mixed"
land.use.pct.df=cbind.data.frame(land.use.pct.df,"UseType"=as.factor(UseType))

# Create ratios now for the demographic variables in conjunction with land use variables
# Use the 'melt' function to now stack the taxi residuals and then merge with demographic and land use data 
taxi.res.melt=melt(taxi_weekly_residuals[,-which(colnames(taxi_weekly_residuals)=="pickup_date")],id="Year")
colnames(taxi.res.melt)=c("Year","LocationID","Resid")
taxi.res.melt$LocationID=factor(taxi.res.melt$LocationID)


# Use a series of inner_joins to merge data-nice because it allows us to ensure that all variables are associated right zones and year
# inner_join also removes 
taxi.res.melt=inner_join(taxi.res.melt,land.use.combined,by=c("LocationID","Year"))
taxi.res.melt=inner_join(taxi.res.melt,land.use.pct.df,by=c("LocationID","Year"))
taxi.res.melt=inner_join(taxi.res.melt,demog.combined.df,by=c("LocationID","Year"))
taxi.res.melt$UnitsRes=taxi.res.melt$UnitsRes+1
taxi.res.melt$pop_to_hh=taxi.res.melt$tot_pop/taxi.res.melt$UnitsRes
taxi.res.melt$AgeCat=rep(NA,nrow(taxi.res.melt))



# Create a categorical variable for age
taxi.res.melt$AgeCat[taxi.res.melt$median_age<40]="Young"
taxi.res.melt$AgeCat[taxi.res.melt$median_age>40]="Old"
taxi.res.melt$AgeCat=as.factor(taxi.res.melt$AgeCat)
# MLR using leaps and caret library

# Subset data first:
# Use only percentages and continuous variables for median age,median earnings and pop:num_house_holds and fulltime_employed:num_households
# No interactions
taxi.res.melt.pct=taxi.res.melt[c(3,19:26,29,31:33)]
mlr.no_int=lm(Resid~.,data = taxi.res.melt.pct)
summary(mlr.no_int)
mlr.step.aic.no_interactions=stepAIC(mlr.no_int,direction = 'both')
summary(mlr.step.aic.no_interactions)

taxi.mlr.no_int.resid=cbind.data.frame("LocationID"=taxi.res.melt$LocationID,
                                      "Year"=taxi.res.melt$Year,
                                      "Date"=taxi_weekly_residuals$pickup_date,
                                      "Residual"=residuals(mlr.step.aic.no_interactions))
write.csv(taxi.mlr.no_int.resid,paste0(wd,"/Taxi-MLR_No_Interactions-Residuals-09-21.csv"))




pct.names=colnames(taxi.res.melt[,19:26])
demog.names=colnames(taxi.res.melt[,c(29,31:33)])
taxi.melt.demog=taxi.res.melt[,c(29,31:33)]
int.list=list()

for(i in 1:length(demog.names)){
  
  int.df.tmp=taxi.res.melt[,19:26]*taxi.melt.demog[,i]
  colnames(int.df.tmp)=pct.names
  colnames(int.df.tmp)=gsub("Pct",paste0("-",demog.names[i]),colnames(int.df.tmp))
  int.list[[i]]=int.df.tmp
  names(int.list)[i]=demog.names[i]

}

int.df.final=data.frame(int.list$fulltime_emp,int.list$median_earnings,
                        int.list$median_age,int.list$pop_to_hh)

taxi.mlr.df=cbind.data.frame(taxi.res.melt.pct,int.df.final)

mlr.1=lm(Resid~.,data = taxi.mlr.df)
summary(mlr.1)
mlr.step.aic.1=stepAIC(mlr.1,direction = 'both')
summary(mlr.step.aic.1)

taxi.mlr.resid=cbind.data.frame("LocationID"=taxi.res.melt$LocationID,
                                "Year"=taxi.res.melt$Year,
                                "Date"=taxi_weekly_residuals$pickup_date,
                                "Residual"=residuals(mlr.step.aic.1))
#write.csv(taxi.mlr.resid,"Taxi-MLR-Residuals-09-26.csv")

#tidy_taxi.step.aic=(tidy(mlr.step.aic.1))
#write.csv(tidy_taxi.step.aic,paste0(wd,"/Taxi-MLR-Results-09-26.csv"))
