# Setup data for weekly models 
# Data Setup and EDA #
rm(list=ls())

library(xts) # Convenitent library for manipulating time series
library(forecast) # Used for automatic ARIMA fitting
library(imputeTS) # Library for time series imputation 
library(tidyverse)


ZoneList = read.csv("D:/Time Series/RideShare Models/Disaggregated Modeling/Data/ZoneList.csv", sep="", stringsAsFactors=FALSE)
Zone.Name.Lookup=read.csv("D:/Time Series/RideShare Models/Disaggregated Modeling/Data/taxi_zone_lookup.csv")

# Make an object for the date
Full.Dates=data.frame("Dates"=seq(as.Date("2015-01-01"),length.out =912 ,by="days"))
Full.Dates=cbind.data.frame(Full.Dates,"Days"=weekdays(Full.Dates$Dates))


#################################################################################################################################################
#################################################################################################################################################
path = 'C:\\Users\\jiz13007\\OneDrive - University of Connecticut\\Rideshare\\Latest Version\\Data\\Modal Data\\'
# Imputation for missing green cab days
green.data=data.frame(read.csv(paste0(path,"Raw Data\\green_tz_15_17_new.csv")))
yellow.data=data.frame(read.csv(paste0(path,"Raw Data\\yellow_tz_daycounts15-17.csv")))

dim(green.data); dim(yellow.data)

z=colnames(yellow.data)
y=colnames(green.data)

#yellow.rm=z[-which(z %in% y)] # Zones 103,104,105 are not in green cab, also not found in TNC
yellow.rm = c('X103','X104','X105')
yellow.data = yellow.data[,!(names(yellow.data) %in% yellow.rm)] # Remove Zones 103 104 and 105
green.data = green.data[,!(names(green.data) %in% yellow.rm)]

# Fill in missing dates with NA's
green.data$pickup_datetime[0:4]
green.data$pickup_datetime=as.Date(green.data$pickup_datetime,format='%Y-%m-%d')
df <- data.frame(pickup_datetime=Full.Dates$Dates)
df$pickup_datetime[0:4]
green.data.new=full_join(df,green.data, by = 'pickup_datetime')
View(green.data.new)

green.data=(green.data.new[,-1])
yellow.data=(yellow.data[,-1])
green.data=cbind.data.frame(green.data,"Day"=Full.Dates$Days)


# Cludgey code to interpolate missing values by the day of week average

# Find the indices that correspond to a given day of the week
index.mon=which(green.data$Day=="Monday")
index.tue=which(green.data$Day=="Tuesday")
index.wed=which(green.data$Day=="Wednesday")
index.thu=which(green.data$Day=="Thursday")
index.fri=which(green.data$Day=="Friday")
index.sat=which(green.data$Day=="Saturday")
index.sun=which(green.data$Day=="Sunday")

# get the mean of each day of the week for each zone

mon.means=round(colMeans(green.data[index.mon,1:262],na.rm = T))
tue.means=round(colMeans(green.data[index.tue,1:262],na.rm = T))
wed.means=round(colMeans(green.data[index.wed,1:262],na.rm = T))
thu.means=round(colMeans(green.data[index.thu,1:262],na.rm = T))
fri.means=round(colMeans(green.data[index.fri,1:262],na.rm = T))
sat.means=round(colMeans(green.data[index.sat,1:262],na.rm = T))
sun.means=round(colMeans(green.data[index.sun,1:262],na.rm = T))

# For loop goes through and replaces each zones missing records with the zones mean value for that day of the week
# This was done to side step issues with interpolation leading to negative counts in certain zones
dim(green.data)
for(k in 1:263){
  
  bad.mon=which(is.na(green.data[,k])==T & green.data$Day=="Monday")
  bad.tue=which(is.na(green.data[,k])==T & green.data$Day=="Tuesday")
  bad.wed=which(is.na(green.data[,k])==T & green.data$Day=="Wednesday")
  bad.thu=which(is.na(green.data[,k])==T & green.data$Day=="Thursday")
  bad.fri=which(is.na(green.data[,k])==T & green.data$Day=="Friday")
  bad.sat=which(is.na(green.data[,k])==T & green.data$Day=="Saturday")
  bad.sun=which(is.na(green.data[,k])==T & green.data$Day=="Sunday")
  
  green.data[bad.mon,k]=mon.means[k]
  green.data[bad.tue,k]=tue.means[k]
  green.data[bad.wed,k]=wed.means[k]
  green.data[bad.thu,k]=thu.means[k]
  green.data[bad.fri,k]=fri.means[k]
  green.data[bad.sat,k]=sat.means[k]
  green.data[bad.sun,k]=sun.means[k]
  
}

sum(is.na(green.data)) # to verify
green.data=green.data[,-263] # remove days since they are no longer needed

taxi.data=xts(green.data+yellow.data,order.by = Full.Dates$Dates) # Final is step is to combine green and yellow cab to get full taxi counts

write.csv(taxi.data, paste0(path, "\\Daily Data\\taxi_tz_daycounts_15-17.csv"), row.names = FALSE)
#################################################################################################################################################
#################################################################################################################################################


# Interpolation via state space representation of an arima model
#green.data.xts=green.data.xts %>% na.kalman("auto.arima")
#yellow.data.xts=yellow.data.xts %>% na.kalman("auto.arima")

#taxi.data=green.data.xts+yellow.data.xts

# Import data for each mode-raw daily counts for 263 zones
# 912 days
# Note that this is the rawest data available
tnc.data=data.frame(read.csv(paste0(path,"Daily Data\\tnc_tz_daycounts_15-17.csv")))
tnc.data=tnc.data[,-c(1,2,266,267)]
tnc.data=xts(tnc.data[,1:ncol(tnc.data)],order.by = Full.Dates$Dates)

taxi.data=data.frame(read.csv(paste0(path,"Daily Data\\taxi_tz_daycounts_15-17.csv")))
taxi.data=taxi.data[,-c(1,2,266,267)]
taxi.data=xts(taxi.data[,1:ncol(taxi.data)],order.by = Full.Dates$Dates)

taxi.data.old=data.frame(read.csv(paste0(path,"Daily Data\\taxi_tz_daycounts_15-17_old.csv")))
taxi.data.old=taxi.data.old[,-c(1,2,266,267)]
taxi.data.old=xts(taxi.data.old[,1:ncol(taxi.data.old)],order.by = Full.Dates$Dates)

sub.data=data.frame(read.csv(paste0(path,"Daily Data\\subway_tz_daycounts_15-17.csv")))
sub.data=sub.data[,-c(1,2,266,267)]
sub.data=xts(sub.data[,1:ncol(sub.data)],order.by = Full.Dates$Dates)


citi.data=data.frame(read.csv(paste0(path,"Daily Data\\bike_tz_daycounts_15-17.csv")))
citi.data=citi.data[,-c(1,2,266,267)] # these columns are not useful-1 and 2 are indices and 264 and 265 are zones with no name
citi.data=xts(citi.data[,1:ncol(citi.data)],order.by = Full.Dates$Dates)

# Aggregate into weekly data

tnc.weekly=matrix()
taxi.old.weekly=matrix()
sub.weekly=matrix()
citi.weekly=matrix()
taxi.weekly=matrix()

for(i in 1:ncol(tnc.data)){
  
  tnc.weekly=cbind(tnc.weekly,apply.weekly(tnc.data[,i],sum)) 
  sub.weekly=cbind(sub.weekly,apply.weekly(sub.data[,i],sum))
  citi.weekly=cbind(citi.weekly,apply.weekly(citi.data[,i],sum))
  
}

for(i in 1:ncol(taxi.data)){
  taxi.weekly=cbind(taxi.weekly,apply.weekly(taxi.data[,i],sum))
  
  
}

for(i in 1:ncol(taxi.data.old)){
  
  taxi.old.weekly=cbind(taxi.old.weekly,apply.weekly(taxi.data.old[,i],sum))
}


# Remove first column since its merely a place holder with NAs in it 
tnc.weekly=tnc.weekly[,-1]
tnc.weekly=tnc.weekly[-c(1,131),] # Remove first and last observations since they do not actually represent complete weeks
tail(tnc.weekly)

taxi.weekly=taxi.weekly[,-1]
taxi.weekly=taxi.weekly[-c(1,131),] # Remove first and last observations since they do not actually represent complete weeks

taxi.old.weekly=taxi.old.weekly[,-1]
taxi.old.weekly=taxi.old.weekly[-c(1,131),] # Remove first and last observations since they do not actually represent complete weeks

sub.weekly=sub.weekly[,-1]
sub.weekly=sub.weekly[-c(1,131),] # Remove first and last observations since they do not actually represent complete weeks


citi.weekly=citi.weekly[,-1]
citi.weekly=citi.weekly[-c(1,131),] # Remove first and last observations since they do not actually represent complete weeks

# Verification
dim(tnc.weekly)
dim(taxi.weekly)
dim(sub.weekly)
dim(citi.weekly)


# For loops to search for zones with max count equal to zero, indicative of a zone with no obs 
tnc.count=0
tnc.no.obs=c()

for(i in 1:ncol(tnc.weekly)){
  
  names.tmp=colnames(tnc.weekly)[i]
  max.tmp=max(tnc.weekly[,i])
  if(max.tmp==0){
    
    tnc.count=tnc.count+1
    tnc.no.obs[i]=names.tmp
    
  }  
  
}

tnc.count # 2 zones with no registered counts ID103 and ID104 
tnc.no.obs # names of zones with no observed counts for taxi

taxi.count=0
taxi.no.obs=c()

for(i in 1:ncol(taxi.weekly)){
  
  names.tmp=colnames(taxi.weekly)[i]
  max.tmp=max(taxi.weekly[,i])
  if(max.tmp==0){
    
    taxi.count=taxi.count+1
    taxi.no.obs[i]=names.tmp
    
  }  
  
}
taxi.count # No taxi series with max count of zero-observations in every zone 
taxi.no.obs # names of zones with no observed counts for taxi

# For loop goes through both TNC and Taxi data frames and finds min for each zone, if it is greater than 10, the zone is good and we record its name to use for subsetting data

good.tnc.list=c()
good.taxi.list=c()

for(i in 1:ncol(tnc.weekly)){
  if(min(tnc.weekly[,i])>10){
    
    good.tnc.list[i]=colnames(tnc.weekly)[i]
    
  }
  
}

for(i in 1:ncol(taxi.weekly)){
  if(min(taxi.weekly[,i])>10){
    
    good.taxi.list[i]=colnames(taxi.weekly)[i]
    
  }
  
  
  
}


# note that if we do this with the mean, then our data can greatly expanded,median also works, though perhaps we'd want to use a smaller quantile

good.tnc.list=good.tnc.list[is.na(good.tnc.list)==F]
length(good.tnc.list) 

good.taxi.list=good.taxi.list[is.na(good.taxi.list)==F]
length(good.taxi.list) 

tnc.indices=which(colnames(tnc.data) %in% good.tnc.list)
taxi.indices=which(colnames(taxi.data) %in% good.taxi.list)

tnc.weekly.subset=tnc.weekly[,tnc.indices]
taxi.weekly.subset=taxi.weekly[,taxi.indices]

summary(tnc.weekly.subset)
summary(taxi.weekly.subset)

dim(taxi.weekly.subset)
dim(tnc.weekly.subset)

################################################################################################################################
# Import exogenous predictors
DailyWeather=data.frame(read.csv(paste0(path,"Raw Data//all_nyc_weather.csv")))
DailyWeather=DailyWeather[,-1]
DailyWeather=xts(DailyWeather,order.by = Full.Dates$Dates)

Events=data.frame(read.csv(paste0(path,"Raw Data//all_nyc_events_raw15-17.csv")))
Events=Events[,-1]
Events=xts(as.numeric(Events),order.by = Full.Dates$Dates)

# Incorporate holidays as a count-number of holidays in a given week
Holidays=data.frame(read.csv(paste0(path,"Raw Data//holidays-nyc.csv")))
Holidays= Holidays[,-1]
Holidays=xts(as.numeric(Holidays$major_holidays),order.by = Full.Dates$Dates)
#as.numeric(Holidays$major_holidays)

# Have left out peak travel season for now
full.weeks=2:130
precip.weekly=apply.weekly(DailyWeather$PRCP_NYC,sum)
precip.weekly=precip.weekly[full.weeks]


events.weekly=apply.weekly(Events,sum)
events.weekly=events.weekly[full.weeks]
colnames(events.weekly)="Events"
events.weekly

holidays.weekly=apply.weekly(Holidays,sum)
holidays.weekly=holidays.weekly[full.weeks]
colnames(holidays.weekly)="Holiday"

# #Export a subset of bad zones to a csv file
# subset.ID=c('ID18','ID20','ID25','ID32',
#            'ID223','ID225','ID241','ID242',
#            'ID255','ID256','ID257','ID260')
# 
# Date=index(taxi.weekly.subset)
# 
# taxi.bad.weekly=cbind.data.frame(Date,(taxi.weekly.subset[,subset.ID]))
# taxi.bad.daily=cbind.data.frame(Date=Full.Dates$Dates,(taxi.data[,subset.ID]))

#write.csv(taxi.weekly,paste0(path,"//Weekly Data//"))
#write.csv(taxi.old.weekly,"D:/Time Series/RideShare Models/Disaggregated Modeling/OldTaxiZones-Weekly.csv")



dim(taxi.weekly)
dim(tnc.weekly)

## write to rds
saveRDS(taxi.weekly.subset,paste0(path, "//Weekly Data//taxi_weekly_subset.rds"))
saveRDS(tnc.weekly.subset,paste0(path, "//Weekly Data//tnc_weekly_subset.rds"))

saveRDS(taxi.weekly,paste0(path, "//Weekly Data//taxi_weekly.rds"))
saveRDS(taxi.weekly,paste0(path, "//Weekly Data//taxi_weekly.rds"))

saveRDS(citi.weekly,paste0(path, "//Weekly Data//citi_weekly.rds"))
saveRDS(sub.weekly,paste0(path, "//Weekly Data//subway_weekly.rds"))

saveRDS(precip.weekly,paste0(path, "//Weekly Data//precip_weekly.rds"))
saveRDS(events.weekly,paste0(path, "//Weekly Data//event_weekly.rds"))
saveRDS(holidays.weekly,paste0(path, "//Weekly Data//holiday_weekly.rds"))