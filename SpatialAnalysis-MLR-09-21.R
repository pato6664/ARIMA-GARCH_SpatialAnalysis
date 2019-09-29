# Weekdays - TNC
# Localized Analysis with Choropleth Maps
# Follows examples laid out in Chapter 8 of Brundson and Comber's book 
# Cannot use Getis-Ord as it requires positive values
rm(list=ls())
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(sp)
library(spdep)
library(sf)
library(tmap)
library(tidyverse)
library(grid)
library(gridExtra)

################################################################################################################################
tnc.model.residuals=data.frame(read.csv("TNC-MLR-Residuals-09-21.csv"))#rownames(tnc.model.residuals)=Full.Dates[100:912]
tnc.model.residuals=tnc.model.residuals[,-1]#rownames(tnc.model.residuals)=Full.Dates[100:912]
tnc.model.residuals$Date=as.Date(tnc.model.residuals$Date,format="%d/%m/%Y")
id.tnc=as.character(levels(tnc.model.residuals$LocationID))

tnc.means=c()
tnc.means.2015=c()
tnc.means.2016=c()
tnc.means.2017=c()


for(i in 1:length(id.tnc)){
  
  tnc.means[i]=mean(tnc.model.residuals[tnc.model.residuals$LocationID==id.tnc[i] ,"Residual"])
  tnc.means.2015[i]= mean(tnc.model.residuals[tnc.model.residuals$LocationID==id.tnc[i] &tnc.model.residuals$Year==2015,"Residual"])
  tnc.means.2016[i]= mean(tnc.model.residuals[tnc.model.residuals$LocationID==id.tnc[i] &tnc.model.residuals$Year==2016,"Residual"])
  tnc.means.2017[i]= mean(tnc.model.residuals[tnc.model.residuals$LocationID==id.tnc[i] &tnc.model.residuals$Year==2017,"Residual"])
  names(tnc.means.2015)[i]=names(tnc.means.2016)[i]=names(tnc.means.2017)[i]=names(tnc.means)[i]=id.tnc[i]
  
}


# load taxi residuals and aggregate
# use global mean and yearly mean for aggregation

taxi.model.residuals=data.frame(read.csv("Taxi-MLR-Residuals-09-21.csv"))
taxi.model.residuals=taxi.model.residuals[,-1]
taxi.model.residuals$Date=as.Date(taxi.model.residuals$Date,format="%d/%m/%Y")


id.taxi=as.character(levels(taxi.model.residuals$LocationID))
taxi.means=c()
taxi.means.2015=c()
taxi.means.2016=c()
taxi.means.2017=c()


for(i in 1:length(id.taxi)){
  
  taxi.means[i]=mean(taxi.model.residuals[taxi.model.residuals$LocationID==id.taxi[i],"Residual"])
  taxi.means.2015[i]= mean(taxi.model.residuals[taxi.model.residuals$LocationID==id.taxi[i] & taxi.model.residuals$Year==2015,"Residual"])
  taxi.means.2016[i]= mean(taxi.model.residuals[taxi.model.residuals$LocationID==id.taxi[i] & taxi.model.residuals$Year==2016,"Residual"])
  taxi.means.2017[i]= mean(taxi.model.residuals[taxi.model.residuals$LocationID==id.taxi[i] & taxi.model.residuals$Year==2017,"Residual"])
  names(taxi.means.2015)[i]=names(taxi.means.2016)[i]=names(taxi.means.2017)[i]=names(taxi.means)[i]=id.taxi[i]
}




# Spatial Analysis
zone.shp=readOGR("D:\\Time Series\\RideShare Models\\Disaggregated Modeling\\Spatial Analysis\\NYC_Taxi_Zone-Shapefile")
zone.shp$LocationID=paste0("ID",zone.shp$LocationID)

# Subset down to zones we have data for 
tnc.zone.shp=zone.shp[zone.shp$LocationID %in% as.character(tnc.model.residuals$LocationID),]
taxi.zone.shp=zone.shp[zone.shp$LocationID %in%  as.character(taxi.model.residuals$LocationID),]

bbinfor.tnc = tmaptools::bb(tnc.zone.shp) # res.innder is the shaple file of subset taxi zones for taxi residuals
bbinfor.tnc['ymax'] = bbinfor.tnc['ymax']+40000 # plus 40000 is try to avoid the title overlays with the plot


bbinfor.taxi = tmaptools::bb(tnc.zone.shp) # res.innder is the shaple file of subset taxi zones for taxi residuals
bbinfor.taxi['ymax'] = bbinfor.taxi['ymax']+40000 # plus 40000 is try to avoid the title overlays with the plot

tnc.zone.shp$borough
# Plot of Subset Data-by Borough
windows()
tm_shape(tnc.zone.shp)+ tm_polygons(style="quantile", col = 'borough') 


# Bind the mean residuals to each spatial dataframes
tnc.means=cbind.data.frame("LocationID"=id.tnc,"Mean"=tnc.means,
                           "Mean-15"=tnc.means.2015,"Mean-16"=tnc.means.2016,"Mean-17"=tnc.means.2017)


taxi.means=cbind.data.frame("LocationID"=id.taxi,"Mean"=taxi.means,
                            "Mean-15"=taxi.means.2015,"Mean-16"=taxi.means.2016,"Mean-17"=taxi.means.2017)

tnc.zone.shp@data=tnc.zone.shp@data %>%
  left_join(tnc.means,by='LocationID')
                    
taxi.zone.shp@data <- taxi.zone.shp@data %>%
  left_join(taxi.means, by = 'LocationID')


# Make nearest neighbor lists
nb.tnc <- poly2nb(tnc.zone.shp, queen=T)
nb.taxi <- poly2nb(taxi.zone.shp, queen=T)

plot(tnc.zone.shp,border='black')
plot(nb.tnc,coordinates(tnc.zone.shp),add=T,col='red')

plot(taxi.zone.shp,border='black')
plot(nb.taxi,coordinates(taxi.zone.shp),add=T,col='red')

# equal weight
lw.nb.tnc <- nb2listw(nb.tnc, style="W", zero.policy=TRUE)
lw.nb.taxi <- nb2listw(nb.taxi, style="W", zero.policy=TRUE)

# Global Moran Tests
moran.test(tnc.zone.shp$`Mean`,lw.nb.tnc,zero.policy = T)
moran.test(tnc.zone.shp$`Mean-15`,lw.nb.tnc,zero.policy = T)
moran.test(tnc.zone.shp$`Mean-16`,lw.nb.tnc,zero.policy = T)
moran.test(tnc.zone.shp$`Mean-17`,lw.nb.tnc,zero.policy = T)

moran.test(taxi.zone.shp$`Mean`,lw.nb.taxi,zero.policy = T)
moran.test(taxi.zone.shp$`Mean-15`,lw.nb.taxi,zero.policy = T)
moran.test(taxi.zone.shp$`Mean-16`,lw.nb.taxi,zero.policy = T)
moran.test(taxi.zone.shp$`Mean-17`,lw.nb.taxi,zero.policy = T)

# Local Moran Tests
local.moran.tnc=localmoran(tnc.zone.shp$`Mean`,lw.nb.tnc,zero.policy = T)
local.moran.tnc.2015=localmoran(tnc.zone.shp$`Mean-15`,lw.nb.tnc,zero.policy = T)
local.moran.tnc.2016=localmoran(tnc.zone.shp$`Mean-16`,lw.nb.tnc,zero.policy = T)
local.moran.tnc.2017=localmoran(tnc.zone.shp$`Mean-17`,lw.nb.tnc,zero.policy = T)

local.moran.taxi=localmoran(taxi.zone.shp$`Mean`,lw.nb.taxi,zero.policy = T)
local.moran.taxi.2015=localmoran(taxi.zone.shp$`Mean-15`,lw.nb.taxi,zero.policy = T)
local.moran.taxi.2016=localmoran(taxi.zone.shp$`Mean-16`,lw.nb.taxi,zero.policy = T)
local.moran.taxi.2017=localmoran(taxi.zone.shp$`Mean-17`,lw.nb.taxi,zero.policy = T)

# Add results of local moran's tests to shapefile

# Setting up different tnc polygon dfs including differences by year
tnc.zone.shp.full=tnc.zone.shp
tnc.zone.shp.2015=tnc.zone.shp
tnc.zone.shp.2016=tnc.zone.shp
tnc.zone.shp.2017=tnc.zone.shp

tnc.zone.shp.full$loc.moran.tnc <- local.moran.tnc[,1]
tnc.zone.shp.full$tnc.pval <- local.moran.tnc[,5]

tnc.zone.shp.2015$loc.moran.tnc <- local.moran.tnc.2015[,1]
tnc.zone.shp.2015$tnc.pval <- local.moran.tnc.2015[,5]

tnc.zone.shp.2016$loc.moran.tnc <- local.moran.tnc.2016[,1]
tnc.zone.shp.2016$tnc.pval <- local.moran.tnc.2016[,5]

tnc.zone.shp.2017$loc.moran.tnc <- local.moran.tnc.2017[,1]
tnc.zone.shp.2017$tnc.pval <- local.moran.tnc.2017[,5]



# Setting up different polygon dfs for taxi-by year
taxi.zone.shp.full=taxi.zone.shp
taxi.zone.shp.2015=taxi.zone.shp
taxi.zone.shp.2016=taxi.zone.shp
taxi.zone.shp.2017=taxi.zone.shp

taxi.zone.shp.full$loc.moran.taxi <- local.moran.taxi[,1]
taxi.zone.shp.full$taxi.pval <- local.moran.taxi[,5]

taxi.zone.shp.2015$loc.moran.taxi <- local.moran.taxi.2015[,1]
taxi.zone.shp.2015$taxi.pval <- local.moran.taxi.2015[,5]

taxi.zone.shp.2016$loc.moran.taxi <- local.moran.taxi.2016[,1]
taxi.zone.shp.2016$taxi.pval <- local.moran.taxi.2016[,5]

taxi.zone.shp.2017$loc.moran.taxi <- local.moran.taxi.2017[,1]
taxi.zone.shp.2017$taxi.pval <- local.moran.taxi.2017[,5]

# Set up shading scheme
pval.breaks=c(0,0.01,0.05,0.1,1)


# Moran's I Plots 
palette="PRGn"

# TNC Moran Plots

tnc.moranI.plt.full=tm_shape(tnc.zone.shp.full,bbox = bbinfor.tnc) +
  tm_polygons("loc.moran.tnc",palette = palette,style = "kmeans",showNA=F,title="Local Moran's I")+
  tm_layout(legend.show = T,title = "TNC",legend.position = c('left',"top"),title.size = 0.8)


# Taxi Moran Plots

taxi.moranI.plt.full=tm_shape(taxi.zone.shp.full,bbox = bbinfor.taxi) +
  tm_polygons("loc.moran.taxi",palette = palette,style = "kmeans",showNA=F,title="Local Moran's I")+
  tm_layout(legend.show = T,title = "Taxi",legend.position = c('left',"top"),title.size = 0.8)

# P-value plots
# Set up a color palette for the p-values
palette=c("red","orangered","orange","khaki")

# TNC P-Value Plots

tnc.pval.plt.full=tm_shape(tnc.zone.shp.full,bbox = bbinfor.tnc) +
  tm_polygons("tnc.pval",palette = palette,style = "fixed",breaks=pval.breaks,showNA=F,title="P-Values")+
  tm_layout(legend.show = T,title = "TNC",title.size = 0.8,legend.position = c('left','top')) 

# Taxi P-Value Plots

taxi.pval.plt.full=tm_shape(taxi.zone.shp.full,bbox = bbinfor.taxi) +
  tm_polygons("taxi.pval",palette = palette,style = "fixed",breaks=pval.breaks,showNA=F,title="P-Values")+
  tm_layout(legend.show = T,title = "Taxi",title.size = 0.8,legend.position = c('left','top')) 

grid.newpage()
page.layout <- grid.layout(nrow = 2, ncol = 2,widths = rep(2.5,4),heights = rep(3.75,2),default.units = "inches" )
pushViewport(viewport(layout = page.layout))

print(tnc.moranI.plt.full, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(tnc.pval.plt.full, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(taxi.moranI.plt.full, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(taxi.pval.plt.full, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
