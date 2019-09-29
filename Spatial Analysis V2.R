## Spatial Analysis
## Read data
## Residual Analysis - Spatial autocorrelation
rm(list=ls())
library(raster)
library(tmap)
library(sf)
library(spdep)
library(mapview)
library(gridExtra)
## Read data
filepath="C:\\Users\\jiz13007\\OneDrive - University of Connecticut\\Rideshare\\Latest Version\\Data\\Modal Data\\"
taxi.model<-readRDS(paste0(filepath,"Model Result\\taxi_model_result_v2.rds")); 
tnc.model<-readRDS(paste0(filepath,"Model Result\\tnc_model_result_v2.rds")); 
pickupdate<-readRDS(paste0(filepath, 'Weekly Data\\weekly_pickupdate.rds'))

# Get temporally clean residuals from ARIMA and GARCH model


taxi.res <-data.frame(taxi.model$clean.residual); tnc.res <-data.frame(tnc.model$clean.residual)

taxi.res$pickup_date=pickupdate; tnc.res$pickup_date = pickupdate
dim(taxi.res); dim(tnc.res)
saveRDS(taxi.res,paste0(filepath,"Model Result\\taxi_weekly_residuals.rds") )
saveRDS(tnc.res,paste0(filepath,"Model Result\\tnc_weekly_residuals.rds") )

## Geo information
zoneinfor=read.csv(paste0(filepath,"Shapefile\\taxi_zone_lookup.csv"))
zoneshape=st_read(paste0(filepath,"Shapefile\\taxi_zones.shp"))
windows()
tm_shape(zoneshape)+ tm_polygons(style="quantile", col = "borough")

taxi.lastcol = dim(taxi.res)[2]-1; tnc.lastcol = dim(tnc.res)[2]-1
## Pearson Correlation
taxi.cor.matrix=cor(taxi.res[,1:taxi.lastcol]); dim(taxi.cor.matrix); 
tnc.cor.matrix=cor(tnc.res[,1:tnc.lastcol]); dim(tnc.cor.matrix);

## transpose data
tres.taxi=t(taxi.res[,1: taxi.lastcol]); dim(tres.taxi)
trans.colname<-paste0("t_", seq(1,129,1)); length(trans.colname)
colnames(tres.taxi) = trans.colname
## locationID
LocationID=substr(rownames(tres.taxi),2,4); LocationID[1:4]

tres.taxi=as.data.frame(tres.taxi)
tres.taxi$all = rowSums(tres.taxi)
tres.taxi$LocationID=LocationID

res.inner=merge(x = zoneshape, y = tres.taxi, by = "LocationID", all.x = FALSE); dim(res.inner)
res.out=merge(x = zoneshape, y = tres.taxi, by = "LocationID", all.x = TRUE); dim(res.out)

## transpose data - tnc
dim(tnc.res)
tres.tnc=t(tnc.res[,1:tnc.lastcol]); dim(tres.tnc)
colnames(tres.tnc) = trans.colname
## locationID
LocationID.tnc=substr(rownames(tres.tnc),3,5); LocationID.tnc[1:5]
tres.tnc=as.data.frame(tres.tnc)
tres.tnc$all = rowSums(tres.tnc)
tres.tnc$LocationID=LocationID.tnc

res.inner.tnc=merge(x = zoneshape, y = tres.tnc, by = "LocationID", all.x = FALSE); dim(res.inner.tnc)

bbinfor = tmaptools::bb(res.inner)
bbinfor['ymax'] = bbinfor['ymax']+40000
  
tnc.subplot = tm_shape(res.inner.tnc, bbox=bbinfor)+tm_polygons(style="quantile", col = "borough")+
  tm_layout(title ='Subset Taxi Zones - TNC')
taxi.subplot =tm_shape(res.inner, bbox=bbinfor)+ tm_polygons(style="quantile", col = "borough")+
  tm_layout(title='Subset Taxi Zones - Taxi')
windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(tnc.subplot, vp=viewport(layout.pos.col = 1))
print(taxi.subplot, vp=viewport(layout.pos.col = 2))


#######################################################################################
#####################  Global Moran's I  - All ########################################
#######################################################################################
moran_clac = function(shpdata, x){
  
  nb <- poly2nb(shpdata, queen=TRUE)
  lw <- nb2listw(nb, style="W", zero.policy=TRUE)
  tmp.lag <- lag.listw(lw, as.numeric(as.character(shpdata[[x]])), zero.policy = TRUE)
  
  #Computing the Moran's I statistic
  test.result=moran.test(as.numeric(as.character(shpdata[[x]])),lw, zero.policy=TRUE)
  
  return(test.result)
}


moran_output<-function(moran_list){
  stat_list = c()
  pvalue_list =c()
  for (i in 1:length(moran_list)){
    tmp_stat = round(moran_list[[i]]$statistic,4)
    tmp_pvalue = round(moran_list[[i]]$p.value,4)
    stat_list<-c(stat_list, tmp_stat)
    pvalue_list<-c(pvalue_list, tmp_pvalue)
  }
  output = cbind(stat_list, pvalue_list)
  colnames(output)=c('Statistic', 'Pvalue')
  return(data.frame(output))
}

tnc.moran.all = moran_clac(res.inner.tnc,'all')
taxi.moran.all = moran_clac(res.inner, 'all')
moran_all_list = list(tnc.moran.all, taxi.moran.all)
length(moran_all_list)

output = moran_output(moran_all_list)
print(output, row.names = FALSE)

#######################################################################################
#####################  Global Moran's I  - Segment ####################################
#######################################################################################
agg_seg = function(tresdata, shpdata){
  segcol1<-paste0("t_",seq(2,33,1)); print(length(segcol1))
  segcol2<-paste0("t_",seq(34,65,1)); print(length(segcol2))
  segcol3<-paste0("t_",seq(66,97,1)); print(length(segcol3))
  segcol4<-paste0("t_",seq(98,129,1)); print(length(segcol4))
  tmp_seg1 = rowMeans( tresdata[,segcol1], dims = 1)
  tmp_seg2 = rowMeans( tresdata[,segcol2], dims = 1)
  tmp_seg3 = rowMeans( tresdata[,segcol3], dims = 1)
  tmp_seg4 = rowMeans( tresdata[,segcol4], dims = 1)
  tmp.data = data.frame(cbind(tmp_seg1, tmp_seg2, tmp_seg3, tmp_seg4, tresdata$LocationID))
  colnames(tmp.data)<-c('Segment1','Segment2','Segment3','Segment4','LocationID')
  newshp = merge(x = shpdata, y =  tmp.data, by = "LocationID", all.x = TRUE)
  print (dim(newshp)); print(dim(shpdata)); print(dim(tmp.data))
  
  return(newshp)
}

seg.tnc = agg_seg(tres.tnc,res.inner.tnc)
seg.tnc$Segment1[1:5];as.numeric(as.character((seg.tnc$Segment1)))[1:5]
tnc.moran.seg1 = moran_clac(seg.tnc,'Segment1'); tnc.moran.seg1
tnc.moran.seg2 = moran_clac(seg.tnc,'Segment2'); tnc.moran.seg2
tnc.moran.seg3 = moran_clac(seg.tnc,'Segment3'); tnc.moran.seg3
tnc.moran.seg4 = moran_clac(seg.tnc,'Segment4'); tnc.moran.seg4


seg.taxi = agg_seg(tres.taxi,res.inner)
seg.taxi$Segment1[1:5];as.numeric(as.character((seg.taxi$Segment1)))[1:5]
taxi.moran.seg1 = moran_clac(seg.taxi,'Segment1'); taxi.moran.seg1
taxi.moran.seg2 = moran_clac(seg.taxi,'Segment2'); taxi.moran.seg2
taxi.moran.seg3 = moran_clac(seg.taxi,'Segment3'); taxi.moran.seg3
taxi.moran.seg4 = moran_clac(seg.taxi,'Segment4'); taxi.moran.seg4
taxi.moran.all = moran_clac(res.inner,'all')
taxi.moran.all

output_seg_tnc = moran_output(list(tnc.moran.seg1, tnc.moran.seg2, tnc.moran.seg3, tnc.moran.seg4, tnc.moran.all))
print(output, row.names = FALSE)

output_seg_taxi = moran_output(list(taxi.moran.seg1, taxi.moran.seg2, taxi.moran.seg3, taxi.moran.seg4, taxi.moran.all))
print(output_seg_taxi, row.names = FALSE)

#######################################################################################
#####################  Global Moran's I  - Month ####################################
#######################################################################################

pickdate2 = as.Date(pickupdate,format='%Y-%m-%d'); pickdate2
length(pickdate2)
pickmonth<- strftime(pickdate2, "%m")
## get index of each month
month.idx =list()
for (i in (1:12)){
  if(i<10){month.name = paste0('0',i)}else(month.name = as.character(i))
  month.idx[[i]] = which(pickmonth == month.name)
}

agg_month<-function(tresdata, shpdata){
  tmp.data =c()
  for (i in (1:12)){
    indx.list = month.idx[[i]]
    tmp.col = paste0('t_',indx.list)
    tmp.mean = rowMeans( tresdata[,tmp.col], dims = 1)
    tmp.data = cbind(tmp.data, tmp.mean)
  }
  tmp.data = data.frame(tmp.data)
  colnames(tmp.data) = paste0('m_', seq(1,12))
  tmp.data$LocationID=tresdata$LocationID
  newshp = merge(x = shpdata, y =  tmp.data, by = "LocationID", all.x = TRUE)
  print (dim(newshp)); print(dim(shpdata)); print(dim(tmp.data))
  return(newshp)
  
}

month.tnc = agg_month(tres.tnc,res.inner.tnc)
month.taxi = agg_month(tres.taxi,res.inner)
month.tnc$m_2[1:5];as.numeric(as.character((month.tnc$m_2)))[1:5]  
month.taxi$m_2[1:5];as.numeric(as.character((month.taxi$m_2)))[1:5]  

month.moran = function(shpdata, varlist){
  tmp.value = c(); tmp.p=c()
  for (var in varlist){
    test.result = moran_clac(shpdata, var)
    test.moran = round(test.result$statistic,4)
    test.pvalue = round(test.result$p.value,4)
    tmp.value = c(tmp.value, test.moran)
    tmp.p = c(tmp.p, test.pvalue)
  }
  tmp.data = cbind(tmp.value, tmp.p)
  return(data.frame(tmp.data))
}

month.tnc.moran = month.moran(month.tnc, paste0('m_',seq(1,12)))
month.taxi.moran = month.moran(month.taxi, paste0('m_',seq(1,12)))
print(month.tnc.moran, row.names = FALSE)
print(month.taxi.moran, row.names = FALSE)


#######################################################################################
#####################  Global Moran's I  - Season ####################################
#######################################################################################
season.collist<-list()
season.collist[[1]] = which(pickmonth %in% c('02','03','04'))
season.collist[[2]] = which(pickmonth %in% c('05','06','07'))
season.collist[[3]] = which(pickmonth %in% c('08','09','10'))
season.collist[[4]] = which(pickmonth %in% c('11','12','01'))

# season.collist<-list()
# for (i in (1:12)){
#   if(i<10){month.name = paste0('0',i)}else(month.name = as.character(i))
#   month.idx[[i]] = which(pickmonth == month.name)
# }


agg_sea<-function(tresdata, shpdata){
  tmp.data =c()
  for (i in (1:4)){
    indx.list = season.collist[[i]]
    tmp.col = paste0('t_',indx.list)
    tmp.mean = rowMeans( tresdata[,tmp.col], dims = 1)
    tmp.data = cbind(tmp.data, tmp.mean)
  }
  tmp.data = data.frame(tmp.data)
  colnames(tmp.data) = paste0('s_', seq(1,4))
  tmp.data$LocationID=tresdata$LocationID
  newshp = merge(x = shpdata, y =  tmp.data, by = "LocationID", all.x = TRUE)
  print (dim(newshp)); print(dim(shpdata)); print(dim(tmp.data))
  return(newshp)
}


season.tnc = agg_sea(tres.tnc,month.tnc)
season.taxi = agg_sea(tres.taxi,month.taxi)
season.tnc$s_2[1:5];as.numeric(as.character((season.tnc$s_2)))[1:5]  
season.taxi$s_2[1:5];as.numeric(as.character((season.taxi$s_2)))[1:5]  

season.tnc.moran = month.moran(season.tnc, paste0('s_',seq(1,4)))
season.taxi.moran = month.moran(season.taxi, paste0('s_',seq(1,4)))
print(season.tnc.moran, row.names = FALSE)
print(season.taxi.moran, row.names = FALSE)



#######################################################################################
#####################  Local Moran's I  - All      ####################################
#######################################################################################
moran_calc_local = function(shpdata, x){
  
  nb <- poly2nb(shpdata, queen=TRUE)
  lw <- nb2listw(nb, style="W", zero.policy=TRUE)
  tmp.lag <- lag.listw(lw, as.numeric(as.character(shpdata[[x]])), zero.policy = TRUE)
  
  #Computing the Moran's I statistic
  test.result=data.frame(localmoran(as.numeric(as.character(shpdata[[x]])),lw, zero.policy=TRUE))
  colnames(test.result) =c('Ii', 'EIi', 'VarIi','ZIi','Pvalue')
  test.result$Ii2 = test.result$Ii
  test.result$Ii2[(test.result$Pvalue>0.05)] = NA
  shpdata$Local_MoranI = test.result$Ii
  shpdata$Pvalue =test.result$Pvalue
  shpdata$Local_MoranI_recode = test.result$Ii2
  return(shpdata)
}

res.inner.local = moran_calc_local(res.inner,'all')
res.inner.tnc.local = moran_calc_local(res.inner.tnc, 'all')
View(res.inner.local)


## Plot Moran'I 
plot_tnc_all=tm_shape(res.inner.tnc.local, bbox = bbinfor)+tm_polygons( style="quantile", col = "Local_MoranI")+
  tm_layout(title ='TNC: Local Morans I - All')
plot_taxi_all=tm_shape(res.inner.local, bbox = bbinfor)+tm_polygons( style="quantile", col = "Local_MoranI")+
  tm_layout(title ='Taxi: Local Morans I - All')
windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))


## Plot Moran'I Recode
plot_tnc_all=tm_shape(res.inner.tnc.local, bbox = bbinfor)+tm_polygons( style="quantile", col = "Local_MoranI_recode")+
  tm_layout(title ='TNC: Local Morans I - All')

plot_taxi_all=tm_shape(res.inner.local, bbox = bbinfor)+tm_polygons( style="quantile", col = "Local_MoranI_recode")+
  tm_layout(title ='Taxi: Local Morans I - All')

windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))


## Plot p-value
pval.breaks=c(0,0.01,0.05,0.1,1)
plot_tnc_all=tm_shape(res.inner.tnc.local, bbox = bbinfor)+
  tm_polygons(style="fixed",breaks=pval.breaks,showNA=F,col = "Pvalue")+
  tm_layout(title ='TNC: Local Morans I - All')

plot_taxi_all=tm_shape(res.inner.local, bbox = bbinfor)+
  tm_polygons( style="fixed",breaks=pval.breaks,showNA=F,col = "Pvalue")+
  tm_layout(title ='Taxi: Local Morans I - All')

windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))



#######################################################################################
#####################  Local Moran's I  - Segmentation     ############################
#######################################################################################
moran_calc_local = function(shpdata, x_list){
  nb <- poly2nb(shpdata, queen=TRUE)
  lw <- nb2listw(nb, style="W", zero.policy=TRUE)
  
  for (x in x_list){
    print(x)
    #Computing the Moran's I statistic
    tmp.lag <- lag.listw(lw, as.numeric(as.character(shpdata[[x]])), zero.policy = TRUE)
    test.result=data.frame(localmoran(as.numeric(as.character(shpdata[[x]])),lw, zero.policy=TRUE))
    print(dim(test.result))
    colnames(test.result) =c('Ii', 'EIi', 'VarIi','ZIi','Pvalue')
    test.result$Ii2 = test.result$Ii
    test.result$Ii2[(test.result$Pvalue>0.05)] = NA
    moran_name = paste0('Local_MoranI_',x);moran_name2 = paste0('Local_MoranI_recode_',x)
    p_name = paste0('Pvalue_',x)
    shpdata[[moran_name]] = test.result$Ii
    shpdata[[moran_name2]] = test.result$Ii2
    shpdata[[p_name]] = test.result$Pvalue
  }
  return(shpdata)
}

x_list = c('Segment1','Segment2', 'Segment3', 'Segment4')
seg.tnc.local = moran_calc_local(seg.tnc,x_list)
seg.taxi.local = moran_calc_local(seg.taxi,c('Segment1','Segment2', 'Segment3', 'Segment4'))

x = 'Segment4'
## Plot Moran'I 
plot_tnc_all=tm_shape(seg.tnc.local, bbox = bbinfor)+tm_polygons( style="quantile", col = paste0('Local_MoranI_',x))+
  tm_layout(title =paste0('TNC: Local Morans I - ', x))
plot_taxi_all=tm_shape(seg.taxi.local, bbox = bbinfor)+tm_polygons( style="quantile", col =paste0('Local_MoranI_',x))+
  tm_layout(title =paste0('Taxi: Local Morans I - ', x))
windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))


## Plot Moran'I 
plot_tnc_all=tm_shape(seg.tnc.local, bbox = bbinfor)+tm_polygons( style="quantile", col = paste0('Local_MoranI_recode_',x))+
  tm_layout(title =paste0('TNC: Recoded Local Morans I - ', x))
plot_taxi_all=tm_shape(seg.taxi.local, bbox = bbinfor)+tm_polygons( style="quantile", col = paste0('Local_MoranI_recode_',x))+
  tm_layout(title =paste0('Taxi: Recoded Local Morans I - ', x))
windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))


## Plot p-value
pval.breaks=c(0,0.01,0.05,0.1,1)
plot_tnc_all=tm_shape(seg.tnc.local, bbox = bbinfor)+
  tm_polygons(style="fixed",breaks=pval.breaks,showNA=F,col = paste0("Pvalue_",x))+
  tm_layout(title =paste0('TNC: P Value of Local Morans I - ', x))

plot_taxi_all=tm_shape(seg.taxi.local, bbox = bbinfor)+
  tm_polygons( style="fixed",breaks=pval.breaks,showNA=F,col = paste0("Pvalue_",x))+
  tm_layout(title =paste0('Taxi: P Value Local Morans I - ', x))

windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))




#######################################################################################
#####################  Local Moran's I  - Season     ############################
#######################################################################################
moran_calc_local = function(shpdata, x_list){
  nb <- poly2nb(shpdata, queen=TRUE)
  lw <- nb2listw(nb, style="W", zero.policy=TRUE)
  
  for (x in x_list){
    print(x)
    #Computing the Moran's I statistic
    tmp.lag <- lag.listw(lw, as.numeric(as.character(shpdata[[x]])), zero.policy = TRUE)
    test.result=data.frame(localmoran(as.numeric(as.character(shpdata[[x]])),lw, zero.policy=TRUE))
    print(dim(test.result))
    colnames(test.result) =c('Ii', 'EIi', 'VarIi','ZIi','Pvalue')
    test.result$Ii2 = test.result$Ii
    test.result$Ii2[(test.result$Pvalue>0.05)] = NA
    moran_name = paste0('Local_MoranI_',x);moran_name2 = paste0('Local_MoranI_recode_',x)
    p_name = paste0('Pvalue_',x)
    shpdata[[moran_name]] = test.result$Ii
    shpdata[[moran_name2]] = test.result$Ii2
    shpdata[[p_name]] = test.result$Pvalue
  }
  return(shpdata)
}

x_list = c('Segment1','Segment2', 'Segment3', 'Segment4')
seg.tnc.local = moran_calc_local(seg.tnc,x_list)
seg.taxi.local = moran_calc_local(seg.taxi,c('Segment1','Segment2', 'Segment3', 'Segment4'))

x = 'Segment4'
## Plot Moran'I 
plot_tnc_all=tm_shape(seg.tnc.local, bbox = bbinfor)+tm_polygons( style="quantile", col = paste0('Local_MoranI_',x))+
  tm_layout(title =paste0('TNC: Local Morans I - ', x))
plot_taxi_all=tm_shape(seg.taxi.local, bbox = bbinfor)+tm_polygons( style="quantile", col =paste0('Local_MoranI_',x))+
  tm_layout(title =paste0('Taxi: Local Morans I - ', x))
windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))


## Plot Moran'I 
plot_tnc_all=tm_shape(seg.tnc.local, bbox = bbinfor)+tm_polygons( style="quantile", col = paste0('Local_MoranI_recode_',x))+
  tm_layout(title =paste0('TNC: Recoded Local Morans I - ', x))
plot_taxi_all=tm_shape(seg.taxi.local, bbox = bbinfor)+tm_polygons( style="quantile", col = paste0('Local_MoranI_recode_',x))+
  tm_layout(title =paste0('Taxi: Recoded Local Morans I - ', x))
windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))


## Plot p-value
pval.breaks=c(0,0.01,0.05,0.1,1)
plot_tnc_all=tm_shape(seg.tnc.local, bbox = bbinfor)+
  tm_polygons(style="fixed",breaks=pval.breaks,showNA=F,col = paste0("Pvalue_",x))+
  tm_layout(title =paste0('TNC: P Value of Local Morans I - ', x))

plot_taxi_all=tm_shape(seg.taxi.local, bbox = bbinfor)+
  tm_polygons( style="fixed",breaks=pval.breaks,showNA=F,col = paste0("Pvalue_",x))+
  tm_layout(title =paste0('Taxi: P Value Local Morans I - ', x))

windows()
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_tnc_all, vp=viewport(layout.pos.col = 1))
print(plot_taxi_all, vp=viewport(layout.pos.col = 2))