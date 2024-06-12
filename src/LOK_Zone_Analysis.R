## Title:      Lake Okeechobee Zone analysis
##             Analysis like Phlips et al. 1993
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2023-04-24

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

# GIS 
library(sf)
library(sp)
library(raster)
library(tmap)

## tables
library(flextable)
library(magrittr)
## Paths
wd="C:/Julian_LaCie/_GitHub/LakeO_WQ"

paths=paste0(wd,c("/Plots/Cyano/","/Export/Cyano/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")

# Helper variables
# epsg.io
nad83.pro=st_crs("EPSG:4269")
utm17=st_crs("EPSG:26917")
utm17.wgs=st_crs("EPSG:9001")
utm17.sp=sp::CRS("+init=epsg:26917")
# nad83.pro=CRS("EPSG:4269")
# utm17=CRS("EPSG:26917")

tmap_mode("view")
# GIS layers --------------------------------------------------------------
lakeO=st_transform(st_read(paste0(GIS.path.gen,"/SFWMD"),"LakeOkeechobee_general"),utm17)
plot(st_geometry(lakeO))
st_area(lakeO)

lakeO.lit=st_transform(st_read("C:/Julian_LaCie/_GitHub/CRE_Conditions/report/GISData","LOK_littoral"),utm17)

LOK.area=st_area(lakeO)-st_area(lakeO.lit)

wmd.monitoring=st_transform(st_read(paste0(GIS.path.gen,"/SFWMD_Monitoring_20240125"),"DBHYDRO_SITE_STATION"),utm17)

wq.mon=subset(wmd.monitoring,ACTIVITY_S=="Surface Water Grab")
LOK.wq.mon.all=wq.mon[st_buffer(lakeO,-500),]
LOK.wq.mon.xlit=LOK.wq.mon.all[st_difference(lakeO,lakeO.lit),]
LOK.wq.mon.xlit2=subset(LOK.wq.mon.xlit,START_DATE<=as.Date("1988-10-01")&END_DATE<=as.Date("1989-09-30"))

plot(st_geometry(lakeO))
plot(st_geometry(LOK.wq.mon),add=T)
plot(st_geometry(LOK.wq.mon.xlit),add=T,pch=21,bg="red")
plot(st_geometry(lakeO.lit),border="red",add=T)
plot(st_geometry(LOK.wq.mon.xlit2),add=T,pch=21,bg="blue")

## Similar to Phlips 
west_east_trans=c("L005","L008","LZ40","L004","LZ15" ); #FEBIN FEBOUT starts 1996 
north_south_trans=c("S191N1.5","L001","L002","LZ40","L006","L007","RITAEAST")

plot(st_geometry(subset(LOK.wq.mon.xlit,STATION%in%west_east_trans)),add=T,pch=22,bg="black",cex=1.5)
plot(st_geometry(subset(LOK.wq.mon.xlit,STATION%in%north_south_trans)),add=T,pch=22,bg="black",cex=1.5)

tm_shape(LOK.wq.mon.xlit)+tm_dots()+
  tm_shape(LOK.wq.mon.all)+tm_dots("blue")+
  tm_shape(LOK.wq.mon.xlit2)+tm_dots("green")+
  # tm_shape(subset(LOK.wq.mon.xlit,STATION%in%west_east_trans))+tm_dots(shape=18,size=0.5)


# Similar time period from Phlips -----------------------------------------
# To understand work flow and see how far we can get with DBHYDRO data
# if successful expand analysis to develop it for an annual metric 
## Analysis idea use discrimiation analysis to identify zone (by station)
## use categorical interpolation (knn ..like with sediment type map) to 
## deliniate zones and see how they change over the years with average 
## annual water level ("habitat" area metric)


## WQ ----------------------------------------------------------------------
dates=date.fun(c("1987-05-10","1990-04-30")); # slightly different dates than Phlips et al
wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100,7,61,179,112),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC","WT","Chla","Chla","Chla"))
inlake.sites=unique(c(LOK.wq.mon.xlit2$STATION,west_east_trans,north_south_trans))# c("L001", "L004", "L005", "L006", "L007", "L008", "LZ40", "LZ30")

wq.inlake.dat=data.frame()
for(i in 1:length(inlake.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],inlake.sites[i],wq.param$Test.Number)
  wq.inlake.dat=rbind(tmp,wq.inlake.dat)
  print(paste0(i,": ",inlake.sites[i]))
}


wq.inlake.dat=merge(wq.inlake.dat,wq.param,"Test.Number")
wq.inlake.dat$WY=WY(wq.inlake.dat$Date.EST)

wq.inlake.dat.Nsamp=ddply(wq.inlake.dat,c("Station.ID","Param"),summarise,N.val=N.obs(HalfMDL))

wq.inlake.dat.xtab=dcast(subset(wq.inlake.dat,!Param%in%c("WT")),Station.ID+Date.EST+WY~Param,value.var = "HalfMDL",mean,na.rm=T)#  fun.aggregate =function(x) exp(mean(log(x),na.rm=T)))

wq.inlake.dat.xtab$TN=NA
wq.inlake.dat.xtab$TN=with(wq.inlake.dat.xtab,TN_Combine(NOx,TKN,TN))
wq.inlake.dat.xtab$DIN=with(wq.inlake.dat.xtab,NH4+NOx)

wq.inlake.dat.xtab.melt=melt(wq.inlake.dat.xtab,id.vars = c("Station.ID","Date.EST","WY"))
wq.inlake.dat.xtab.melt$CY=as.numeric(format(wq.inlake.dat.xtab.melt$Date.EST,"%Y"))
wq.inlake.dat.xtab.melt$dec.DOY=with(wq.inlake.dat.xtab.melt,as.numeric(format(Date.EST,"%j"))/as.numeric(format(date.fun(paste0(CY,"-12-31")),"%j")))
# wq.inlake.dat.xtab.melt$dec.DOWY=with(wq.inlake.dat.xtab.melt,hydro.day(Date.EST)/hydro.day(date.fun(paste0(WY,"-04-30"))))
wq.inlake.dat.xtab.melt$dec.WY=with(wq.inlake.dat.xtab.melt,decimal.WY(Date.EST)-WY)
                                     
wq.inlake.dat.xtab.means=ddply(wq.inlake.dat.xtab.melt,c("Station.ID","WY","variable"),summarise,
           mean.val=mean(value,na.rm=T),
           geomean=exp(mean(log(value),na.rm=T)),
           time.wm=Hmisc::wtd.mean(value,dec.WY,na.rm=T),
           time.wd=sqrt(Hmisc::wtd.var(value,dec.WY,na.rm=T)))
plot(time.wm~mean.val,subset(wq.inlake.dat.xtab.means,variable=="TP"));abline(0,1)
plot(time.wm~mean.val,subset(wq.inlake.dat.xtab.means,variable=="TN"));abline(0,1)
plot(time.wm~mean.val,subset(wq.inlake.dat.xtab.means,variable=="Chla"));abline(0,1)
plot(time.wm~mean.val,subset(wq.inlake.dat.xtab.means,variable=="TSS"));abline(0,1)

# wq.inlake.dat.xtab2=dcast(wq.inlake.dat.xtab.melt,Station.ID+WY~variable,value.var = "value",fun.aggregate =function(x) exp(mean(log(x),na.rm=T)))
wq.inlake.dat.xtab2=dcast(wq.inlake.dat.xtab.means,Station.ID+WY~variable,value.var = "time.wm",mean,na.rm=T)

wq.inlake.dat.xtab2$Station.ID=as.factor(wq.inlake.dat.xtab2$Station.ID)
## Transect like analysis --------------------------------------------------
## North South
ns_trans_wq=subset(wq.inlake.dat.xtab2,Station.ID%in%north_south_trans)
ns_trans_wq$Station.ID=factor(ns_trans_wq$Station.ID,levels=north_south_trans)
ns_trans_wq=ns_trans_wq[order(ns_trans_wq$Station.ID),]

test=glm(Chla+TP+TN+TSS~Station.ID,ns_trans_wq,family = gaussian)
summary(test)
hist(residuals(test))

test.aov=aov(Chla+TP+TN+TSS~Station.ID,ns_trans_wq)
summary(test.aov)
test.hsd=TukeyHSD(test.aov)
plot(test.hsd)

test.hsd2=agricolae::HSD.test(test.aov,trt="Station.ID")
test.hsd2

barplot(subset(ns_trans_wq,WY==1988)$Chla)
barplot(subset(ns_trans_wq,WY==1988)$TP)
barplot(subset(ns_trans_wq,WY==1988)$TN)
barplot(subset(ns_trans_wq,WY==1988)$TSS)

## West East 
we_trans_wq=subset(wq.inlake.dat.xtab2,Station.ID%in%west_east_trans)# subset(wq.inlake.dat.xtab,Station.ID%in%west_east_trans)
we_trans_wq$Station.ID=factor(we_trans_wq$Station.ID,levels=west_east_trans)

test=glm(Chla+TP+TN+TSS~Station.ID,we_trans_wq,family = gaussian)
summary(test)
hist(residuals(test))

test.aov=aov(Chla+TP+TN+TSS~Station.ID,we_trans_wq)
summary(test.aov)
test.hsd=TukeyHSD(test.aov)
plot(test.hsd)

test.hsd2=agricolae::HSD.test(test.aov,trt="Station.ID")
test.hsd2

barplot(subset(we_trans_wq,WY==1988)$Chla)
barplot(subset(we_trans_wq,WY==1988)$TP)
barplot(subset(we_trans_wq,WY==1988)$TN)
barplot(subset(we_trans_wq,WY==1988)$TSS)


## Discrimination Analysis -------------------------------------------------
library(car)
library(MASS)

## Example Code https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# linear DA
vars=c("Station.ID","Chla","TP","TN","TSS")
LOK.lda=lda(Station.ID ~.,data=wq.inlake.dat.xtab2[,vars])
summary(LOK.lda)


LOK.lda.values <- predict(LOK.lda)
ldahist(data = LOK.lda.values$x[,4])

library(klaR)
partimat(Station.ID~.,data=wq.inlake.dat.xtab2[,vars],method="lda") 
