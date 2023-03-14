## 
## Lake Okeechobee - Water Quality Update
##
## Code was compiled by Paul Julian
## contact info: pjulian@evergladesfoundation.org 

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(gstat)

#thin plate spline https://rspatial.org/raster/analysis/4-interpolation.html
library(fields)
library(raster)

library(flextable)
library(magrittr)


## Paths
wd="C:/Julian_LaCie/_Github/LakeO_WQ"

paths=paste0(wd,c("/Plots/","/export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"

# Helper variables
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")

leg.fun=function(b,pal,leg.title,
                 top.val=0.8,bot.val=0.2,mid.v.val=NULL,
                 x.max=0.3,x.min=0.1,mid.val=NULL,
                 txt.offset.val=-0.01,txt.y=NULL,leg.txt=NULL,
                 txt.cex=0.75,txt.adj=0,txt.pos=4,txt.offset=0.5,
                 title.cex=0.8,title.pos=3,title.adj=0,
                 title.x=NULL,title.y=NULL,
                 leg.type=c("continuous","categorical"), ...){
  l.b=length(b)
  labs=c(paste0("< ",b[2]),paste(b[2:(l.b-2)],b[3:(l.b-1)],sep=" - "),paste(paste0(">",b[(l.b-1)])))
  n.bks=length(b)-1
  mid.v.val=if(is.null(mid.v.val)==T){bot.val+(top.val-bot.val)/2}else{mid.v.val}
  
  mid.val=if(is.null(mid.val)==T){x.min+(x.max-x.min)/2}else{mid.val}
  if(leg.type=="continuous"){
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    rasterImage(legend_image,x.min,bot.val,x.max,top.val)
    txt.y=if(is.null(txt.y)==T){c(bot.val,top.val)}else(txt.y)
    leg.txt=if(is.null(leg.txt)==T){format(c(min(b),max(b)))}else(leg.txt)
    text(x=x.max, y = txt.y, labels =leg.txt,cex=txt.cex,adj=txt.adj,pos=txt.pos,offset=txt.offset, ...)
  }
  if(leg.type=="categorical"){
    bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
    rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal),lty=0)
    leg.txt=if(is.null(leg.txt)==T){labs}else(leg.txt)
    text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, 
         labels = rev(leg.txt),cex=txt.cex,xpd=NA,pos=txt.pos,adj=txt.adj)
  }
  
  title.x=if(is.null(title.x)==T){mid.val}else{title.x}
  title.y=if(is.null(title.y)==T){top.val}else{title.y}
  text(x=title.x,y=title.y,leg.title,adj=title.adj,cex=title.cex,pos=title.pos,xpd=NA)
}
# GIS Data ----------------------------------------------------------------
canals=spTransform(readOGR(paste0(GIS.path.gen,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)

lakeO=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD"),"LakeOkeechobee_general"),wkt(utm17))
lakeO.lit=spTransform(readOGR(paste0(GIS.path.gen,"/LakeOkeechobee/Littoral"),"LAKEOKEELITTORALZONE_Dissolv"),wkt(utm17))
lakeO.vegall=spTransform(readOGR(paste0(GIS.path.gen,"/LakeOkeechobee/Littoral"),"LAKEO_LITTORALZONE_VEG2007"),wkt(utm17))
lakeO.pelagic=spTransform(readOGR(paste0(GIS.path.gen,"/LakeOkeechobee/Littoral"),"LAKEoMINUSLITTORAL"),wkt(utm17))

wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Mointoring_20230302"),"DBHYDRO_SITE_STATION"),wkt(utm17))

wq.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")

tmp=subset(wq.mon,STATION%in%c("KISSR0.0","LZ2","NES135","NES191","S308C",'CLV10A'))

plot(lakeO)
plot(tmp,add=T)

# writeOGR(lakeO,paste0(wd,"/currentWQStatus/GIS"),"LakeO",driver="ESRI Shapefile")
# writeOGR(wq.mon,paste0(wd,"/currentWQStatus/GIS"),"WQmonitoring",driver="ESRI Shapefile",overwrite_layer = T)

bbox(lakeO)
AOI=raster::extent(gBuffer(lakeO,width=5000))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17

bbox.lims=bbox(AOI.poly)
plot(lakeO,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(AOI.poly,add=T)
plot(crop(canals,AOI.poly),add=T)
# writeOGR(crop(canals,AOI.poly),paste0(wd,"/currentWQStatus/GIS"),"crop_canals",driver="ESRI Shapefile",overwrite_layer = T)

# sites=c(paste0("L00",1:8),"LZ40","LZ30","KISSR0.0","TREEOUT","POLESOUT",
#         "FEBOUT","FEBIN","PALMOUT","POLE3S","MBOXSOU","MH32000","MH24000",
#         "MH16000","TIN13700","TIN16100","RITTAE2","LZ25A","LZ25","PELBAY3",
#         "PELMID","LZ2")
sites=c(paste0("L00",1:8),"LZ40","LZ30","KISSR0.0",
        "FEBOUT","FEBIN","PALMOUT","POLE3S","MBOXSOU","MH32000","MH24000",
        "MH16000","OISLAND","TIN13700","TIN16100","RITTAE2","LZ25A","LZ25","PELBAY3",
        "PELMID","LZ2",
        paste0("TREEN",c("IN","MID","OUT")),
        paste0("TREE",c("IN","MID","OUT")),
        paste0("PLN4",c("IN","MID","OUT")),
        paste0("PLN3",c("IN","MID","OUT")),
        paste0("PLN2",c("IN","MID","OUT")),
        paste0("PLN1",c("IN","MID","OUT")),
        "RITAEAST","RITAW3","RITAWEST","LZ25",
        "LZ15","LZ42N",
        paste0("CPT",c("IN","MID","OUT")),
        paste0("KBAR",c("IN","MID","OUT")),
        paste0("3RDPT",c("IN","MID","OUT")),
        paste0("TIN",c("IN","MID","OUT")),
        paste0("IP",c("IN","MID","OUT")),
        paste0("STAKE",c("IN","MID","OUT")),
        paste0("POLES",c("IN","MID","OUT")))

plot(lakeO)
plot(subset(wq.mon,SITE%in%sites),add=T)
text(subset(wq.mon,SITE=="L004"),"SITE")
text(subset(wq.mon,SITE=="LZ40"),"SITE")
text(subset(wq.mon,SITE=="TREEOUT"),"SITE")
text(subset(wq.mon,SITE=="LZ2"),"SITE")


plot(lakeO)
plot(subset(wq.mon,SITE%in%sites),add=T)
plot(lakeO.lit,add=T)
plot(gBuffer(lakeO.vegall,width=1000),add=T)

lakeNS.buf=crop(gBuffer(lakeO.vegall,width=500),lakeO)
tmp.df=data.frame(ID=1)
rownames(tmp.df)="1"
lakeNS.buf=SpatialPolygonsDataFrame(lakeNS.buf,
                                       tmp.df)

tmp=raster::crop(gBuffer(gBuffer(lakeO.lit,width=200),width=-200),lakeO)

# lakeLit.clean=gBuffer(crop(lakeO,gBuffer(lakeO.lit,width=200)),width=-200)
lakeLit.clean=gBuffer(gBuffer(lakeO.lit,width=200),width=-200)
lakeLit.clean=gIntersection(lakeO,lakeLit.clean)
tmp.df=data.frame(ID=1)
rownames(tmp.df)="1"
lakeLit.clean=SpatialPolygonsDataFrame(lakeLit.clean,tmp.df)

# lakeNS=gDifference(lakeLit.buf,lakeLit.clean)

plot(lakeNS.buf)
plot(lakeLit.clean,add=T,col="red")
plot(subset(wq.mon,SITE%in%sites),add=T)
plot(lakeO,add=T)
# writeOGR(lakeLit.clean,paste0(wd,"/currentWQStatus/GIS"),"LakeOLittoral",driver="ESRI Shapefile")

tmp=data.frame(sf::st_intersection(sf::st_as_sf(subset(wq.mon,SITE%in%sites)),
                                   sf::st_as_sf(gBuffer(lakeLit.clean,width=-100))))["SITE"]
# tmp=spatialEco::point.in.poly(subset(wq.mon,SITE%in%sites),gBuffer(lakeLit.clean,width=-100))@data[,c("SITE","poly.ids")]
lit.sites=tmp# subset(tmp,poly.ids==1)
lit.sites$zone="littoral"
plot(subset(wq.mon,SITE%in%lit.sites$SITE),add=T,pch=21,bg="green")

tmp=data.frame(sf::st_intersection(sf::st_as_sf(subset(wq.mon,SITE%in%sites)),
                                   sf::st_as_sf(lakeNS.buf)))[c("SITE")]
# tmp=spatialEco::point.in.poly(subset(wq.mon,SITE%in%sites),lakeNS.buf)@data[,c("SITE","poly.ids")]
NS.sites=tmp# subset(tmp,poly.ids==1)
NS.sites$zone="nearshore"
NS.sites=subset(NS.sites,!(SITE%in%lit.sites$SITE))
plot(subset(wq.mon,SITE%in%NS.sites$SITE),add=T,pch=21,bg="yellow",cex=1.25)

pelg.sites=data.frame(SITE=sites[!sites%in%c(lit.sites$SITE,NS.sites$SITE)],
                      zone="pelagic")
plot(subset(wq.mon,SITE%in%pelg.sites$SITE),add=T,pch=21,bg="blue")
site.zones=rbind(lit.sites[,c("SITE","zone")],
                 NS.sites[,c("SITE","zone")],
                 pelg.sites)
site.zones$zone=with(site.zones,ifelse(SITE%in%c("FEBIN","MBOXSOU","MH16000","MH24000","MH32000","STAKEIN"),"littoral",zone))
site.zones$zone=with(site.zones,ifelse(SITE%in%c("LZ2","CPTIN","CPTMID","CPTOUT","LZ42N","IPOUT","LZ42N"),NA,zone))
site.zones=subset(site.zones,is.na(zone)==F)


site.zones=data.frame(
  SITE=c("STAKEMID", "STAKEIN", "TREEMID", "OISLAND", "3RDPTIN", "MH16000", 
         "TREEIN", "PLN3IN", "PLN3MID", "TIN13700", "STAKEOUT", "POLESMID", 
         "3RDPTOUT", "FEBIN", "PLN4MID", "TININ", "PLN1IN", "POLE3S", 
         "PELBAY3", "TINMID", "TIN16100", "MBOXSOU", "PLN2MID", "POLESOUT", 
         "PLN4OUT", "PLN2IN", "POLESIN", "KBARIN", "PLN3OUT", "IPIN", 
         "TREENOUT", "3RDPTMID", "PLN4IN", "PLN1OUT", "KBAROUT", "LZ25", 
         "TREENMID", "TREENIN", "RITAWEST", "MH32000", "LZ25A", "FEBOUT", 
         "RITAEAST", "IPMID", "PLN2OUT", "MH24000", "PLN1MID", "TREEOUT", 
         "RITTAE2", "PALMOUT", "TINOUT", "KBARMID", "KISSR0.0", "PELMID", 
         "L001", "L002", "L003", "L004", "L005", "L006", "L007", "L008", 
         "LZ40", "LZ30", "RITAW3", "LZ15"),
  zone=c("littoral", "littoral", "littoral", "littoral", "littoral", 
         "littoral", "littoral", "littoral", "littoral", "littoral", "nearshore", 
         "nearshore", "nearshore", "littoral", "nearshore", "nearshore", 
         "nearshore", "nearshore", "nearshore", "nearshore", "nearshore", 
         "littoral", "nearshore", "nearshore", "nearshore", "nearshore", 
         "nearshore", "nearshore", "nearshore", "nearshore", "nearshore", 
         "nearshore", "nearshore", "nearshore", "nearshore", "nearshore", 
         "nearshore", "nearshore", "nearshore", "littoral", "nearshore", 
         "nearshore", "nearshore", "nearshore", "nearshore", "littoral", 
         "nearshore", "nearshore", "nearshore", "nearshore", "nearshore", 
         "nearshore", "nearshore", "nearshore", "pelagic", "pelagic", 
         "pelagic", "pelagic", "pelagic", "pelagic", "pelagic", "pelagic", 
         "pelagic", "pelagic", "pelagic", "pelagic")
)






plot(lakeO)
plot(subset(wq.mon,SITE%in%sites),add=T)
plot(subset(wq.mon,SITE%in%subset(site.zones,zone=="littoral")$SITE),add=T,pch=21,bg="green")
plot(subset(wq.mon,SITE%in%subset(site.zones,zone=="nearshore")$SITE),add=T,pch=21,bg="yellow")
plot(subset(wq.mon,SITE%in%subset(site.zones,zone=="pelagic")$SITE),add=T,pch=21,bg="blue")

# -------------------------------------------------------------------------
dates=date.fun(c("2000-05-01",as.character(Sys.Date())))

params=data.frame(Test.Number=c(21,20,18,80,61,179,25,23,16,12),
                  param=c("TKN","NH4","NOx","TN","Chla","Chla","TP","SRP","TSS","Turb"))

dat=data.frame()
for(i in 1:length(sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],sites[i],params$Test.Number)
  dat=rbind(tmp,dat)
  print(i)
}
dat=merge(dat,params,"Test.Number")
dat.xtab=data.frame(cast(dat,Station.ID+Date.EST~param,value="HalfMDL",mean))
dat.xtab$TN=with(dat.xtab,TN_Combine(NOx,TKN,TN))
dat.xtab$DIN=with(dat.xtab,NH4+NOx)
dat.xtab$TP=dat.xtab$TP*1000;# convert to ug/L
dat.xtab$SRP=dat.xtab$SRP*1000;# convert to ug/L

dat.xtab$WY=WY(dat.xtab$Date.EST)
dat.xtab$season=FL.Hydroseason(dat.xtab$Date.EST)

dat.xtab$month=as.numeric(format(dat.xtab$Date.EST,"%m"))
dat.xtab$CY=as.numeric(format(dat.xtab$Date.EST,"%Y"))
dat.xtab$day=as.numeric(format(dat.xtab$Date.EST,"%d"))

sites.shp=cbind(data.frame(Station.ID=subset(wq.mon,STATION%in%sites)@data$STATION),
                coordinates(subset(wq.mon,STATION%in%sites)))
colnames(sites.shp)=c("Station.ID","UTMX","UTMY")
sites.shp=SpatialPointsDataFrame(sites.shp[,c("UTMX","UTMY")],
                                 sites.shp,
                                 proj4string = utm17)

dat.xtab=merge(dat.xtab,sites.shp@data,"Station.ID",all.x=T)
dat.xtab=merge(dat.xtab,site.zones,by.x="Station.ID",by.y="SITE")

# tail(subset(dat.xtab,CY==2023))
dat.xtab.month.region=ddply(dat.xtab,c("CY","month","zone"),summarise,
                            mean.TP=mean(TP,na.rm=T),sd.TP=sd(TP,na.rm=T),N.TP=N.obs(TP),SE.TP=SE(TP),
                            mean.SRP=mean(SRP,na.rm=T),sd.SRP=sd(SRP,na.rm=T),N.SRP=N.obs(SRP),SE.SRP=SE(SRP),
                            mean.TN=mean(TN,na.rm=T),sd.TN=sd(TN,na.rm=T),N.TN=N.obs(TN),SE.TN=SE(TN),
                            mean.DIN=mean(DIN,na.rm=T),sd.DIN=sd(DIN,na.rm=T),N.DIN=N.obs(DIN),SE.DIN=SE(DIN),
                            mean.Chla=mean(Chla,na.rm=T),sd.Chla=sd(Chla,na.rm=T),N.DIN=N.obs(Chla),SE.Chla=SE(Chla))
zone.vals=c("littoral","nearshore","pelagic")
fill.val=expand.grid(CY=seq(2000,as.numeric(format(dates[2],"%Y")),1),
            month=1:12,
            zone=zone.vals)
dat.xtab.month.region=merge(dat.xtab.month.region,fill.val,c("CY","month","zone"),all.y=T)
dat.xtab.month.region$date.monCY=with(dat.xtab.month.region,date.fun(paste(CY,month,01,sep="-")))

bks.TP=seq(0,600,100)
cols.TP=viridis::plasma(length(bks.TP)-1,alpha = 0.75)
bks.TN=seq(0,5,1)
cols.TN=viridis::magma(length(bks.TN)-1,alpha = 0.75)

t1=max(dat.xtab$Date.EST)-lubridate::duration(1,"month")
t1.dat=subset(dat.xtab,month==as.numeric(format(t1,"%m"))&CY==as.numeric(format(t1,"%Y")))
t1.dat.shp=merge(sites.shp,t1.dat,"Station.ID",all.x=T)
t1.dat.shp$TP.cols=col.vals=findInterval(t1.dat.shp$TP,bks.TP)
t1.dat.shp.TP=subset(t1.dat.shp,is.na(TP.cols)==F)
t1.dat.shp$TN.cols=col.vals=findInterval(t1.dat.shp$TN,bks.TN)
t1.dat.shp.TN=subset(t1.dat.shp,is.na(TN.cols)==F)
t1.dat.shp.TN$TN=round(t1.dat.shp.TN$TN,2)

t2=max(dat.xtab$Date.EST)
t2.dat=subset(dat.xtab,month==as.numeric(format(t2,"%m"))&CY==as.numeric(format(t2,"%Y")))
t2.dat.shp=merge(sites.shp,t2.dat,"Station.ID")
t2.dat.shp$TP.cols=col.vals=findInterval(t2.dat.shp$TP,bks.TP)
t2.dat.shp.TP=subset(t2.dat.shp,is.na(TP.cols)==F)
t2.dat.shp$TN.cols=col.vals=findInterval(t2.dat.shp$TN,bks.TN)
t2.dat.shp.TN=subset(t2.dat.shp,is.na(TN.cols)==F)
t2.dat.shp.TN$TN=round(t2.dat.shp.TN$TN,2)


sites.shp2=merge(sites.shp,site.zones,by.x="Station.ID",by.y="SITE")
cols=wesanderson::wes_palette("Zissou1",3,"continuous")
zone.vals.labs=c("Littoral","Nearshore","Pelagic")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.4))

plot(lakeO,border="grey",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(subset(sites.shp2,zone==zone.vals[3]),pch=21,bg=cols[3],add=T,cex=1.5,lwd=0.01)
plot(subset(sites.shp2,zone==zone.vals[2]),pch=21,bg=cols[2],add=T,cex=1.5,lwd=0.01)
plot(subset(sites.shp2,zone==zone.vals[1]),pch=21,bg=cols[1],add=T,cex=1.5,lwd=0.01)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=zone.vals.labs,
       lty=c(0),col=c("black"),
       pch=c(21),pt.bg=cols,
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)

# png(filename=paste0(plot.path,"/LOK_TP.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:3,1,3,byrow=T),widths=c(1,0.4,1))
bbox.lims=bbox(lakeO)

plot(lakeO,border="grey",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(t1.dat.shp.TP,pch=21,bg=cols[t1.dat.shp.TP$TP.cols],add=T,cex=1.5,lwd=0.01)
text(t1.dat.shp.TP,"TP",pos=4,offset=0.5)
mtext(side=3,adj=0,line=-1.25,
      paste(min(format(t1.dat$Date.EST,"%b %d")),max(format(t1.dat$Date.EST,"%b %d, %Y")),sep=" - "))

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(bks.TP,cols,"Total Phosphorus (\u03BCg L\u207B\u00B9)",
        leg.type = "categorical")

plot(lakeO,border="grey",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(t2.dat.shp.TP,pch=21,bg=cols[t1.dat.shp.TP$TP.cols],add=T,cex=1.5,lwd=0.01)
text(t2.dat.shp.TP,"TP",pos=4,offset=0.5)
mtext(side=3,adj=0,line=-1.25,
      paste(min(format(t2.dat$Date.EST,"%b %d")),max(format(t2.dat$Date.EST,"%b %d, %Y")),sep=" - "))
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");


par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:3,1,3,byrow=T),widths=c(1,0.4,1))

bbox.lims=bbox(lakeO)

plot(lakeO,border="grey",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(t1.dat.shp.TN,pch=21,bg=cols[t1.dat.shp.TN$TN.cols],add=T,cex=1.5,lwd=0.01)
text(t1.dat.shp.TN,format(t1.dat.shp.TN$TN,nsmall=2),pos=4,offset=0.5)
mtext(side=3,adj=0,line=-1.25,
      paste(min(format(t1.dat$Date.EST,"%b %d")),max(format(t1.dat$Date.EST,"%b %d, %Y")),sep=" - "))

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(bks.TN,cols,"Total Nitorgen (mg L\u207B\u00B9)",
        leg.type = "categorical")

plot(lakeO,border="grey",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=1)
plot(t2.dat.shp.TN,pch=21,bg=cols[t1.dat.shp.TN$TN.cols],add=T,cex=1.5,lwd=0.01)
text(t2.dat.shp.TN,format(t2.dat.shp.TN$TN,nsmall=2),pos=4,offset=0.5)
mtext(side=3,adj=0,line=-1.25,
      paste(min(format(t2.dat$Date.EST,"%b %d")),max(format(t2.dat$Date.EST,"%b %d, %Y")),sep=" - "))
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");






## Time Series Plot
edate=date.fun(paste(format(Sys.Date(),"%Y"),format(Sys.Date(),"%m"),"01",sep="-"))
sdate=date.fun(edate-lubridate::duration(18,"months"))
sdate=date.fun(paste(format(sdate,"%Y"),format(sdate,"%m"),"01",sep="-"))
cols=wesanderson::wes_palette("Zissou1",3,"continuous")
zone.vals.labs=c("Littoral","Nearshore","Pelagic")

layout(matrix(1:15,5,3,byrow=T))
par(family="serif",mar=c(1,1,0.25,0.5),oma=c(2,3,1,0.1));
xlim.val=c(sdate,edate);xmaj=seq(xlim.val[1],xlim.val[2],"6 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")

ylim.val=c(0,600);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(mean.TP~date.monCY,dat.xtab.month.region,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")  
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line_error(date.monCY,mean.TP,SE.TP,2,cols[i],1,21,cols[i],length=0.02,pt.lwd=0.01))
  #with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line(date.monCY,mean.TP,2,cols[i],1,21,cols[i]))
  # axis_fun(1,xmaj,xmin,NA)
  # axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"),line=-0.5)
  if(i==1){
    axis_fun(2,ymaj,ymin,ymaj);
    mtext(side=2,line=2.5,"TP (\u03BCg L\u207B\u00B9)")
    }else{
      axis_fun(2,ymaj,ymin,NA)
      }
  box(lwd=1)
  mtext(side=3,zone.vals.labs[i])
}
# ylim.val=c(0,600);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(mean.TP~date.monCY,dat.xtab.month.region,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")  
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line_error(date.monCY,mean.SRP,SE.SRP,2,cols[i],1,21,cols[i],length=0.02,pt.lwd=0.01))
  #with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line(date.monCY,mean.TP,2,cols[i],1,21,cols[i]))
  axis_fun(1,xmaj,xmin,NA)
  # axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"),line=-0.5)
  if(i==1){
    axis_fun(2,ymaj,ymin,ymaj);
    mtext(side=2,line=2.5,"SRP (\u03BCg L\u207B\u00B9)")
  }else{
    axis_fun(2,ymaj,ymin,NA)
  }
  box(lwd=1)
}

ylim.val=c(0,5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(mean.TP~date.monCY,dat.xtab.month.region,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")  
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line_error(date.monCY,mean.TN,SE.TN,2,cols[i],1,21,cols[i],length=0.02,pt.lwd=0.01))
  #with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line(date.monCY,mean.TP,2,cols[i],1,21,cols[i]))
  axis_fun(1,xmaj,xmin,NA)
  # axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"),line=-0.5)
  if(i==1){
    axis_fun(2,ymaj,ymin,ymaj);
    mtext(side=2,line=2.5,"TN (mg L\u207B\u00B9)")
  }else{
    axis_fun(2,ymaj,ymin,NA)
  }
  box(lwd=1)
}
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(mean.TP~date.monCY,dat.xtab.month.region,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")  
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line_error(date.monCY,mean.DIN,SE.DIN,2,cols[i],1,21,cols[i],length=0.02,pt.lwd=0.01))
  #with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line(date.monCY,mean.TP,2,cols[i],1,21,cols[i]))
  axis_fun(1,xmaj,xmin,NA)
  # axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"),line=-0.5)
  if(i==1){
    axis_fun(2,ymaj,ymin,format(ymaj));
    mtext(side=2,line=2.5,"DIN (mg L\u207B\u00B9)")
  }else{
    axis_fun(2,ymaj,ymin,NA)
  }
  box(lwd=1)
}
ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(mean.TP~date.monCY,dat.xtab.month.region,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")  
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line_error(date.monCY,mean.Chla,SE.Chla,2,cols[i],1,21,cols[i],length=0.02,pt.lwd=0.01))
  #with(subset(dat.xtab.month.region,zone==zone.vals[i]),pt_line(date.monCY,mean.TP,2,cols[i],1,21,cols[i]))
  # axis_fun(1,xmaj,xmin,NA)
  axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"),line=-0.5)
  if(i==1){
    axis_fun(2,ymaj,ymin,format(ymaj));
    mtext(side=2,line=2.5,"Chl-a (\u03BCg L\u207B\u00B9)")
  }else{
    axis_fun(2,ymaj,ymin,NA)
  }
  box(lwd=1)
  
}
mtext(side=1,outer=T,line=0.75,"Date (Month-Year)")
