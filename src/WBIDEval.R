## 
## Lake Okeechobee Water Quality
## WBID 3212H Evaluation
## Miami Canal source water
## 
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)
library(zoo)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)


library(flextable)
library(magrittr)


## Paths
wd="C:/Julian_LaCie/_Github/LakeO_WQ"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

GIS.path="C:/Julian_LaCie/_GISData"

nad83.pro=CRS(SRS_string="EPSG:4269")
utm17=CRS(SRS_string="EPSG:26917")

tmap_mode("view")
# GIS ---------------------------------------------------------------------
wmd.mon=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20210119"),"DBHYDRO_SITE_STATION"),utm17)
wq.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")
wbid=spTransform(readOGR(paste0(GIS.path,"/FDEP"),"WBIDs"),utm17)

wbid.lakeO=subset(wbid,WBID%in%paste0("3212",LETTERS[1:9]))
plot(wbid.lakeO)


# -------------------------------------------------------------------------
dates=date.fun(c("2000-05-01","2020-05-01"))

params=data.frame(Test.Number=c(18,21,80,20,25,61,179,13,67),
                  param=c("NOx","TKN","TN","NH4","TP","Chla","Chla","color","alk"))

wq.sites=data.frame(Station.ID=c("KISSR0.0","LZ2","L001","FEBIN","MBOXSOU","MH16000",
                        "MH24000","MH32000","OISLAND","TIN13700","TIN16100",
                        "POLESOUT","FEBOUT","L008","L005","L004","PALMOUT",
                        "LZ30","L006","LZ40","CLV10A","L007","POLE3S","RITTAE2",
                        "PELBAY3","PELMID","LZ25A","LZ25"),
           WBID=c(rep("3212A",2),"3212B",rep("3212C",6),rep("3212D",6),"3212E",
                  rep("3212F",2),rep("3212G",3),rep("3212H",3),rep("3212I",4)))
# plot(subset(wq.mon,STATION%in%wq.sites$Station.ID),add=T)
# wq.sites=subset(wq.sites,WBID=="3212H")

wq.dat=data.frame()
for(i in 1:nrow(wq.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],wq.sites$Station.ID[i],params$Test.Number)
  wq.dat=rbind(wq.dat,tmp)
  print(i)
}
wq.dat=merge(wq.dat,params,"Test.Number")
wq.dat=merge(wq.dat,wq.sites,"Station.ID")

# wq.dat=wq.dat[order(wq.dat$Date.EST,wq.dat$Station.ID),]
wq.dat$WY=WY(wq.dat$Date.EST)
wq.dat$season=FL.Hydroseason(wq.dat$Date.EST)
wq.dat.xtab=cast(wq.dat,Station.ID+WBID+Date.EST+WY+season~param,value="HalfMDL",mean)
wq.dat.xtab$TN=with(wq.dat.xtab,TN_Combine(NOx,TKN,TN))

## 
col.samp.size=cast(wq.dat.xtab,Station.ID+WY~season,value="color",fun.aggregate = function(x)N.obs(x))
col.samp.size$TSamp=rowSums(col.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
col.samp.size$sea.screen=with(col.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
col.samp.size$param="color"

alk.samp.size=cast(wq.dat.xtab,Station.ID+WY~season,value="alk",fun.aggregate = function(x)N.obs(x))
alk.samp.size$TSamp=rowSums(alk.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
alk.samp.size$sea.screen=with(alk.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
alk.samp.size$param="alk"

Chla.samp.size=cast(wq.dat.xtab,Station.ID+WY~season,value="Chla",fun.aggregate = function(x)N.obs(x))
Chla.samp.size$TSamp=rowSums(Chla.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
Chla.samp.size$sea.screen=with(Chla.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
Chla.samp.size$param="Chla"

TP.samp.size=cast(wq.dat.xtab,Station.ID+WY~season,value="TP",fun.aggregate = function(x)N.obs(x))
TP.samp.size$TSamp=rowSums(TP.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
TP.samp.size$sea.screen=with(TP.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
TP.samp.size$param="TP"

TN.samp.size=cast(wq.dat.xtab,Station.ID+WY~season,value="TN",fun.aggregate = function(x)N.obs(x))
TN.samp.size$TSamp=rowSums(TN.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
TN.samp.size$sea.screen=with(TN.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
TN.samp.size$param="TN"

samp.size.scrn=rbind(col.samp.size,alk.samp.size,Chla.samp.size,TP.samp.size,TN.samp.size)
samp.size.scrn[,c("Station.ID","WY","param","sea.screen")]

vars=c("Station.ID","WBID", "Date.EST", "WY", "season", "alk", "Chla", "color","TN", "TP")
wq.dat.xtab.melt=melt(data.frame(wq.dat.xtab[,vars]),id.vars=vars[1:5],variable_name="param")
wq.dat.xtab.melt=merge(wq.dat.xtab.melt,samp.size.scrn[,c("Station.ID","WY","param","sea.screen")],c("Station.ID","WY","param"))

wq.dat.GM=ddply(subset(wq.dat.xtab.melt,sea.screen==1),c("Station.ID","WBID","WY","param"),summarise,GM=exp(mean(log(value),na.rm=T)))
wq.dat.GM=merge(wq.dat.GM,expand.grid(Station.ID=wq.sites$Station.ID,
                                      WY=seq(2001,2020,1),
                                      param=c("alk", "Chla", "color","TN", "TP")),all.y=T)
# write.csv(wq.dat.GM,paste0(export.path,"GM_data.csv"),row.names = F)
# wq.dat.GM.avg=ddply(wq.dat.GM,c("WY","param"),summarise,avg.GM=mean(GM,na.rm=T))
wq.dat.GM.avg=cast(wq.dat.GM,WBID+WY~param,value="GM",mean,na.rm=T)


wq.dat.GM.avg$alk_3WYmean=with(wq.dat.GM.avg,ave(alk,WBID,FUN=function(x)c(rep(NA,2),rollmean(x,k=3))))
wq.dat.GM.avg$col_3WYmean=with(wq.dat.GM.avg,ave(color,WBID,FUN=function(x)c(rep(NA,2),rollmean(x,k=3))))

wq.dat.GM.avg$Chla.ann.exceed=with(wq.dat.GM.avg,ifelse(Chla>ifelse(alk>=20,20,6),1,0))
wq.dat.GM.avg$Chla_1_3=with(wq.dat.GM.avg,ave(Chla.ann.exceed,WBID,FUN=function(x) ifelse(c(rep(NA,2),rollsum(x,k=3))>=3,1,0)))

wq.dat.GM.avg$TP.lim=with(wq.dat.GM.avg,ifelse(color>=40&Chla.ann.exceed==1,0.05,
                                               ifelse(color>=40&Chla.ann.exceed==0,0.16,
                                               ifelse(color<=40&alk>=20&Chla.ann.exceed==1,0.03,
                                                      ifelse(color<=40&alk>=20&Chla.ann.exceed==0,0.09,
                                                             ifelse(color<=40&alk<=20&Chla.ann.exceed==1,0.01,
                                                                    ifelse(color<=40&alk<=20&Chla.ann.exceed==0,0.03,NA)))))))
wq.dat.GM.avg$TN.lim=with(wq.dat.GM.avg,ifelse(color>=40&Chla.ann.exceed==1,1.27,
                                               ifelse(color>=40&Chla.ann.exceed==0,2.23,
                                                      ifelse(color<=40&alk>=20&Chla.ann.exceed==1,1.05,
                                                             ifelse(color<=40&alk>=20&Chla.ann.exceed==0,1.91,
                                                                    ifelse(color<=40&alk<=20&Chla.ann.exceed==1,0.51,
                                                                           ifelse(color<=40&alk<=20&Chla.ann.exceed==0,0.93,NA)))))))
wq.dat.GM.avg$TP.ann.exceed=with(wq.dat.GM.avg,ifelse(TP>TP.lim,1,0))
wq.dat.GM.avg$TP_1_3=with(wq.dat.GM.avg,ave(TP.ann.exceed,WBID,FUN=function(x) ifelse(c(rep(NA,2),rollsum(x,k=3))>=3,1,0)))
wq.dat.GM.avg$TN.ann.exceed=with(wq.dat.GM.avg,ifelse(TN>TN.lim,1,0))
wq.dat.GM.avg$TN_1_3=with(wq.dat.GM.avg,ave(TN.ann.exceed,WBID,FUN=function(x) ifelse(c(rep(NA,2),rollsum(x,k=3))>=3,1,0)))

subset(wq.dat.GM.avg,WBID=="3212H")
subset(wq.dat.GM.avg,Chla_1_3>0)

tmp=subset(wq.dat.GM.avg,WBID=="3212I")

vars=c("WBID","WY","alk","color","Chla","TP","TN","TP.lim","TN.lim","Chla.ann.exceed","TP.ann.exceed","TN.ann.exceed")
tmp[,vars]%>%
  flextable(col_keys=vars[1:7])%>%
  colformat_num(j=3:5,digits=0)%>%
  colformat_num(j=6:7,digits=2)%>%
  bg(j=5,i=~Chla.ann.exceed==1,bg=adjustcolor("red",0.25))%>%
  bg(j=6,i=~TP.ann.exceed==1,bg=adjustcolor("red",0.25))%>%
  bg(j=7,i=~TN.ann.exceed==1,bg=adjustcolor("red",0.25))%>%
  align(j=3:7,align="center",part="all")%>%
  set_header_labels("alk"="Alkalinity\n(mg L\u207B\u00B9)",
                    "color"="Color\n(PCU)",
                    "Chla"="Chlorophyll-a\n(\u03BCg L\u207B\u00B9)",
                    "TP"="Total Phosphorus\n(mg L\u207B\u00B9)",
                    "TN"="Total Nitrogen\n(mg L\u207B\u00B9)")%>%
  width(width=c(0.5,0.5,0.75,0.75,1.1,1.4,1.1))%>%
  footnote(j=1,i=1,part="header",
           value=as_paragraph(paste("Includes Stations:",
                                    paste(subset(wq.sites,WBID=="3212H")$Station.ID,collapse=", "))))
           
# png(filename=paste0(plot.path,"WBID3212H_WQ_WY.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
# tiff(filename=paste0(plot.path,"WBID3212H_WQ_WY.tiff"),width=5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,3,0.5,1),oma=c(2.5,3,0.75,0.25));
# layout(matrix(c(1:5,rep(6,5)),5,2,byrow=F),widths=c(1,0.25))
layout(matrix(1:5,5,1,byrow=F))

cols=adjustcolor(c("indianred1","dodgerblue1","forestgreen"),0.5)
xlim.val=c(2001,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(GM~WY,wq.dat.GM,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:3){
  with(subset(wq.dat.GM,Station.ID==as.character(wq.sites$Station.ID[i])&param=="alk"),
       pt_line(WY,GM,2,cols[i],1,21,cols[i],pt.lwd=0.1,cex=1.25,pt.col=adjustcolor("black",0.5)))
}
abline(h=20,col="Red")
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Alkalinity\n(mg L\u207B\u00B9)")
mtext(side=3,adj=0,"WBID: 3212H")
legend("topleft",legend=wq.sites$Station.ID,
       pch=c(21),lty=0,lwd=0.1,
       col=adjustcolor("black",0.5),pt.bg=cols,
       pt.cex=1.5,ncol=3,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(GM~WY,wq.dat.GM,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:3){
  with(subset(wq.dat.GM,Station.ID==as.character(wq.sites$Station.ID[i])&param=="color"),
       pt_line(WY,GM,2,cols[i],1,21,cols[i],pt.lwd=0.1,cex=1.25,pt.col=adjustcolor("black",0.5)))
}
abline(h=40,col="Red")
#axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Color\n(PCU)")

ylim.val=c(0,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(GM~WY,wq.dat.GM,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:3){
  with(subset(wq.dat.GM,Station.ID==as.character(wq.sites$Station.ID[i])&param=="Chla"),
       pt_line(WY,GM,2,cols[i],1,21,cols[i],pt.lwd=0.1,cex=1.25,pt.col=adjustcolor("black",0.5)))
}
#axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Chl-a\n(\u03BCg L\u207B\u00B9)")

ylim.val=c(0,0.250);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(GM~WY,wq.dat.GM,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:3){
  with(subset(wq.dat.GM,Station.ID==as.character(wq.sites$Station.ID[i])&param=="TP"),
       pt_line(WY,GM,2,cols[i],1,21,cols[i],pt.lwd=0.1,cex=1.25,pt.col=adjustcolor("black",0.5)))
}
#axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj*1000);box(lwd=1)
mtext(side=2,line=2.5,"TP\n(\u03BCg L\u207B\u00B9)")

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(GM~WY,wq.dat.GM,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(i in 1:3){
  with(subset(wq.dat.GM,Station.ID==as.character(wq.sites$Station.ID[i])&param=="TN"),
       pt_line(WY,GM,2,cols[i],1,21,cols[i],pt.lwd=0.1,cex=1.25,pt.col=adjustcolor("black",0.5)))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TN\n(mg L\u207B\u00B9)")
mtext(side=1,line=2,"Water Year")
#
# plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA);
# legend(0.25,0.5,legend=wq.sites$Station.ID,
#        pch=c(21),lty=0,lwd=0.1,
#        col=adjustcolor("black",0.5),pt.bg=cols,
#        pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Water Quality Sites",title.adj = 0)
dev.off()