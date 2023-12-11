## 
## Lake Okeechobee Mass Balance
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org


## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

library(rvest)

## Paths
wd="C:/Julian_LaCie/_Github/LakeO_WQ"

paths=paste0(wd,c("/Plots/","/export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"

# -------------------------------------------------------------------------
WYs=seq(1979,2021,1)
# dates=date.fun(c("1978-05-01","2020-05-01"))
dates=date.fun(c("1973-05-01","2023-05-01"))


# Discharge ---------------------------------------------------------------
flow.dbkeys.E=data.frame(
  STRUCT=c("S308","S308"),
  ALIAS=c("S308","S308"),
  STATION=c("S308.DS","S308_S"),
  DBKEY=c("15626","DJ239"),
  Priority=c("P1","P2"),
  Basin="E",
  Inflow_direction=-1,
  Outflow=1,
  WQSite=c("S308C")
)

flow.dbkeys.W=data.frame(
  STRUCT=c("CU5A.S282","S77","S77"),
  ALIAS=c("CULV5A_C","S77_T","S77_T"),
  STATION=c("CULV5A_C","S77_T","S77_T"),
  DBKEY=c("90880","15635","DJ235"),
  Priority=c("P1","P1","P2"),
  Basin="W",
  Inflow_direction=-1,
  Outflow=1,
  WQSite=c("CULV5A","S77","S77")
)

flow.dbkeys.S=data.frame(
  STRUCT=c("CU10","CU10A","CU10A","CU12","CU12A","CU4A","INDUST","INDUST",
           "S2.S351","S2.S351","S236","S3.S354","S3.S354","S351_TEMP",
           "S352","S352","S352_TEMP","S354_TEMP","S4","S4","S2","S3"),
  ALIAS=c("C-10","CU10A","CU10A","C-12","C-12A","C-4A","INDUST","INDUST",
          "S2","S2","S236_P","S3","S3","S351_TEMP",
          "S352","S352","S352_TEMP","S354_TEMP","S4_P","S4_P","S2","S3"),
  STATION=c("C-10","L8.441","S271","C-12","C-12A","C-4A","INDUST","INDUST",
            "S2","S2","S236_P","S3","S3","S351_TEMP",
            "HGS5X","HGS5X","S352_TEMP","S354_TEMP","S4_P","S4_P","S2","S3"),
  DBKEY=c("15645","15640","65409","15646","15647","15648","15628","02747",
          "91508","15021","15644","91513","15018","91509",
          "15068","91510","91511","91514","15630","91608","91473","91599"),
  Priority=c("P1","P1","P2","P1","P1","P1","P1","P2",
             "P1","P2","P1","P1","P2","P1",
             "P1","P2","P1","P1","P1","P2","P1","P1"),
  Basin="S",
  Inflow_direction=c(1,-1,-1,1,1,1,-1,-1,
                     -1,-1,1,-1,-1,-1,
                     -1,-1,1,-1,1,1,1,-1),
  Outflow=c(0,1,1,0,0,0,1,1,
            1,1,0,1,1,1,
            1,1,1,1,0,0,1,1),
  WQSite=c("CULV10","CULV10A","CULV10A","CULV12","CULV12A","CULV4A","INDUSCAN","INDUSCAN",
           "S2","S2","S236","S3","S3","S351",
           "S352","S352","S352","S354","S4","S4","S2","S3")
)

flow.dbkeys.N=data.frame(
  STRUCT=c("C41","CU5.S281","FISHCR","G207","G208","G33",
           "G34","G74","G75","G76",
           "S127","S127","S127","S129","S129","S129",
           "S131","S131","S133","S133","S135","S135","S135",
           "S154","S154","S154C","S191","S191",
           "S65E","S65E","S65EX1","S71","S71",
           "S72","S72","S84","S84","S84X"),
  ALIAS=c("C41H78_I","CV5","FISHCR","G207","G208","G33_C",
          "G34_C","G74_C","G75_C","G76_C",
          "S127_C","S127_P","S127_P","S129_C","S129_P","S129_P",
          "S131_C","S131_P","S133_P","S133_P","S135_C","S135_P","S135_P",
          "S154_C","S154_C","S154C_C","S191_S","S191_S",
          "S65E_S","S65E_S","S65EX1_S","S71_S","S71_S",
          "S72_S","S72_S","S84_S","S84_S","S84X_S"),
  STATION=c("C41H78_I","CV5","FISHCR","G207","G208","G33_C",
            "G34_C","G74_C","G75_C","G76_C",
            "S127_C","S127_P","S127_P","S129_C","S129 PMP_P","S129 PMP_P",
            "S131_C","S131 PMP_P","S133_P","S133_P","S135_C","S135 PMP_P","S135 PMP_P",
            "S154_C","S154_C","S154C_C","S191_S","S191_S",
            "S65E_S","S65E_S","S65EX1_S","S71_S","S71_S",
            "S72_S","S72_S","S84_S","S84_S","S84X_S"),
  DBKEY=c(90871,90882,"WH036",90916,90918,91014,
          91071,91267,91268,91269,
          91370,15641,91371,91373,15642,91372,
          91376,15643,15637,91377,91379,15638,91378,
          15629,91401,91399,15639,91429,
          15631,91656,"AL760",15633,91668,
          15634,91675,15636,91687,91686),
  Priority=paste0("P",
                  c(1,1,1,1,1,1,
                    1,1,1,1,
                    1,1,2,1,1,2,
                    1,1,1,2,1,1,2,
                    1,2,1,1,2,
                    1,2,1,1,2,
                    1,2,1,2,1)),
  Basin="N",
  Inflow_direction=c(1,1,1,-1,-1,1,
                     1,1,1,1,
                     1,1,1,1,1,1,
                     1,1,1,1,1,1,1,
                     1,1,1,1,1,
                     1,1,1,1,1,
                     1,1,1,1,1),
  Outflow=c(0,1,0,1,1,0,
            0,0,0,0,
            1,0,0,1,0,0,
            1,0,0,0,1,0,0,
            0,0,0,0,0,
            0,0,0,0,0,
            0,0,0,0,0),
  WQSite=c("C41H78","CULV5","FECSR78","G207","G208","C38W",
           "L59E","L59W","L60E","L60W",
           "S127","S127","S127","S129","S129","S129",
           "S131","S131","S133","S133","S135","S135","S135",
           "S154","S154","S154C","S191","S191",
           "S65E","S65E","S65E","S71","S71",
           "S72","S72","S84","S84","S84")
)


flow.dbkeys=rbind(flow.dbkeys.N,flow.dbkeys.E,flow.dbkeys.S,flow.dbkeys.W)
# Q.meta=DBHYDRO.meta.byDBKEY(flow.dbkeys$DBKEY)

flow.meta=DBHYDRO.meta.byDBKEY(flow.dbkeys$DBKEY)
# head(flow.meta)
flow.meta$START.DATE=date.fun(flow.meta$START.DATE,form="%d-%B-%Y")
flow.meta$END.DATE=date.fun(flow.meta$END.DATE,form="%d-%B-%Y")

# max(flow.meta$END.DATE)
# min(flow.meta$END.DATE)
# subset(flow.meta,END.DATE<date.fun("2023-03-13"))

subset(flow.meta,DBKEY=="WH036")
# DBHYDRO.meta.bysite("FISHCR","FLOW")
# -------------------------------------------------------------------------
# edate=date.fun(as.character(Sys.Date()))
# sdate=date.fun(paste(format(edate-lubridate::duration(4,"years"),"%Y"),"05-01",sep="-"))
# dates=c(sdate,edate)

# subset(flow.meta,END.DATE%in%seq(sdate,edate,"1 day"))
# flow.dbkeys
# flow.dbkeys2=subset(flow.dbkeys,DBKEY%in%subset(flow.meta,END.DATE%in%seq(sdate,edate,"1 day"))$DBKEY)

flow.dbkeys2=flow.dbkeys# subset(flow.dbkeys,DBKEY%in%subset(flow.meta,START.DATE<dates[2]&END.DATE>dates[1])$DBKEY)


## Just to check
merge(ddply(flow.dbkeys,"STRUCT",summarise,N.val=N.obs(STRUCT)),
      ddply(flow.dbkeys2,"STRUCT",summarise,N.val=N.obs(STRUCT)),"STRUCT",all.x=T)

flow.dat=data.frame()
for(i in 1:nrow(flow.dbkeys2)){
  tmp=DBHYDRO_daily(dates[1],dates[2],flow.dbkeys2$DBKEY[i])
  tmp$DBKEY=as.character(flow.dbkeys2$DBKEY[i])
  flow.dat=rbind(tmp,flow.dat)
  print(paste(i,": ",flow.dbkeys2$DBKEY[i]))
}


flow.data=merge(flow.dat,flow.dbkeys[,c("DBKEY","STRUCT","ALIAS","Priority","Basin","Inflow_direction","Outflow","WQSite")],"DBKEY")
flow.data$WY=WY(flow.data$Date)

flow.xtab=data.frame(reshape::cast(flow.data,Date+WY+STRUCT+ALIAS+Inflow_direction+Outflow+Basin+WQSite~Priority,value="Data.Value",fun.aggregate=function(x) ifelse(sum(x,na.rm=T)==0,NA,sum(x,na.rm=T))))
flow.xtab$Date.EST=date.fun(flow.xtab$Date)
flow.xtab$fflow.cfs=with(flow.xtab,ifelse(is.na(P1),P2,P1));#if only two priorities
flow.xtab$fflow.cfs=with(flow.xtab,fflow.cfs*Inflow_direction)#all inflows are positive and all negative values are outflow
flow.xtab$direct=with(flow.xtab,ifelse(fflow.cfs<0,"Outflow","Inflow"))
flow.xtab$month=as.numeric(format(flow.xtab$Date,"%m"))
flow.xtab$CY=as.numeric(format(flow.xtab$Date,"%Y"))

mon.seq=data.frame(Date.EST=date.fun(seq(dates[1],dates[2],"1 months")))
mon.seq$month=as.numeric(format(mon.seq$Date.EST,"%m"))
mon.seq$CY=as.numeric(format(mon.seq$Date.EST,"%Y"))
mon.seq$WY=WY(mon.seq$Date.EST)

# flow.mon.sum=reshape2::dcast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+CY+month~direct,value.var="fflow.cfs",fun.aggregate=function(x) sum(abs(cfs.to.acftd(x)),na.rm=T))
flow.mon.sum=reshape2::dcast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+CY+month~direct,value.var="fflow.cfs",fun.aggregate=function(x) sum(abs(x),na.rm=T))
flow.mon.sum=flow.mon.sum[,c("STRUCT", "ALIAS", "Basin", "WQSite", "WY", "CY", "month","Inflow", "Outflow")]


WY.Tflow=ddply(flow.mon.sum,"WY",summarise,inflow.Q.cfs=sum(Inflow,na.rm=T),outflow.Q.cfs=sum(Outflow,na.rm=T))
WY.Tflow
cfs.to.acftd(WY.Tflow$inflow.Q.cfs)
cfs.to.acftd(WY.Tflow$outflow.Q.cfs)
# Water Quality -----------------------------------------------------------
wq.sites=ddply(flow.dbkeys2,"WQSite",summarise,N.val=N.obs(ALIAS))

wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC"))
wq.param=subset(wq.param,!(Param%in%c("NH4","DOC","TOC","OP")))
wq.dat=data.frame()
for(i in 1:nrow(wq.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],wq.sites$WQSite[i],wq.param$Test.Number)
  wq.dat=rbind(tmp,wq.dat)
  print(paste0(i,": ",wq.sites$WQSite[i]))
}

wq.dat=merge(wq.dat,wq.param,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method=="G") # Grab sample only 
wq.dat$month=as.numeric(format(wq.dat$Date.EST,"%m"))
wq.dat$CY=as.numeric(format(wq.dat$Date.EST,"%Y"))
wq.dat$WY=WY(wq.dat$Date.EST)
N.TP=ddply(subset(wq.dat,Param=="TP"),c("WY","Station.ID"),summarise,N.val=N.obs(HalfMDL),mean.val=mean(HalfMDL,na.rm=T))
N.TP
N.TSS=ddply(subset(wq.dat,Param=="TSS"),c("WY","Station.ID"),summarise,N.val=N.obs(HalfMDL),mean.val=mean(HalfMDL,na.rm=T))
N.TSS

# checking for more than one sample per day
# test=cast(subset(wq.dat,Project.Code!="LAB"),Station.ID+Date.EST~Param,value="HalfMDL",fun.aggregate = function(x)N.obs(x))
# subset(test,TP>2)
# subset(wq.dat,Param=="TP"&Station.ID=="S65E"&Date.EST==date.fun("1990-06-18"))

# Daily mean WQ removing LAB project code.
wq.dat.xtab=dcast(subset(wq.dat,Project.Code!="LAB"),Station.ID+Date.EST~Param,value.var="HalfMDL",mean,na.rm=T)
if(sum(names(wq.dat.xtab)%in%c("TKN"))==0){wq.dat.xtab$TKN=NA}
wq.dat.xtab$TN=with(wq.dat.xtab,TN_Combine(NOx,TKN,TN))
head(wq.dat.xtab)

ALIAS.vals=ddply(flow.dbkeys,"ALIAS",summarise,N.val=N.obs(DBKEY))
date.fill.frame=expand.grid(Date.EST=date.fun(seq(dates[1],dates[2],"1 days")),
                            ALIAS=ALIAS.vals$ALIAS)
flow.vars=c("STRUCT","ALIAS","WQSite","Basin","Date.EST","WY","direct","fflow.cfs")
wq.vars=c("Date.EST","Station.ID","TP","TN",'TSS')

flow.wq=merge(flow.xtab[,flow.vars],wq.dat.xtab[,wq.vars],by.x=c("Date.EST","WQSite"),by.y=c("Date.EST","Station.ID"),all.x=T)
head(flow.wq)
flow.wq=merge(date.fill.frame,flow.wq,c("Date.EST","ALIAS"),all.y=T)
flow.wq=flow.wq[order(flow.wq$ALIAS,flow.wq$Date.EST),]

# ddply(flow.wq,c("ALIAS","STRUCT",'WQSite'),summarise,N.val=N.obs(TP))
flow.wq$TP.int=with(flow.wq,ave(TP,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TPLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TP.int))
flow.wq$TN.int=with(flow.wq,ave(TN,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TNLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TN.int))
flow.wq$TSS.int=with(flow.wq,ave(TSS,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TSSLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TSS.int))

flow.wq$month=as.numeric(format(flow.wq$Date.EST,"%m"))
flow.wq$CY=as.numeric(format(flow.wq$Date.EST,"%Y"))

TPload.mon.sum=dcast(flow.wq,STRUCT+ALIAS+WQSite+Basin+WY+CY+month~direct,value.var="TPLoad.kg",fun.aggregate=function(x) sum(x,na.rm=T))
TPload.mon.sum=TPload.mon.sum[,c("STRUCT", "ALIAS","WQSite", "Basin", "WY", "CY", "month","Inflow", "Outflow")]

TNload.mon.sum=dcast(flow.wq,STRUCT+ALIAS+WQSite+Basin+WY+CY+month~direct,value.var="TNLoad.kg",fun.aggregate=function(x) sum(x,na.rm=T))
TNload.mon.sum=TNload.mon.sum[,c("STRUCT", "ALIAS","WQSite", "Basin", "WY", "CY", "month","Inflow", "Outflow")]

# Annual Load -------------------------------------------------------------
WY.TPLoad=ddply(subset(flow.wq,direct=="Inflow"),c("WY","direct"),summarise,
                inflow.TPload=sum(kg.to.mt(TPLoad.kg),na.rm=T),
                inflow.flow=sum(cfs.to.m3d(fflow.cfs),na.rm=T),
                inflow.FWM=sum(TPLoad.kg*1e9,na.rm=T)/sum(inflow.flow*1000,na.rm=T))
# WY.TPLoad=ddply(TPload.mon.sum,"WY",summarise,inflow.load=sum(kg.to.mt(Inflow),na.rm=T),outflow.load=sum(kg.to.mt(Outflow),na.rm=T))

WYs2=seq(1974,2023,1)
WY.TPLoad=subset(WY.TPLoad,WY%in%WYs2)
WY.TPLoad$atm=35

# png(filename=paste0(plot.path,"LakeO_WY_Inflow.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.75,3),oma=c(1,2,1,1),lwd=0.1);

ylim.val=c(0,1200);by.y=300;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1974,2023);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
xlabs=seq(xlim.val[1],xlim.val[2],1)

x=barplot(t(WY.TPLoad[,c("inflow.TPload","atm")]),space=0,col=NA,border=NA,
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col="grey",lwd=0.5)
x=barplot(t(WY.TPLoad[,c("inflow.TPload","atm")]),space=0,col=c(adjustcolor("dodgerblue1",0.5),"white"),border="grey20",
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)),add=T)
abline(h=140,col=adjustcolor("red",0.5),lwd=2)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,las=0,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(new=T);x=barplot(WY.TPLoad$inflow.FWM,col=NA,border=NA,axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)))
pt_line(x,WY.TPLoad$inflow.FWM,2,"black",1,19,"black")
axis_fun(4,ymaj,ymin,ymaj)

mtext(side=1,line=1.5,"Water Year")
mtext(side=2,line=2.75,"TP Load (tons WY\u207B\u00B9)")
mtext(side=4,line=2.5,"TP FWM (\u03Bcg L\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee Inflow",font=3)
legend("topleft",
       legend=c("Inflow","Atmospheric Deposition","TP FWM","TMDL (140 tons)"),
       pch=c(22,22,19,NA),pt.bg=c(adjustcolor("dodgerblue1",0.5),"white","black",NA),
       lty=c(0,0,0,1),lwd=c(rep(0.1,3),1),col=c("grey20","grey20","black","red"),
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

dev.off()

# png(filename=paste0(plot.path,"LakeO_WY_Inflow_v2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.75,0.5),oma=c(1,2,1,0.5),lwd=0.1);

ylim.val=c(0,1200);by.y=300;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1974,2023);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
xlabs=seq(xlim.val[1],xlim.val[2],1)

x=barplot(t(WY.TPLoad[,c("inflow.TPload","atm")]),space=0,col=NA,border=NA,
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col="grey",lwd=0.5)
x=barplot(t(WY.TPLoad[,c("inflow.TPload","atm")]),space=0,col=c(adjustcolor("dodgerblue1",0.5),"white"),border="grey20",
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)),add=T)
abline(h=140,col=adjustcolor("red",0.5),lwd=2)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,las=0,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

# ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# par(new=T);x=barplot(WY.TPLoad$inflow.FWM,col=NA,border=NA,axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)))
# pt_line(x,WY.TPLoad$inflow.FWM,2,"black",1,19,"black")
# axis_fun(4,ymaj,ymin,ymaj)

mtext(side=1,line=1.5,"Water Year")
mtext(side=2,line=2.75,"TP Load (tons WY\u207B\u00B9)")
# mtext(side=4,line=2.5,"TP FWM (\u03Bcg L\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee Inflow",font=3)
legend("topleft",
       legend=c("Inflow","Atmospheric Deposition","TMDL (140 tons)"),
       pch=c(22,22,NA),pt.bg=c(adjustcolor("dodgerblue1",0.5),"white",NA),
       lty=c(0,0,1),lwd=c(rep(0.1,2),1),col=c("grey20","grey20","red"),
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

dev.off()

# In-Lake -----------------------------------------------------------------
wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC"))
wq.param=subset(wq.param,!(Param%in%c("NH4","DOC","TOC","OP")))

inlake.sites=c("L001", "L004", "L005", "L006", "L007", "L008", "LZ40", "LZ30")

# dates=date.fun(c("1960-05-01","2023-05-01"))

lake.wq.dat=data.frame()
for(i in 1:length(inlake.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],inlake.sites[i],wq.param$Test.Number)
  lake.wq.dat=rbind(tmp,lake.wq.dat)
  print(i)
}
range(lake.wq.dat$Date.EST)

lake.wq.dat=merge(lake.wq.dat,wq.param,"Test.Number")
lake.wq.dat=subset(lake.wq.dat,Collection.Method=="G") # Grab sample only 
lake.wq.dat$month=as.numeric(format(lake.wq.dat$Date.EST,"%m"))
lake.wq.dat$CY=as.numeric(format(lake.wq.dat$Date.EST,"%Y"))
lake.wq.dat$WY=WY(lake.wq.dat$Date.EST)
lake.wq.dat$season=FL.Hydroseason(lake.wq.dat$Date.EST)

ddply(lake.wq.dat,c("WY","Param","Station.ID"),summarise,N.val=N.obs(HalfMDL))
ddply(subset(lake.wq.dat,Param=="TP"),c("WY","Param","Station.ID"),summarise,N.val=N.obs(HalfMDL))

dcast(subset(lake.wq.dat,Param=="TP"),WY+Station.ID~season,value.var = "HalfMDL",N.obs)
dcast(lake.wq.dat,CY+month+WY~Param,value.var="HalfMDL",N.obs)

lake.wq.mon=dcast(subset(lake.wq.dat,WY>=1974),CY+month+WY~Param,value.var="HalfMDL",mean)
lake.wq.WY=dcast(lake.wq.dat,WY~Param,value.var="HalfMDL",mean)
lake.wq.WY$TP_3Yr=with(lake.wq.WY,c(rep(NA,2),zoo::rollapply(TP,width=3,FUN=function(x)mean(x,na.rm=T))))


# png(filename=paste0(plot.path,"LakeO_WY_InflowLoad_Inlake.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.75,0.5),lwd=0.1);
layout(matrix(1:2,2,1))

ylim.val=c(0,1400);by.y=300;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1979,2023);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
xlabs=seq(xlim.val[1],xlim.val[2],1)

x=barplot(t(WY.TPLoad[,c("inflow.TPload","atm")]),space=0,col=NA,border=NA,
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col="grey",lwd=0.5)
x=barplot(t(WY.TPLoad[,c("inflow.TPload","atm")]),space=0,col=c(adjustcolor("dodgerblue1",0.5),"white"),border="grey20",
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)),add=T)
abline(h=140,col=adjustcolor("red",0.5),lwd=2)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,las=0,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"TP Load (tons WY\u207B\u00B9)")
# mtext(side=1,line=2,"Water Year")
# mtext(side=4,line=2.5,"TP FWM (\u03Bcg L\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee Inflow",font=3)
legend("topleft",
       legend=c("Inflow","Atmospheric Deposition"),
       pch=c(22,22),pt.bg=c(adjustcolor("dodgerblue1",0.5),"white"),
       lty=c(0),lwd=0.1,col=c("grey20","grey20"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

ylim.val=c(0,0.45);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(subset(lake.wq.WY,WY>=1979)$TP,space=0,col=NA,border=NA,
          axes=F,xaxs="i",yaxs="i",ylim=ylim.val,names.arg = rep(NA,nrow(WY.TPLoad)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col="grey",lwd=0.5)
with(subset(lake.wq.WY,WY>=1979),pt_line(x,TP,2,"indianred1",1,21,"indianred1",pt.lwd=0.1,cex=1))
with(WY.TPLoad,pt_line(x,inflow.FWM/1000,2,"black",1,19,"black",pt.lwd=0.1,cex=1))
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,las=0,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj*1000);box(lwd=1)
mtext(side=1,line=1.75,"Water Year")
mtext(side=2,line=2.75,"TP (\u03BCg L\u207B\u00B9)")
legend("topleft",
       legend=c("In-Lake Average","Inflow Flow-weighted Mean"),
       pch=c(21,19),pt.bg=c("indianred1","black"),
       lty=c(0),lwd=0.1,col=c("grey20","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
dev.off()



# png(filename=paste0(plot.path,"LakeO_WY_Inlake.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.75,0.5));

xlim.val=c(1974,2023);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,0.40);by.y=0.05;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(TP~WY,lake.wq.WY,ann=F,axes=F,type="n",xlim=xlim.val,ylim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(lake.wq.WY,pt_line(WY,TP,2,"indianred1",1,21,"indianred1",pt.lwd=0.1,cex=1))
with(WY.TPLoad,pt_line(WY,inflow.FWM/1000,2,"black",1,19,"black",pt.lwd=0.1,cex=1))
axis_fun(1,xmaj,xmin,xmaj,las=0,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj*1000);box(lwd=1)
mtext(side=1,line=1.75,"Water Year")
mtext(side=2,line=2.75,"TP (\u03BCg L\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee",font=3)
legend("topleft",
       legend=c("In-Lake Average","Inflow Flow-weighted Mean"),
       pch=c(21,19),pt.bg=c("indianred1","black"),
       lty=c(0),lwd=0.1,col=c("grey20","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
dev.off()

# png(filename=paste0(plot.path,"LakeO_WY_Inlake_v2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.5),oma=c(2,2,0.75,0.5));

xlim.val=c(1974,2023);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,0.250);by.y=0.05;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(TP~WY,lake.wq.WY,ann=F,axes=F,type="n",xlim=xlim.val,ylim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(lake.wq.WY,pt_line(WY,TP,2,"indianred1",1,21,"indianred1",pt.lwd=0.1,cex=1))
# with(WY.TPLoad,pt_line(WY,inflow.FWM/1000,2,"black",1,19,"black",pt.lwd=0.1,cex=1))
axis_fun(1,xmaj,xmin,xmaj,las=0,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj*1000);box(lwd=1)
mtext(side=1,line=1.75,"Water Year")
mtext(side=2,line=2.75,"TP (\u03BCg L\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee",font=3)
legend("topleft",
       legend=c("In-Lake Average"),
       pch=c(21),pt.bg=c("indianred1"),
       lty=c(0),lwd=0.1,col=c("grey20"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
dev.off()