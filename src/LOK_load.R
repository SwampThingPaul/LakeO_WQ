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
dates=date.fun(c("1978-05-01","2022-05-01"))


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
paste(flow.dbkeys$DBKEY,collapse="/")

## Check metadata
DBHYDRO.meta.bysite=function(site,type,cat="SW"){
  # c("FLOW","STG","GATE")
  # cat = c("SW","RAIN","ETP")
  site.vals=paste0("v_site=",site)
  link=paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_js_flag=Y&v_category=",cat,"&",
              site.vals,
              "&v_data_type=",type,
              "&v_dbkey_list_flag=Y&v_order_by=STATION")
  rslt.table=rvest::read_html(link)
  rslt.table=data.frame(rvest::html_table(rslt.table,fill=T)[[5]])
  rslt.table=rslt.table[,2:ncol(rslt.table)]
  colnames(rslt.table)=toupper(names(rslt.table))
  return(rslt.table)
  
}
# DBHYDRO.meta.bysite("S333","FLOW")

DBHYDRO.meta.byDBKEY=function(DBKEY){
  
  link=paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_js_flag=Y&v_dbkey=",
              paste(DBKEY,collapse="/"),
              "&v_category=SW")
  rslt.table=rvest::read_html(link)
  rslt.table=data.frame(rvest::html_table(rslt.table,fill=T)[[5]])
  rslt.table=rslt.table[,2:ncol(rslt.table)]
  colnames(rslt.table)=toupper(names(rslt.table))
  return(rslt.table)
  
}
# DBHYDRO.meta.byDBKEY(flow.dbkeys$DBKEY)
#
# DBHYDRO.meta.link=paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_js_flag=Y&v_dbkey=",
#                          paste(flow.dbkeys$DBKEY,collapse="/"),
#                          "&v_category=SW")
# flow.meta=read_html(DBHYDRO.meta.link)
# flow.meta=data.frame(html_table(flow.meta,fill=T)[[5]])

flow.meta=DBHYDRO.meta.byDBKEY(flow.dbkeys$DBKEY)
# head(flow.meta)
flow.meta$START.DATE=date.fun(flow.meta$START.DATE,form="%d-%B-%Y")
flow.meta$END.DATE=date.fun(flow.meta$END.DATE,form="%d-%B-%Y")

# max(flow.meta$END.DATE)
# min(flow.meta$END.DATE)
# subset(flow.meta,END.DATE<date.fun("2023-03-13"))

subset(flow.meta,DBKEY=="WH036")
DBHYDRO.meta.bysite("FISHCR","FLOW")
# -------------------------------------------------------------------------
edate=date.fun(as.character(Sys.Date()))
sdate=date.fun(paste(format(edate-lubridate::duration(4,"years"),"%Y"),"05-01",sep="-"))
dates=c(sdate,edate)

subset(flow.meta,END.DATE%in%seq(sdate,edate,"1 day"))
flow.dbkeys
flow.dbkeys2=subset(flow.dbkeys,DBKEY%in%subset(flow.meta,END.DATE%in%seq(sdate,edate,"1 day"))$DBKEY)

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



out.region=data.frame(STRUCT=c("S2.S351","S3.S354","S352","CU10A","S3","S2","S351_TEMP","S352_TEMP","S354_TEMP",
                               "S77",
                               "S308"),
                      region=c(rep("south",9),
                               "west",
                               "east")
                      )
TPload.mon.sum=merge(TPload.mon.sum,out.region,"STRUCT",all.x=T)
TNload.mon.sum=merge(TNload.mon.sum,out.region,"STRUCT",all.x=T)
flow.mon.sum=merge(flow.mon.sum,out.region,"STRUCT",all.x=T)
# -------------------------------------------------------------------------
library(zoo)
Qload.mon.sum2=ddply(flow.mon.sum,c("WY","CY","month"),summarise,
                     inflow.flow=sum(Inflow,na.rm=T),
                     outflow.flow=sum(Outflow,na.rm=T))
Qload.mon.sum2$DOWY=with(Qload.mon.sum2,hydro.day(date.fun(paste(CY,month,"01",sep="-"))))
Qload.mon.sum2$cumInflow=with(Qload.mon.sum2,ave(inflow.flow,WY,FUN=function(x)cumsum(x)/1e3))
Qload.mon.sum2$cumOutflow=with(Qload.mon.sum2,ave(outflow.flow,WY,FUN=function(x)cumsum(x)/1e3))

TPload.mon.sum2=ddply(TPload.mon.sum,c("WY","CY","month"),summarise,
                      inflow.load=sum(Inflow,na.rm=T),
                      outflow.load=sum(Outflow,na.rm=T))
TPload.mon.sum2$DOWY=with(TPload.mon.sum2,hydro.day(date.fun(paste(CY,month,"01",sep="-"))))
TPload.mon.sum2$cumInflow=with(TPload.mon.sum2,ave(inflow.load,WY,FUN=function(x)cumsum(x)/1e3))
TPload.mon.sum2$cumOutflow=with(TPload.mon.sum2,ave(outflow.load,WY,FUN=function(x)cumsum(x)/1e3))

TPload.mon.sum2=merge(TPload.mon.sum2,Qload.mon.sum2[,c("CY","month","inflow.flow","outflow.flow")],c("CY","month"))
TPload.mon.sum2$inflow.FWM=with(TPload.mon.sum2,(inflow.load/(inflow.flow*1.233e6))*1e9)
TPload.mon.sum2$outflow.FWM=with(TPload.mon.sum2,(outflow.load/(outflow.flow*1.233e6))*1e9)
TPload.mon.sum2=TPload.mon.sum2[order(TPload.mon.sum2$CY,TPload.mon.sum2$month),]
TPload.mon.sum2$inflow.FWM.ma=with(TPload.mon.sum2,rollapply(inflow.load,12,sum,align="right",fill=NA)/rollapply(inflow.flow*1.233e6,12,sum,align="right",fill=NA)*1e9)
TPload.mon.sum2$outflow.FWM.ma=with(TPload.mon.sum2,rollapply(outflow.load,12,sum,align="right",fill=NA)/rollapply(outflow.flow*1.233e6,12,sum,align="right",fill=NA)*1e9)
TPload.mon.sum2$MonCY.date=with(TPload.mon.sum2,date.fun(paste(CY,month,"01",sep="-")))


TNload.mon.sum2=ddply(TNload.mon.sum,c("WY","CY","month"),summarise,
                      inflow.load=sum(Inflow,na.rm=T),
                      outflow.load=sum(Outflow,na.rm=T))
TNload.mon.sum2$DOWY=with(TNload.mon.sum2,hydro.day(date.fun(paste(CY,month,"01",sep="-"))))
TNload.mon.sum2$cumInflow=with(TNload.mon.sum2,ave(inflow.load,WY,FUN=function(x)cumsum(x)/1e3))
TNload.mon.sum2$cumOutflow=with(TNload.mon.sum2,ave(outflow.load,WY,FUN=function(x)cumsum(x)/1e3))

TNload.mon.sum2=merge(TNload.mon.sum2,Qload.mon.sum2[,c("CY","month","inflow.flow","outflow.flow")],c("CY","month"))
TNload.mon.sum2$inflow.FWM=with(TNload.mon.sum2,(inflow.load/(inflow.flow*1.233e6))*1e6)
TNload.mon.sum2$outflow.FWM=with(TNload.mon.sum2,(outflow.load/(outflow.flow*1.233e6))*1e6)
TNload.mon.sum2=TNload.mon.sum2[order(TNload.mon.sum2$CY,TNload.mon.sum2$month),]
TNload.mon.sum2$inflow.FWM.ma=with(TNload.mon.sum2,rollapply(inflow.load,12,sum,align="right",fill=NA)/rollapply(inflow.flow*1.233e6,12,sum,align="right",fill=NA)*1e6)
TNload.mon.sum2$outflow.FWM.ma=with(TNload.mon.sum2,rollapply(outflow.load,12,sum,align="right",fill=NA)/rollapply(outflow.flow*1.233e6,12,sum,align="right",fill=NA)*1e6)
TNload.mon.sum2$MonCY.date=with(TNload.mon.sum2,date.fun(paste(CY,month,"01",sep="-")))


## regional summary
Qload.mon.sum.region=ddply(subset(flow.mon.sum,is.na(region)==F),c("region","WY","CY","month"),summarise,
                     inflow.flow=sum(Inflow,na.rm=T),
                     outflow.flow=sum(Outflow,na.rm=T))
Qload.mon.sum.region$DOWY=with(Qload.mon.sum.region,hydro.day(date.fun(paste(CY,month,"01",sep="-"))))
Qload.mon.sum.region$cumOutflow=with(Qload.mon.sum.region,ave(outflow.flow,region,WY,FUN=function(x)cumsum(x)/1e3))

TPload.mon.sum.region=ddply(subset(TPload.mon.sum,is.na(region)==F),c("region","WY","CY","month"),summarise,
                      inflow.load=sum(Inflow,na.rm=T),
                      outflow.load=sum(Outflow,na.rm=T))
TPload.mon.sum.region$DOWY=with(TPload.mon.sum.region,hydro.day(date.fun(paste(CY,month,"01",sep="-"))))
TPload.mon.sum.region$cumOutflow=with(TPload.mon.sum.region,ave(outflow.load,region,WY,FUN=function(x)cumsum(x)/1e3))
TPload.mon.sum.region=merge(TPload.mon.sum.region,Qload.mon.sum.region[,c("region","WY","CY","month","inflow.flow","outflow.flow")],c("region","WY","CY","month"))
TPload.mon.sum.region$outflow.FWM=with(TPload.mon.sum.region,(outflow.load/(outflow.flow*1.233e6))*1e9)
TPload.mon.sum.region=TPload.mon.sum.region[order(TPload.mon.sum.region$region,TPload.mon.sum.region$CY,TPload.mon.sum.region$month),]
TPload.mon.sum.region$MonCY.date=with(TPload.mon.sum.region,date.fun(paste(CY,month,"01",sep="-")))
reg.vals=c("west","south","east")
TPload.mon.sum.region$outflow.FWM.ma=NA
for(i in 1:3){
  TPload.mon.sum.region[TPload.mon.sum.region$region==reg.vals[i],"outflow.FWM.ma"]=rollapply(TPload.mon.sum.region[TPload.mon.sum.region$region==reg.vals[i],"outflow.load"],12,sum,align="right",fill=NA)/rollapply(TPload.mon.sum.region[TPload.mon.sum.region$region==reg.vals[i],"outflow.flow"]*1.233e6,12,sum,align="right",fill=NA)*1e9
}

TNload.mon.sum.region=ddply(subset(TNload.mon.sum,is.na(region)==F),c("region","WY","CY","month"),summarise,
                            inflow.load=sum(Inflow,na.rm=T),
                            outflow.load=sum(Outflow,na.rm=T))
TNload.mon.sum.region$DOWY=with(TNload.mon.sum.region,hydro.day(date.fun(paste(CY,month,"01",sep="-"))))
TNload.mon.sum.region$cumOutflow=with(TNload.mon.sum.region,ave(outflow.load,region,WY,FUN=function(x)cumsum(x)/1e3))
TNload.mon.sum.region=merge(TNload.mon.sum.region,Qload.mon.sum.region[,c("region","WY","CY","month","inflow.flow","outflow.flow")],c("region","WY","CY","month"))
TNload.mon.sum.region$outflow.FWM=with(TNload.mon.sum.region,(outflow.load/(outflow.flow*1.233e6))*1e6)
TNload.mon.sum.region=TNload.mon.sum.region[order(TNload.mon.sum.region$region,TNload.mon.sum.region$CY,TNload.mon.sum.region$month),]
TNload.mon.sum.region$MonCY.date=with(TNload.mon.sum.region,date.fun(paste(CY,month,"01",sep="-")))
TNload.mon.sum.region$outflow.FWM.ma=NA
for(i in 1:3){
  TNload.mon.sum.region[TNload.mon.sum.region$region==reg.vals[i],"outflow.FWM.ma"]=rollapply(TNload.mon.sum.region[TNload.mon.sum.region$region==reg.vals[i],"outflow.load"],12,sum,align="right",fill=NA)/rollapply(TNload.mon.sum.region[TNload.mon.sum.region$region==reg.vals[i],"outflow.flow"]*1.233e6,12,sum,align="right",fill=NA)*1e6
}

CurWY=WY(Sys.Date())
reg.vals=c("west","south","east")
reg.vals.labs=c("West (S77)" ,"South (S351,S352,S354,C10A)","East (S308)")
WY.vals=c(CurWY,CurWY-1,CurWY-2)
cols=rev(adjustcolor(wesanderson::wes_palette("Zissou1",3,"continuous"),0.5))

# png(filename=paste0(plot.path,"LakeO_Regional_cum.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.75),oma=c(2,3.5,1,0.1));
layout(matrix(1:9,3,3,byrow=T))
xlim.val=c(0,365);# by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
xlim.val2=as.Date(xlim.val,origin="2000-05-01");xmaj=seq(xlim.val2[1],xlim.val2[2],"3 months");xmin=hydro.day(seq(xlim.val2[1],xlim.val2[2],"1 months"))
xmaj.labs=format(xmaj,"%b");xmaj=hydro.day(xmaj)

ylim.val=c(0,500);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(cumOutflow~DOWY,Qload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  for(j in 1:length(WY.vals)){
    with(subset(Qload.mon.sum.region,region==reg.vals[i]&WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
  }
  axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"Annual Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)",cex=0.8)}
  mtext(side=3,adj=0,reg.vals.labs[i],cex=0.8)
  if(i==1){
    legend("topleft",
           legend=c(paste0(CurWY," - Incomplete Year"),CurWY-1,CurWY-2),
           pch=NA,pt.bg=NA,
           lty=c(1),lwd=2,col=cols,
           pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
           title.adj = 0,title=" Water Year")
  }
}

ylim.val=c(0,110);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(cumOutflow~DOWY,TPload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  for(j in 1:length(WY.vals)){
    with(subset(TPload.mon.sum.region,region==reg.vals[i]&WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
  }
  axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"Annual TP Load\n(tons Y\u207B\u00B9)",cex=0.8)}
}

ylim.val=c(0,1500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(cumOutflow~DOWY,TNload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  for(j in 1:length(WY.vals)){
    with(subset(TNload.mon.sum.region,region==reg.vals[i]&WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
  }
  axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"Annual TN Load\n(tons Y\u207B\u00B9)",cex=0.8)}
  # mtext(side=3,adj=0,reg.vals.labs[i])
}
mtext(side=1,outer=T,"Day of Water Year",line=0.75)
dev.off()

pal1=colorRampPalette(c("#ffb400", "#d2980d", "#a57c1b", "#786028", "#363445", "#48446e", "#5e569b", "#776bcd", "#9080ff"))
reg.vals=c("west","south","east")
reg.vals.labs=c("West (S77)" ,"South (S351,S352,S354,C10A)","East (S308)")
cols=adjustcolor(wesanderson::wes_palette("IsleofDogs1",3,"continuous"),0.5)
cols=adjustcolor(pal1(3),0.5)
# png(filename=paste0(plot.path,"LakeO_Regional_FWM.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,0.75),oma=c(2,3.5,1,0.1));
layout(matrix(1:6,2,3,byrow=T))
xlim.val=date.fun(c(paste(CurWY-2,"05-01",sep="-"),paste(CurWY,"05-01",sep="-")));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")

ylim.val=c(100,1000);ymaj=c(log.scale.fun(ylim.val,"major"),500);ymin=log.scale.fun(ylim.val,"minor")# by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
plot(outflow.FWM~MonCY.date,TPload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,log="y")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(TPload.mon.sum.region,region==reg.vals[i]),pt_line(MonCY.date,outflow.FWM,1,cols[i],1.5,19,cols[i],pt.col=cols[i],cex=1.25))
lines(outflow.FWM.ma~MonCY.date,subset(TPload.mon.sum.region,region==reg.vals[i]),lty=2,col=cols[i],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
if(i==1){mtext(side=2,line=2.75,"TP FWM (\u03BCg L\u207B\u00B9)")}
mtext(side=3,adj=0,reg.vals.labs[i],cex=0.8)
if(i==1){legend("topleft",
       legend=c("Monthly","12-month moving average"),
       pch=NA,pt.bg=NA,
       lty=c(1,2),lwd=2,col="grey",
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
}
}

ylim.val=c(1,10);ymaj=c(log.scale.fun(ylim.val,"major"),500);ymin=log.scale.fun(ylim.val,"minor")# by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(outflow.FWM~MonCY.date,TNload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  with(subset(TNload.mon.sum.region,region==reg.vals[i]),pt_line(MonCY.date,outflow.FWM,1,cols[i],1.5,19,cols[i],pt.col=cols[i],cex=1.25))
  lines(outflow.FWM.ma~MonCY.date,subset(TNload.mon.sum.region,region==reg.vals[i]),lty=2,col=cols[i],lwd=1.5)
  axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side=2,line=2.75,"TN FWM (mg L\u207B\u00B9)")}

}
mtext(side=1,outer=T,"Water Year",line=0.75)
dev.off()





plot(inflow.FWM~MonCY.date,TPload.mon.sum2,type="l")
lines(inflow.FWM.ma~MonCY.date,TPload.mon.sum2,lty=2)

plot(outflow.FWM~MonCY.date,TPload.mon.sum2,type="l")
lines(outflow.FWM.ma~MonCY.date,TPload.mon.sum2,lty=2)

subset(TPload.mon.sum,month==4)

CurWY=WY(Sys.Date())
cols=rev(adjustcolor(wesanderson::wes_palette("Zissou1",3,"continuous"),0.5))
# png(filename=paste0(plot.path,"LakeO_TPcumLoad.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.75),oma=c(2,2,0.75,0.1));
layout(matrix(1:2,1,2))
xlim.val=c(0,365);# by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
xlim.val2=as.Date(xlim.val,origin="2000-05-01");xmaj=seq(xlim.val2[1],xlim.val2[2],"3 months");xmin=hydro.day(seq(xlim.val2[1],xlim.val2[2],"1 months"))
xmaj.labs=format(xmaj,"%b");xmaj=hydro.day(xmaj)

ylim.val=c(0,550);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cumInflow~DOWY,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(TPload.mon.sum2,WY==CurWY),pt_line(DOWY,cumInflow,1,cols[1],2,19,cols[1],pt.col=cols[1],cex=1.25))
with(subset(TPload.mon.sum2,WY==CurWY-1),pt_line(DOWY,cumInflow,1,cols[2],2,19,cols[2],pt.col=cols[2],cex=1.25))
with(subset(TPload.mon.sum2,WY==CurWY-2),pt_line(DOWY,cumInflow,1,cols[3],2,19,cols[3],pt.col=cols[3],cex=1.25))
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Annual TP Load (tons Y\u207B\u00B9)")
# mtext(side=3,adj=0,line=-1.25," Inflow",font=2)
mtext(side=3,"Inflow",font=2)
legend("topleft",
       legend=c(paste0(CurWY," - Incomplete Year"),CurWY-1,CurWY-2),
       pch=NA,pt.bg=NA,
       lty=c(1),lwd=2,col=cols,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Water Year")

plot(cumInflow~DOWY,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(TPload.mon.sum2,WY==CurWY),pt_line(DOWY,cumOutflow,1,cols[1],2,19,cols[1],pt.col=cols[1],cex=1.25))
with(subset(TPload.mon.sum2,WY==CurWY-1),pt_line(DOWY,cumOutflow,1,cols[2],2,19,cols[2],pt.col=cols[2],cex=1.25))
with(subset(TPload.mon.sum2,WY==CurWY-2),pt_line(DOWY,cumOutflow,1,cols[3],2,19,cols[3],pt.col=cols[3],cex=1.25))
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Outflow",font=2)
mtext(side=1,outer=T,"Day of Water Year",line=0.75)
dev.off()

# png(filename=paste0(plot.path,"LakeO_TNcumLoad.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.75),oma=c(2,2,0.75,0.1));
layout(matrix(1:2,1,2))
xlim.val=c(0,365);# by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
xlim.val2=as.Date(xlim.val,origin="2000-05-01");xmaj=seq(xlim.val2[1],xlim.val2[2],"3 months");xmin=hydro.day(seq(xlim.val2[1],xlim.val2[2],"1 months"))
xmaj.labs=format(xmaj,"%b");xmaj=hydro.day(xmaj)

ylim.val=c(0,5500);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cumInflow~DOWY,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(TNload.mon.sum2,WY==CurWY),pt_line(DOWY,cumInflow,1,cols[1],2,19,cols[1],pt.col=cols[1],cex=1.25))
with(subset(TNload.mon.sum2,WY==CurWY-1),pt_line(DOWY,cumInflow,1,cols[2],2,19,cols[2],pt.col=cols[2],cex=1.25))
with(subset(TNload.mon.sum2,WY==CurWY-2),pt_line(DOWY,cumInflow,1,cols[3],2,19,cols[3],pt.col=cols[3],cex=1.25))
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Annual TN Load (tons Y\u207B\u00B9)")
# mtext(side=3,adj=0,line=-1.25," Inflow",font=2)
mtext(side=3,"Inflow",font=2)
legend("topleft",
       legend=c(paste0(CurWY," - Incomplete Year"),CurWY-1,CurWY-2),
       pch=NA,pt.bg=NA,
       lty=c(1),lwd=2,col=cols,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Water Year")

plot(cumInflow~DOWY,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(TNload.mon.sum2,WY==CurWY),pt_line(DOWY,cumOutflow,1,cols[1],2,19,cols[1],pt.col=cols[1],cex=1.25))
with(subset(TNload.mon.sum2,WY==CurWY-1),pt_line(DOWY,cumOutflow,1,cols[2],2,19,cols[2],pt.col=cols[2],cex=1.25))
with(subset(TNload.mon.sum2,WY==CurWY-2),pt_line(DOWY,cumOutflow,1,cols[3],2,19,cols[3],pt.col=cols[3],cex=1.25))
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Outflow",font=2)
mtext(side=1,outer=T,"Day of Water Year",line=0.75)
dev.off()

# png(filename=paste0(plot.path,"LakeO_QcumLoad.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.75),oma=c(2,2,0.75,0.1));
layout(matrix(1:2,1,2))
xlim.val=c(0,365);# by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
xlim.val2=as.Date(xlim.val,origin="2000-05-01");xmaj=seq(xlim.val2[1],xlim.val2[2],"3 months");xmin=hydro.day(seq(xlim.val2[1],xlim.val2[2],"1 months"))
xmaj.labs=format(xmaj,"%b");xmaj=hydro.day(xmaj)

ylim.val=c(0,1500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cumInflow~DOWY,Qload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(Qload.mon.sum2,WY==CurWY),pt_line(DOWY,cumInflow,1,cols[1],2,19,cols[1],pt.col=cols[1],cex=1.25))
with(subset(Qload.mon.sum2,WY==CurWY-1),pt_line(DOWY,cumInflow,1,cols[2],2,19,cols[2],pt.col=cols[2],cex=1.25))
with(subset(Qload.mon.sum2,WY==CurWY-2),pt_line(DOWY,cumInflow,1,cols[3],2,19,cols[3],pt.col=cols[3],cex=1.25))
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Annual Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
# mtext(side=3,adj=0,line=-1.25," Inflow",font=2)
mtext(side=3,"Inflow",font=2)
legend("topleft",
       legend=c(paste0(CurWY," - Incomplete Year"),CurWY-1,CurWY-2),
       pch=NA,pt.bg=NA,
       lty=c(1),lwd=2,col=cols,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Water Year")

plot(cumInflow~DOWY,Qload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(subset(Qload.mon.sum2,WY==CurWY),pt_line(DOWY,cumOutflow,1,cols[1],2,19,cols[1],pt.col=cols[1],cex=1.25))
with(subset(Qload.mon.sum2,WY==CurWY-1),pt_line(DOWY,cumOutflow,1,cols[2],2,19,cols[2],pt.col=cols[2],cex=1.25))
with(subset(Qload.mon.sum2,WY==CurWY-2),pt_line(DOWY,cumOutflow,1,cols[3],2,19,cols[3],pt.col=cols[3],cex=1.25))
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Outflow",font=2)
mtext(side=1,outer=T,"Day of Water Year",line=0.75)
dev.off()



CurWY=WY(Sys.Date())
cols=adjustcolor(c("dodgerblue1","indianred1"),0.5)
# png(filename=paste0(plot.path,"LakeO_TPFWM.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.75),oma=c(2,2,0.75,0.1));
layout(matrix(1:2,1,2))
xlim.val=date.fun(c(paste(CurWY-2,"05-01",sep="-"),paste(CurWY,"05-01",sep="-")));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")

ylim.val=c(0,450);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.FWM~MonCY.date,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TPload.mon.sum2,pt_line(MonCY.date,inflow.FWM,1,cols[1],1.5,19,cols[1],pt.col=cols[1],cex=1.25))
lines(inflow.FWM.ma~MonCY.date,TPload.mon.sum2,lty=2,col=cols[1],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")
mtext(side=3,"Inflow",font=2)
legend("topleft",
       legend=c("Monthly","12-month moving average"),
       pch=NA,pt.bg=NA,
       lty=c(1,2),lwd=2,col="grey",
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

plot(inflow.FWM~MonCY.date,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TPload.mon.sum2,pt_line(MonCY.date,outflow.FWM,1,cols[2],1.5,19,cols[2],pt.col=cols[2],cex=1.25))
lines(outflow.FWM.ma~MonCY.date,TPload.mon.sum2,lty=2,col=cols[2],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Outflow",font=2)
mtext(side=1,outer=T,"Water Year",line=0.75)
dev.off()

# png(filename=paste0(plot.path,"LakeO_TNFWM.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,0.75),oma=c(2,2,0.75,0.1));
layout(matrix(1:2,1,2))
xlim.val=date.fun(c(paste(CurWY-2,"05-01",sep="-"),paste(CurWY,"05-01",sep="-")));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")

ylim.val=c(0,6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.FWM~MonCY.date,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TNload.mon.sum2,pt_line(MonCY.date,inflow.FWM,1,cols[1],1.5,19,cols[1],pt.col=cols[1],cex=1.25))
lines(inflow.FWM.ma~MonCY.date,TNload.mon.sum2,lty=2,col=cols[1],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TN FWM (mg L\u207B\u00B9)")
mtext(side=3,"Inflow",font=2)
legend("topleft",
       legend=c("Monthly","12-month moving average"),
       pch=NA,pt.bg=NA,
       lty=c(1,2),lwd=2,col="grey",
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

plot(inflow.FWM~MonCY.date,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TNload.mon.sum2,pt_line(MonCY.date,outflow.FWM,1,cols[2],1.5,19,cols[2],pt.col=cols[2],cex=1.25))
lines(outflow.FWM.ma~MonCY.date,TNload.mon.sum2,lty=2,col=cols[2],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Outflow",font=2)
mtext(side=1,outer=T,"Water Year",line=0.75)
dev.off()


CurWY=WY(Sys.Date())
cols=adjustcolor(c("dodgerblue1","indianred1"),0.5)
# png(filename=paste0(plot.path,"LakeO_FWM.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,0.75),oma=c(2,2,0.75,0.1));
layout(matrix(1:4,2,2,byrow=T))
xlim.val=date.fun(c(paste(CurWY-2,"05-01",sep="-"),paste(CurWY,"05-01",sep="-")));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")

ylim.val=c(0,450);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.FWM~MonCY.date,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TPload.mon.sum2,pt_line(MonCY.date,inflow.FWM,1,cols[1],1.5,19,cols[1],pt.col=cols[1],cex=1.25))
lines(inflow.FWM.ma~MonCY.date,TPload.mon.sum2,lty=2,col=cols[1],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")
mtext(side=3,"Inflow",font=2)
legend("topleft",
       legend=c("Monthly","12-month moving average"),
       pch=NA,pt.bg=NA,
       lty=c(1,2),lwd=2,col="grey",
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

plot(inflow.FWM~MonCY.date,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TPload.mon.sum2,pt_line(MonCY.date,outflow.FWM,1,cols[1],1.5,19,cols[2],pt.col=cols[1],cex=1.25))
lines(outflow.FWM.ma~MonCY.date,TPload.mon.sum2,lty=2,col=cols[1],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,"Outflow",font=2)

ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.FWM~MonCY.date,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TNload.mon.sum2,pt_line(MonCY.date,inflow.FWM,1,cols[2],1.5,19,cols[2],pt.col=cols[2],cex=1.25))
lines(inflow.FWM.ma~MonCY.date,TNload.mon.sum2,lty=2,col=cols[2],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"TN FWM (mg L\u207B\u00B9)")

plot(inflow.FWM~MonCY.date,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(TNload.mon.sum2,pt_line(MonCY.date,outflow.FWM,1,cols[2],1.5,19,cols[2],pt.col=cols[2],cex=1.25))
lines(outflow.FWM.ma~MonCY.date,TNload.mon.sum2,lty=2,col=cols[2],lwd=1.5)
axis_fun(1,xmaj,xmin,WY(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,outer=T,"Water Year",line=0.75)
dev.off()

CurWY=WY(Sys.Date())
reg.vals.labs=c("Inflow","Outflow")
WY.vals=c(CurWY,CurWY-1,CurWY-2)
cols=rev(adjustcolor(wesanderson::wes_palette("Zissou1",3,"continuous"),0.5))
# png(filename=paste0(plot.path,"LakeO_cum.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.75,0.75),oma=c(2,3.5,1,0.1));
layout(matrix(1:6,3,2,byrow=T))
xlim.val=c(0,365);# by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
xlim.val2=as.Date(xlim.val,origin="2000-05-01");xmaj=seq(xlim.val2[1],xlim.val2[2],"3 months");xmin=hydro.day(seq(xlim.val2[1],xlim.val2[2],"1 months"))
xmaj.labs=format(xmaj,"%b");xmaj=hydro.day(xmaj)

ylim.val=c(0,1500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cumOutflow~DOWY,Qload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(WY.vals)){
    with(subset(Qload.mon.sum2,WY==WY.vals[j]),pt_line(DOWY,cumInflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Annual Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)",cex=0.8)
mtext(side=3,reg.vals.labs[1],cex=0.8,font=2)
legend("topleft",
       legend=c(paste0(CurWY," - Incomplete Year"),CurWY-1,CurWY-2),
       pch=NA,pt.bg=NA,
       lty=c(1),lwd=2,col=cols,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Water Year")

plot(cumOutflow~DOWY,Qload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(WY.vals)){
  with(subset(Qload.mon.sum2,WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,reg.vals.labs[2],cex=0.8,font=2)

ylim.val=c(0,560);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cumOutflow~DOWY,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(WY.vals)){
  with(subset(TPload.mon.sum2,WY==WY.vals[j]),pt_line(DOWY,cumInflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Annual TP Load\n(tons Y\u207B\u00B9)",cex=0.8)

plot(cumOutflow~DOWY,TPload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(WY.vals)){
  with(subset(TPload.mon.sum2,WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

ylim.val=c(0,5600);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cumOutflow~DOWY,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(WY.vals)){
  with(subset(TNload.mon.sum2,WY==WY.vals[j]),pt_line(DOWY,cumInflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Annual TN Load\n(tons Y\u207B\u00B9)",cex=0.8)

plot(cumOutflow~DOWY,TNload.mon.sum2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(WY.vals)){
  with(subset(TNload.mon.sum2,WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
}
axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,outer=T,"Day of Water Year",line=0.75)
dev.off()






ylim.val=c(0,110);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(cumOutflow~DOWY,TPload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  for(j in 1:length(WY.vals)){
    with(subset(TPload.mon.sum.region,region==reg.vals[i]&WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
  }
  axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"Annual TP Load\n(tons Y\u207B\u00B9)",cex=0.8)}
}

ylim.val=c(0,1500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:3){
  plot(cumOutflow~DOWY,TNload.mon.sum.region,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
  for(j in 1:length(WY.vals)){
    with(subset(TNload.mon.sum.region,region==reg.vals[i]&WY==WY.vals[j]),pt_line(DOWY,cumOutflow,1,cols[j],2,19,cols[j],pt.col=cols[j],cex=1.25))
  }
  axis_fun(1,xmaj,xmin,xmaj.labs,line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side=2,line=2.5,"Annual TN Load\n(tons Y\u207B\u00B9)",cex=0.8)}
  # mtext(side=3,adj=0,reg.vals.labs[i])
}
mtext(side=1,outer=T,"Day of Water Year",line=0.75)
dev.off()
