## Title:      Lake Okeechobee Algae and WQ evaluation
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


# GIS layers --------------------------------------------------------------
lakeO=st_transform(st_read(paste0(GIS.path.gen,"/SFWMD"),"LakeOkeechobee_general"),utm17)
plot(st_geometry(lakeO))
st_area(lakeO)

lakeO.lit=st_transform(st_read("C:/Julian_LaCie/_GitHub/CRE_Conditions/report/GISData","LOK_littoral"),utm17)

LOK.area=st_area(lakeO)-st_area(lakeO.lit)

wmd.monitoring=st_transform(st_read(paste0(GIS.path.gen,"/SFWMD_Monitoring_20240125"),"DBHYDRO_SITE_STATION"),utm17)
# plot(st_geometry(subset(wmd.monitoring,ACTIVITY_S=="Continuous Logger")))
# plot(st_geometry(subset(wmd.monitoring,ACTIVITY_S=="Continuous Logger"))[lakeO])
# subset(wmd.monitoring,ACTIVITY_S=="Continuous Logger")[lakeO,]
# sites.val=c("L006","L005","L001","POLESOUT1","POLESOUT3","LZ40")
# subset(wmd.monitoring,SITE%in%sites.val&ACTIVITY_T=="Chemistry")

ID.val=c(1380,1584,2924,3239,3426,3743)
LOK.sonde=subset(wmd.monitoring,OBJECTID%in%ID.val)

# Cyano RS data -----------------------------------------------------------
## Waiting to get dinalized 2023 dataset
CICyano.path="C:/Julian_LaCie/_GitHub/CRE_Conditions/Data/CyanoHAB_RS"

ci.reverse.scaling.fun=function(DN){
  10^(3.0 / 250.0 * DN - 4.2)
}
ci.scaling.fun=function(ci){
  round(83.3 * (log10(ci[ci>0]) + 4.2))
}

yr.vals=seq(2016,2022,1)
dat.inventory=data.frame()
for(i in 1:length(yr.vals)){
fname.vals=list.files(paste0(CICyano.path,"/",yr.vals[i]),full.names = T)
fname.vals=fname.vals[grepl("*.tif|.png",fname.vals)]
fname.vals=fname.vals[grepl("legend.png",fname.vals)==F]

data.product=ifelse(grepl("CIcyano",fname.vals)==T,"CIcyano","Other")

date.vals=strsplit(sapply(fname.vals,"[",1),"\\.")
yr.val=as.numeric(substr(sapply(date.vals,"[",2),1,4))
month.val=as.numeric(substr(sapply(date.vals,"[",3),1,2))
day.val=as.numeric(substr(sapply(date.vals,"[",3),3,4))

dat.inventory=rbind(dat.inventory,
                    data.frame(fname=fname.vals,date=as.Date(paste(yr.val,month.val,day.val,sep="-")),
                    data.product=data.product)
)
}

dat.inventory.CICyano=subset(dat.inventory,data.product=="CIcyano")

tail(dat.inventory.CICyano)


tmp.raster=raster(dat.inventory.CICyano$fname[i])
LOK.sonde=st_transform(LOK.sonde,crs(tmp.raster))
# LOK.sonde=st_transform(LOK.sonde,utm17.wgs)

cyano_area=data.frame()
sonde_CI=data.frame()
pb=txtProgressBar(min=0,max=nrow(dat.inventory.CICyano),style=3)
for(i in 1:nrow(dat.inventory.CICyano)){
  tmp.raster=raster(dat.inventory.CICyano$fname[i])
  # tmp.raster=projectRaster(tmp.raster,crs=crs(lakeO))
  
  # plot(tmp.raster)
  # plot(mask(tmp.raster,gBuffer(lakeO,width=500)))
  tmp.raster=mask(tmp.raster,st_buffer(lakeO,dist=500))
  
  
  # plot(tmp.raster)
  # crs(tmp.raster)
  
  # remove cloud/no data values (see tif header)
  # 0= nodetect
  # 250 = saturated; 251 = ci adj; 252 = land; 253 = cloud;
  # 254 = mixed pixel; 255 = no data
  
  cloud.area=tmp.raster==253
  cloud.area.raster=cloud.area
  cloud.area=cellStats(cloud.area,sum)*raster::res(cloud.area)[1]*raster::res(cloud.area)[2]
  
  other.area=tmp.raster%in%c(0,250,251,254,255)
  other.area.raster=other.area
  other.area=cellStats(other.area,sum)*raster::res(other.area)[1]*raster::res(other.area)[2]
  
  vals=c(0,250,251,252,253,254,255)
  tmp.raster[tmp.raster%in%vals]=NA
  
  rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)*1e8
  
  if(sum(is.infinite(range(getValues(rev.scale),na.rm=T)))==2){next}else{
  
    sonde.x=raster::extract(rev.scale,LOK.sonde)  # extract specific point/consider buffer average
    sonde_CI=rbind(sonde_CI,data.frame(date=dat.inventory.CICyano$date[i],
                                       SITE=LOK.sonde$SITE,
                                       CICyano=sonde.x))
    
    
    med.val=cellStats(rev.scale,median,na.rm=T)
    mean.val=cellStats(rev.scale,mean,na.rm=T)
    max.val=cellStats(rev.scale,max,na.rm=T)
    min.val=cellStats(rev.scale,min,na.rm=T)
    sd.val=cellStats(rev.scale,sd,na.rm=T)
    N.val=N.obs(getValues(rev.scale))
  
    area=rev.scale>0
    val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]
  
    area2=rev.scale>(1000*1000); ## area when blooms exceed 1e6
    val2=cellStats(area2,sum)*raster::res(area2)[1]*raster::res(area2)[2]

    tmp.rslt=data.frame(date=dat.inventory.CICyano$date[i],
                      cloud.area=cloud.area,
                      bloom.area.m2=val,
                      vis.bloom=val2,
                      median=med.val,
                      mean=mean.val,
                      min=min.val,
                      max=max.val,
                      sd=sd.val,
                      N=N.val)
    cyano_area=rbind(cyano_area,tmp.rslt)
    setTxtProgressBar(pb, i)
  }
}
# beepr::beep(4)

sonde_CI2=sonde_CI
# write.csv(sonde_CI,paste0(export.path,"CiCyano_Sonde.csv"),row.names=F)
cyano_area.raw=cyano_area
# write.csv(cyano_area.raw,paste0(export.path,"NOAA_CICyano.csv"),row.names=F)

cyano_area=ddply(cyano_area,c("date"),summarise,
                 cloud.area=max(cloud.area,na.rm=T),
                 bloom.area.m2=max(bloom.area.m2,na.rm=T),
                 vis.bloom=max(vis.bloom,na.rm=T))

plot(bloom.area.m2~date,cyano_area)
plot(vis.bloom~date,cyano_area)

cyano_area$cloud.area.per=cyano_area$cloud.area/LOK.area
# cyano_area$bloom.area.m2.scn=with(cyano_area,ifelse(cloud.area.per>0.25,NA,bloom.area.m2))
cyano_area$bloom.area.mi2=cyano_area$bloom.area.m2*3.86102e-7
cyano_area$bloom.area.per=(as.numeric(cyano_area$bloom.area.m2)/as.numeric(LOK.area))*100
cyano_area$date=date.fun(cyano_area$date)
cyano_area$vis.bloom.mi2=cyano_area$vis.bloom*3.86102e-7# square miles
cyano_area$vis.bloom.per=(as.numeric(cyano_area$vis.bloom)/as.numeric(LOK.area))*100

plot(bloom.area.per~date,cyano_area,type="l",col="blue")
plot(vis.bloom.per~date,cyano_area)

subset(cyano_area,bloom.area.per>90)

plot(bloom.area.per~date,subset(cyano_area,as.numeric(format(date,'%Y'))==2016),type="l")

tmp.raster=raster(subset(dat.inventory.CICyano,date==as.Date("2021-05-11"))$fname[2])
# tmp.raster=raster(subset(dat.inventory.CICyano,date==as.Date("2016-04-27"))$fname)

cloud.area=tmp.raster==253
cloud.area.raster=cloud.area

vals=c(0,250,251,252,253,254,255)
tmp.raster[tmp.raster%in%vals]=NA
rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)*1e8

## Example plot of RS data
par(family="serif",mar=c(0.5,0.5,0.5,0.5),oma=c(0.1,0.1,0.1,0.1));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

b=c(0,20,100,500,1000,6300)*1000
cols=viridisLite::turbo(249,direction=1)
plot(st_geometry(lakeO),lwd=0.05)
plot(st_geometry(lakeO.lit),lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(st_geometry(lakeO),lwd=0.05,add=T)
image(rev.scale,add=T,col = cols)
image(cloud.area.raster,add=T,col=c(NA,"grey"))
mapmisc::scaleBar(lakeO,"bottomright",bty="n",cex=1,seg.len=4,outer=F)

with(subset(cyano_area,date==date.fun("2021-05-11"))[1,],
     mtext(side=3,line=-2.5,adj=0,paste("Date: ",format(date,"%m-%d-%Y"),
                                        "\nData Source: NOAA NCCOS\nAlgae Coverage: ",
                                        round(bloom.area.mi2)," mi\u00B2 (",round(bloom.area.per)," %)",sep="")))
plot(0:1,0:1,ann=F,axes=F,type="n")
b2=b/1000
l.b=length(b2)
labs=b2
n.bks=length(b2) -1
top.val=0.8
bot.val=0.2
mid.v.val=bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
lab.pos=seq(bot.val,top.val,length.out=l.b)
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
text(x=x.max, y = lab.pos, labels = format(b2),cex=0.75,adj=0,pos=4,offset=0.5)
segments(rep(x.min,l.b),lab.pos,rep(x.max,l.b),lab.pos,lwd=2)

legend_image=as.raster(matrix("grey",ncol=1))
rasterImage(legend_image,x.min,bot.val-0.05,x.max,bot.val)
text(x=x.max, y = bot.val-0.025, labels = "Clouds/Invalid data",cex=0.5,adj=0,pos=4,offset=0.5)
legend_image=as.raster(matrix(adjustcolor("honeydew2",0.5),ncol=1))
rasterImage(legend_image,x.min,bot.val-0.1,x.max,bot.val-0.05)
text(x=x.max, y = bot.val-0.075, labels = "Littoral Zone",cex=0.5,adj=0,pos=4,offset=0.5)

text(x=mid.val,y=top.val,expression(paste("CI"["Cyano"]," (cells mL"^"-1","x1000)")),adj=0,cex=0.8,pos=3,xpd=NA)
#leg.fun(b2,cols,expression(paste("CI"["Cyano"]," (cells mL"^"-1","x1000)")),
#        leg.type = "continuous")
## 

cyano_area$week.num=as.numeric(format(cyano_area$date,"%j"))
cyano_area$CY=as.numeric(format(cyano_area$date,"%Y"))
cyano_area$month=as.numeric(format(cyano_area$date,"%m"))

cyano_area2=ddply(cyano_area,c("CY","month"),summarise,mean.bloom.per=mean(bloom.area.per,na.rm=T),max.bloom.per=max(bloom.area.per,na.rm=T))

plot(cyano_area2$mean.bloom.per,type="l")
plot(cyano_area2$max.bloom.per,type="l")

## LOK Discharge ---------------------------------------------------------------
WYs=seq(1979,2021,1)
# dates=date.fun(c("1973-05-01","2023-05-01"))
dates=date.fun(c("2015-05-01","2023-04-30"))

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

flow.meta=DBHYDRO.meta.byDBKEY(flow.dbkeys$DBKEY)

flow.meta$START.DATE=date.fun(flow.meta$START.DATE,form="%d-%B-%Y")
flow.meta$END.DATE=date.fun(flow.meta$END.DATE,form="%d-%B-%Y")


flow.meta=subset(flow.meta,END.DATE>=dates[1])


# -------------------------------------------------------------------------
flow.dbkeys2=subset(flow.dbkeys,DBKEY%in%flow.meta$DBKEY)

## Just to check
# merge(ddply(flow.dbkeys,"STRUCT",summarise,N.val=N.obs(STRUCT)),
#       ddply(flow.dbkeys2,"STRUCT",summarise,N.val=N.obs(STRUCT)),"STRUCT",all.x=T)

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

## Water Quality -----------------------------------------------------------
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

CY.mon.TPLoad=ddply(subset(flow.wq,direct=="Inflow"),c("CY","month","direct"),summarise,
                inflow.TPload=sum(kg.to.mt(TPLoad.kg),na.rm=T),
                inflow.flow=sum(cfs.to.m3d(fflow.cfs),na.rm=T),
                inflow.FWM=sum(TPLoad.kg*1e9,na.rm=T)/sum(inflow.flow*1000,na.rm=T))
CY.mon.TPLoad$date.monCY=with(CY.mon.TPLoad,date.fun(paste(CY,month,"01",sep="-")))

CY.mon.TPLoad2=merge(CY.mon.TPLoad,
                     cyano_area2,
                     c("CY","month"))
CY.mon.TPLoad2=CY.mon.TPLoad2[order(CY.mon.TPLoad2$date.monCY),]


plot(max.bloom.per~date.monCY,CY.mon.TPLoad2,type="l")
par(new=T);plot(inflow.FWM~date.monCY,CY.mon.TPLoad2,type="l",col="grey")

plot(max.bloom.per~inflow.FWM,CY.mon.TPLoad2)
acf(CY.mon.TPLoad2$mean.bloom.per)
acf(CY.mon.TPLoad2$inflow.FWM)
acf(CY.mon.TPLoad2$inflow.TPload)
acf(CY.mon.TPLoad2$inflow.flow)

with(CY.mon.TPLoad2,ccf(inflow.FWM,max.bloom.per))
lag.vals=seq(-15,15,1)
ccf.pearsons=data.frame()
for(h in 1:length(lag.vals)){
  lagged=lag(zoo::as.zoo(CY.mon.TPLoad2$inflow.FWM),lag.vals[h],na.pad=T)
  tmp.dat=zoo::as.zoo(CY.mon.TPLoad2$max.bloom.per)
  stat=with(data.frame(lag=lagged,dat=tmp.dat),cor.test(lag,dat,method="pearson"))
  cor2.vals=with(data.frame(lag=lagged,dat=tmp.dat),cor(lag,dat,use="complete.obs"))
  ccf.pearsons=rbind(ccf.pearsons,data.frame(lag=lag.vals[h],estimate=as.numeric(stat$estimate),pval=stat$p.value,cor2=cor2.vals))
}
ccf.pearsons

points(estimate~lag,ccf.pearsons)

plot(lag(zoo::as.zoo(CY.mon.TPLoad2$inflow.FWM),-11,na.pad=T),zoo::as.zoo(CY.mon.TPLoad2$max.bloom.per))
cor.test(lag(zoo::as.zoo(CY.mon.TPLoad2$inflow.FWM),-11,na.pad=T),zoo::as.zoo(CY.mon.TPLoad2$max.bloom.per))
cor.test(lag(zoo::as.zoo(CY.mon.TPLoad2$inflow.FWM),-5,na.pad=T),zoo::as.zoo(CY.mon.TPLoad2$max.bloom.per))



## In-Lake WQ-----------------------------------------------------------------
### Sonde -------------------------------------------------------------------
sonde.dbkeys=data.frame(SITE=c(rep("L001",11),rep("POLESOUT1",11),rep("POLESOUT3",11),rep("LZ40",11),rep("L006",11),rep("L005",11)),
                        depth="surface",
                        DBKEY=c(39923, 39924, 39920, 39922, 39919, 93664, 93665, 39925, 93663, 39926, 93662,
                                39959, 39960, 39956, 39958, 39955, 93648, 93649, 39961, 93647, 39962, 93646,
                                39975, 39976, 39972, 39974, 39971, 93671, 93672, 39977, 93669, 39981, 93668,
                                39941, 39942, 39938, 39940, 39937, 93628, 93630, 39943, 93627, 39944, 93626,
                                39905, 39906, 39902, 39904, 39901, 93658, 93659, 39907, 93657, 39908, 93656,
                                39886, 39888, 39882, 39885, 39881, 93652, 93653, 39889, 93651, 39890, 93650),
                        PARAM=rep(c("CF", "CU", "DO", "FTURB", "H2OT", "NH4F", "NO3F", "PF", "PH", "PU", "SCOND"),6))

tmp=DBHYDRO.meta.byDBKEY(sonde.dbkeys$DBKEY)
merge(sonde.dbkeys,tmp[,c("DBKEY","DATA.TYPE")],"DBKEY")

sonde.dat=data.frame()
pb=txtProgressBar(min=0,max=nrow(sonde.dbkeys),style=3)
for(i in 1:nrow(sonde.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],sonde.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(sonde.dbkeys$DBKEY[i])
  sonde.dat=rbind(sonde.dat,tmp)
  setTxtProgressBar(pb, i)
}
sonde.dat=merge(sonde.dat,sonde.dbkeys,"DBKEY")

sonde.dat.xtab=dcast(sonde.dat,SITE+Date~PARAM,value.var="Data.Value",mean,na.rm=T)

attributes(sonde.dat.xtab$Date)
unique(sonde.dat.xtab$SITE)
attributes(sonde_CI2$date)
unique(sonde_CI2$SITE)

sonde.dat.xtab$Date=date.fun(sonde.dat.xtab$Date)
sonde_CI2$date=date.fun(sonde_CI2$date)

sonde_CI2=ddply(sonde_CI2,c("date","SITE"),summarise,max.CICyano=max(CICyano,na.rm=T),mean.CICyano=mean(CICyano,na.rm=T))

sonde.dat.xtab=merge(sonde.dat.xtab,sonde_CI2,by.x=c("Date","SITE"),by.y=c("date","SITE"))
sonde.dat.xtab$month=as.numeric(format(sonde.dat.xtab$Date,"%m"))
sonde.dat.xtab$CY=as.numeric(format(sonde.dat.xtab$Date,"%Y"))

plot(mean.CICyano~month,sonde_CI2)
plot(PF~month,sonde.dat.xtab)# something funky with PF data in Jan and Feb
plot(PU~month,sonde.dat.xtab)
plot(CF~month,sonde.dat.xtab)
plot(CU~month,sonde.dat.xtab)


plot(mean.CICyano~PF,sonde.dat.xtab,log="xy")
plot(mean.CICyano~CF,sonde.dat.xtab,log="xy")
plot(mean.CICyano~DO,sonde.dat.xtab)
plot(mean.CICyano~H2OT,sonde.dat.xtab,log="y")
with(sonde.dat.xtab,cor.test(mean.CICyano,H2OT,method="spearman"))


plot(mean.CICyano~date,subset(sonde_CI2,SITE=="POLESOUT3"),log="y")
plot(mean.CICyano~date,subset(sonde_CI2,SITE=="L001"),log="y")
plot(mean.CICyano~date,subset(sonde_CI2,SITE=="L005"),log="y")

### WQ ----------------------------------------------------------------------
wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100,7,61,179,112),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC","WT","Chla","Chla","Chla"))
# wq.param=subset(wq.param,!(Param%in%c("NH4","DOC","TOC","OP")))
inlake.sites=c("L001", "L004", "L005", "L006", "L007", "L008", "LZ40", "LZ30")

wq.inlake.dat=data.frame()
for(i in 1:length(inlake.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],inlake.sites[i],wq.param$Test.Number)
  wq.inlake.dat=rbind(tmp,wq.inlake.dat)
  print(paste0(i,": ",inlake.sites[i]))
}




## Climate index?


# Lake Stage --------------------------------------------------------------
dates=date.fun(c("2015-01-01","2024-12-31"))
comp.dbkey=data.frame(DBKEY=c("N3466","06832"),Priority=c("P2","P1"))

DBHYDRO.meta.byDBKEY(comp.dbkey$DBKEY)

stg.da=data.frame()
for(i in 1:nrow(comp.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],comp.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(comp.dbkey$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,comp.dbkey,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

LakeO.xtab=dcast(stg.da,DATE~Priority,value.var="Data.Value",mean)
LakeO.xtab$Mean=with(LakeO.xtab,ifelse(is.na(P1)==T,P2,P1))



# Estuary Discharges ------------------------------------------------------


cal.flow=data.frame(DBKEY=c("DJ235","88280","DJ237","00865"),
                    STATION=c(rep("S77",2),rep("S79",2)),priority=rep(c("P1","P2"),2),Region="CRE")
SLE.dbkeys=data.frame(STATION=c("GORDY","S49","S48","S80","S308","S308.DS"),
                      DBKEY=c("91295","91607","91606","JW224","DJ239","06548"),
                      priority="P1",Region="SLE")
est.flow.dbkey=rbind(cal.flow,SLE.dbkeys)

est.qdat=data.frame()
for(i in 1:nrow(est.flow.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],est.flow.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(est.flow.dbkey$DBKEY[i])
  est.qdat=rbind(tmp,est.qdat)
  print(i)
}

est.qdat$Date=date.fun(est.qdat$Date)

est.qdat$WY=WY(est.qdat$Date)
est.qdat=merge(est.qdat ,est.flow.dbkey,"DBKEY")
est.qdat.xtab=dcast(est.qdat,STATION+Region+Date+WY~priority,value.var="Data.Value",mean,na.rm=T)
est.qdat.xtab$Data.Value=with(est.qdat.xtab,ifelse(is.na(P1)==T,P2,P1))
est.qdat.xtab=dcast(est.qdat.xtab,Date+WY~STATION,value.var="Data.Value",mean)

est.qdat.xtab$hydro.day=hydro.day(est.qdat.xtab$Date)
est.qdat.xtab$DOY=as.numeric(format(est.qdat.xtab$Date,"%j"))
est.qdat.xtab$CY=as.numeric(format(est.qdat.xtab$Date,"%Y"))
est.qdat.xtab$S77=with(est.qdat.xtab,ifelse(S77<0,0,S77))
est.qdat.xtab$C43=with(est.qdat.xtab,ifelse(S79<S77,0,S79-S77))
est.qdat.xtab$LOK.CRE=apply(est.qdat.xtab[,c("S77","S79")],1,min,na.rm=T)

est.qdat.xtab$S308_comp=with(est.qdat.xtab,ifelse(is.na(S308)==T,S308.DS,S308))
est.qdat.xtab$S308_out=with(est.qdat.xtab,ifelse(S308_comp<0,0,S308_comp))
est.qdat.xtab$NorthFork=rowSums(est.qdat.xtab[,c("GORDY","S49","S48")],na.rm=T)
est.qdat.xtab$SLE.tot=rowSums(est.qdat.xtab[,c("GORDY","S49","S48","S80")],na.rm=T)

est.qdat.xtab$C44Basin=with(est.qdat.xtab,S80-S308_comp)# with(est.qdat.xtab,ifelse(S80<S308_comp,0,S80-S308_comp))
est.qdat.xtab$LOK.SLE=apply(est.qdat.xtab[,c("S308_out","S80")],1,min,na.rm=T)

est.qdat.xtab$cum.S77=with(est.qdat.xtab,ave(cfs.to.acftd(S77),CY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
est.qdat.xtab$cum.S79=with(est.qdat.xtab,ave(cfs.to.acftd(S79),CY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
est.qdat.xtab$cum.S80=with(est.qdat.xtab,ave(cfs.to.acftd(S80),CY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
est.qdat.xtab$cum.S308=with(est.qdat.xtab,ave(cfs.to.acftd(S308),CY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
est.qdat.xtab$cum.S308pos=with(est.qdat.xtab,ave(cfs.to.acftd(S308),CY,FUN = function(x)cumsum(ifelse(is.na(x),0,ifelse(x<0,0,x)))))

plot(S80~Date,subset(est.qdat.xtab,CY==2016),type="b")
plot(LOK.SLE~Date,subset(est.qdat.xtab,CY==2016),type="b")

plot(S79~Date,subset(est.qdat.xtab,CY==2016),type="b")
plot(LOK.CRE~Date,subset(est.qdat.xtab,CY==2016),type="b")

cyano_area$CY=as.numeric(format(cyano_area$date,'%Y'))


## 2016 --------------------------------------------------------------------
# png(filename=paste0(plot.path,"2016_EstuaryDischarges.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3.75,0.5,3),oma=c(2,1,1,1),lwd=0.5);
layout(matrix(1:3,3,1,byrow=F))

yaxis.lab.ln=3
xlim.val=date.fun(paste(2016,c("01-01","12-31"),sep="-"));xmaj=seq(xlim.val[1],xlim.val[2],"1 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,100);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(bloom.area.per~date,cyano_area,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(bloom.area.per~date,cyano_area,lty=1,col="darkolivegreen4")
with(subset(cyano_area,CY==2016),shaded.range(date,rep(0,length(date)),bloom.area.per,"darkolivegreen1",lty=0))
xx.val=date.fun(c("2016-01-01","2016-04-24"))
yy.min=c(0,0);yy.max=c(100,100)
shaded.range(xx.val,yy.min,yy.max,"grey",lty=0)
text(xx.val[1]+diff(xx.val)/2,50,"No Data")
mtext(side=3,adj=0,"Lake Okeechobee")
mtext(side=3,adj=1,"2016")
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Algae Coverage (%)")
par(new=T)
ylim.val=c(10,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Mean~DATE,LakeO.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
lines(Mean~DATE,LakeO.xtab,lwd=1.25)
axis_fun(4,ymaj,ymin,ymaj)
mtext(side=4,line=2,"Stage (Ft, NGVD29)")
legend("topright",legend=c("Algae Cover","Lake Stage"),
       lty=c(NA,1),lwd=c(0.1,2),col=c("darkolivegreen4","black"),
       pch=c(22,NA),pt.bg=c(adjustcolor("darkolivegreen1",0.25),NA),pt.cex=1.5,
       ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)


ylim.val=c(0,15000);by.y=2500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(S79~Date,est.qdat.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(S79~Date,est.qdat.xtab,lty=1,col="dodgerblue1")
lines(LOK.CRE~Date,est.qdat.xtab,lty=1,col="indianred1")
with(subset(est.qdat.xtab,CY==2016),shaded.range(Date,rep(0,length(Date)),LOK.CRE,"indianred1",lty=0))
legend("topright",legend=c("Total S79","from Lake"),
       lty=c(1),lwd=c(2),col=c("dodgerblue1","indianred1"),
       pch=NA,pt.bg=NA,pt.cex=1,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
abline(v=date.fun("2016-01-30"),lty=2,lwd=2)
mtext(side=3,adj=0,"Caloosahatchee River Estuary")
# mtext(side=3,adj=1,"2016")
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Discharge (ft\u00B3 s\u207B\u00B9)")

ylim.val=c(0,5000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(S80~Date,est.qdat.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(S80~Date,est.qdat.xtab,lty=1,col="dodgerblue1")
lines(LOK.SLE~Date,est.qdat.xtab,lty=1,col="indianred1")
with(subset(est.qdat.xtab,CY==2016),shaded.range(Date,rep(0,length(Date)),LOK.SLE,"indianred1",lty=0))
lines(S80~Date,est.qdat.xtab,lty=1,col="dodgerblue1");# added to re-draw the lines
legend("topright",legend=c("Total S80","from Lake"),
       lty=c(1),lwd=c(2),col=c("dodgerblue1","indianred1"),
       pch=NA,pt.bg=NA,pt.cex=1,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
abline(v=date.fun("2016-01-30"),lty=2,lwd=2)
mtext(side=3,adj=0,"St Lucie River Estuary")
# mtext(side=3,adj=1,"2016")
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Discharge (ft\u00B3 s\u207B\u00B9)")
mtext(side=1,line=2,"Month (2016)")
dev.off()


# png(filename=paste0(plot.path,"2016_EstuaryDischarges_cumplot.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.5,0.5),oma=c(2,1,1,0.5),lwd=0.5);
layout(matrix(1:2,2,1,byrow=F))

yaxis.lab.ln=0
xlim.val=c(0,365);by.x=60;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,300e4);by.y=100e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(cum.S79~DOY,est.qdat.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(cum.S79~DOY,subset(est.qdat.xtab,CY==2016),lty=1,col="dodgerblue1",lwd=1.5)
lines(cum.S77~DOY,subset(est.qdat.xtab,CY==2016),lty=1,col="indianred1",lwd=1.5)
legend("bottomright",legend=c("S79","S77"),
       lty=c(1),lwd=c(2),col=c("dodgerblue1","indianred1"),
       pch=NA,pt.bg=NA,pt.cex=1,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=3,adj=0,"Caloosahatchee River Estuary")
mtext(side=3,adj=1,"2016")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj/1e3);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Discharge (x1000 AcFt)",outer=T)

ylim.val=c(0,100e4);by.y=25e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cum.S79~DOY,est.qdat.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(cum.S80~DOY,subset(est.qdat.xtab,CY==2016),lty=1,col="dodgerblue1",lwd=1.5)
lines(cum.S308pos~DOY,subset(est.qdat.xtab,CY==2016),lty=1,col="indianred1",lwd=1.5)
legend("bottomright",legend=c("S80","S308"),
       lty=c(1),lwd=c(2),col=c("dodgerblue1","indianred1"),
       pch=NA,pt.bg=NA,pt.cex=1,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=3,adj=0,"St Lucie River Estuary")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj/1e3);box(lwd=1)
mtext(side=1,line=2,"Day of Year")
dev.off()


## Annual Loop --------------------------------------------------------------------
yr.vals=2017:2024
for(i in 1:length(yr.vals)){
png(filename=paste0(plot.path,yr.vals[i],"_EstuaryDischarges.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
  par(family="serif",mar=c(2,3.75,0.5,3),oma=c(2,1,1,1),lwd=0.5);
  layout(matrix(1:3,3,1,byrow=F))

yaxis.lab.ln=3
xlim.val=date.fun(paste(yr.vals[i],c("01-01","12-31"),sep="-"));xmaj=seq(xlim.val[1],xlim.val[2],"1 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,100);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(bloom.area.per~date,cyano_area,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(bloom.area.per~date,cyano_area,lty=1,col="darkolivegreen4")
with(subset(cyano_area,CY==yr.vals[i]),shaded.range(date,rep(0,length(date)),bloom.area.per,"darkolivegreen1",lty=0))
# xx.val=date.fun(c("2016-01-01","2016-04-24"))
# yy.min=c(0,0);yy.max=c(100,100)
# shaded.range(xx.val,yy.min,yy.max,"grey",lty=0)
# text(xx.val[1]+diff(xx.val)/2,50,"No Data")
mtext(side=3,adj=0,"Lake Okeechobee")
mtext(side=3,adj=1,yr.vals[i])
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Algae Coverage (%)")
par(new=T)
ylim.val=c(10,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Mean~DATE,LakeO.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
lines(Mean~DATE,LakeO.xtab,lwd=1.25)
axis_fun(4,ymaj,ymin,ymaj)
mtext(side=4,line=2,"Stage (Ft, NGVD29)")
legend("topright",legend=c("Algae Cover","Lake Stage"),
       lty=c(NA,1),lwd=c(0.1,2),col=c("darkolivegreen4","black"),
       pch=c(22,NA),pt.bg=c(adjustcolor("darkolivegreen1",0.25),NA),pt.cex=1.5,
       ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)



ylim.val=c(0,15000);by.y=2500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(S79~Date,est.qdat.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(S79~Date,est.qdat.xtab,lty=1,col="dodgerblue1")
lines(LOK.CRE~Date,est.qdat.xtab,lty=1,col="indianred1")
with(subset(est.qdat.xtab,CY==yr.vals[i]),shaded.range(Date,rep(0,length(Date)),LOK.CRE,"indianred1",lty=0))
legend("topright",legend=c("Total S79","from Lake"),
       lty=c(1),lwd=c(2),col=c("dodgerblue1","indianred1"),
       pch=NA,pt.bg=NA,pt.cex=1,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
# abline(v=date.fun("2016-01-30"),lty=2,lwd=2)
mtext(side=3,adj=0,"Caloosahatchee River Estuary")
# mtext(side=3,adj=1,"2016")
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Discharge (ft\u00B3 s\u207B\u00B9)")

ylim.val=c(0,5000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(S80~Date,est.qdat.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(S80~Date,est.qdat.xtab,lty=1,col="dodgerblue1")
lines(LOK.SLE~Date,est.qdat.xtab,lty=1,col="indianred1")
with(subset(est.qdat.xtab,CY==yr.vals[i]),shaded.range(Date,rep(0,length(Date)),LOK.SLE,"indianred1",lty=0))
lines(S80~Date,est.qdat.xtab,lty=1,col="dodgerblue1");# added to re-draw the lines
legend("topright",legend=c("Total S80","from Lake"),
       lty=c(1),lwd=c(2),col=c("dodgerblue1","indianred1"),
       pch=NA,pt.bg=NA,pt.cex=1,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
# abline(v=date.fun("2016-01-30"),lty=2,lwd=2)
mtext(side=3,adj=0,"St Lucie River Estuary")
# mtext(side=3,adj=1,"2016")
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5);box(lwd=1)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=yaxis.lab.ln,"Discharge (ft\u00B3 s\u207B\u00B9)")
mtext(side=1,line=2,paste0("Month (",yr.vals[i],")"))
dev.off()
}




# -------------------------------------------------------------------------
LORS=read.csv("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/LORS.csv")
LORS$DATE=with(LORS,date.fun(paste(2024,Month,Day,sep="-")))

LakeO.xtab.2024=merge(subset(LakeO.xtab,as.numeric(format(DATE,"%Y"))==2024),LORS,'DATE',all.x=T)

head(LakeO.xtab.2024)                      
with(LakeO.xtab.2024,ifelse(Mean>Low,1,0))

LakeO.xtab.2024$x_into_inter=with(LakeO.xtab.2024,ifelse(Mean>Low,1,0))


ylim.val=c(10,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Mean~DATE,LakeO.xtab,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,lwd=0.5,col="grey")
lines(Mean~DATE,LakeO.xtab,lwd=1.25,col="red")
with(LORS,lines(High~DATE,lwd=1.5,col="black"))
with(LORS,lines(Intermediate~DATE,lwd=1.5,col="black"))
with(LORS,lines(Low~DATE,lwd=1.5,col="black"))
with(LORS,lines(BaseFlow~DATE,lwd=1.5,col="black"))
with(LORS,lines(BeneficialUse~DATE,lwd=1.5,col="black"))
with(LORS,lines(WSM~DATE,lwd=1.5,col="grey"))
with(LORS,lines(Inter1ft~DATE,lwd=1.5,lty=5,col="black"))
with(LORS,lines(LowLow~DATE,lwd=1.5,lty=5,col="grey"))
with(LORS,lines(LowMid~DATE,lwd=1.5,lty=5,col="grey"))

points(date.fun("2024-01-15"),subset(LakeO.xtab.2024,DATE==date.fun("2024-01-15"))$Mean,pch=21,bg="red")



# Flows south -------------------------------------------------------------

eaa.flow.dbkey=data.frame(SITE=c("S354","S351","S352","S271",
                                 "G251","G310","S7","S150","S8"),
                          DBKEY=c("91513","91508","91510","02855",
                                  "90934","90973","91681","91395","91688"),
                          region=c(rep("inflow",4),
                                   rep("outflow",5))
)
eaa.flow.dbkey=subset(eaa.flow.dbkey,SITE!="S271")

qdat=data.frame()
for(i in 1:nrow(eaa.flow.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],eaa.flow.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(eaa.flow.dbkey$DBKEY[i])
  qdat=rbind(tmp,qdat)
  print(i)
}

qdat$Date=date.fun(qdat$Date)
range(qdat$Data.Value,na.rm=T)

subset(qdat,Data.Value<0)
qdat$Q.pos=with(qdat,ifelse(Data.Value<0,0,Data.Value))

qdat$WY=WY(qdat$Date)
qdat$hydro.day=hydro.day(qdat$Date)
qdat$DOY=as.numeric(format(qdat$Date,"%j"))
qdat$CY=as.numeric(format(qdat$Date,"%Y"))
qdat=merge(qdat,eaa.flow.dbkey,"DBKEY")


ddply(qdat,c("SITE"),summarise,min.date=min(Date),max.date=max(Date))

CY.Tot.Q=dcast(qdat,CY~region,value.var = "Q.pos",fun.aggregate = function(x) sum(cfs.to.acftd(x)/1000,na.rm=T))
with(CY.Tot.Q,outflow/inflow)

plot(inflow~CY,CY.Tot.Q)
points(outflow~CY,CY.Tot.Q,pch=21,bg="Red")

WY.Tot.Q=dcast(qdat,WY~region,value.var = "Data.Value",fun.aggregate = function(x) sum(cfs.to.acftd(x)/1000,na.rm=T))
plot(inflow~WY,WY.Tot.Q)
points(outflow~WY,WY.Tot.Q,pch=21,bg="Red")
WY.Tot.Q


qdat.xtab=dcast(qdat,STATION+Region+Date+WY~priority,value.var="Data.Value",mean,na.rm=T)
qdat.xtab$Data.Value=with(qdat.xtab,ifelse(is.na(P1)==T,P2,P1))
qdat.xtab=dcast(qdat.xtab,Date+WY~STATION,value.var="Data.Value",mean)

qdat.xtab$hydro.day=hydro.day(qdat.xtab$Date)
qdat.xtab$DOY=as.numeric(format(qdat.xtab$Date,"%j"))
qdat.xtab$CY=as.numeric(format(qdat.xtab$Date,"%Y"))