
############################################################
# Water Balance mutlibucket(unsaturated) DEPI
# Niranjan Wimalathunge
# date: 2016-01-05
##########################################################
#Clear the workspace.
rm(list = ls())

# load required libraries
library(raster)
library(RCurl)
library(rgdal)
library(downloader)
library(stringr)
library(date)
library(ggplot2)

# calculate theta for each level
setwd('X:/theta/SOIL')
tiffs <- list.files(pattern= '*.tif')
x2 <- stack(tiffs[1])
for (a in 2:24){ 
  x2 <- addLayer(x2,raster(tiffs[a]))}

b <- as(extent(141.0,149.0,-39.0, -34.0), 'SpatialPolygons')
crs(b) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
out <- crop(x2,b,snap="out")


# extract value from rasterstack with xy coordinates
xy = matrix(c(142.553988, 142.895719, 142.455425, 143.295554, 144.604781, 142.234669, 143.95275,
              142.814326, 143.732672, 142.421236, 144.108267, 146.976569, 142.427738, 142.364371, 141.614091,
              145.748277, 146.8745076, 146.8602172, 146.9493241, 147.0618367, 146.7516579, 146.9482283,
              147.7071187, 147.6231003, 147.762109, 147.1531321, 148.5148428, 148.2542173, 147.5069297,
              146.6986186, 147.1984246, 147.149446, 147.5316272, 147.2681658, -36.25634, -35.978836, -36.126991,
              -36.402317, -36.425082, -37.775396, -36.425082, -37.732813, -35.829157, -35.066535, -36.470131,
              -38.354332, -35.413082, -36.739474, -34.454351, -36.141618, -35.10358457, -35.26621555, -34.28951458,
              -34.79356113, -34.69986394, -34.28862941, -34.74213748, -34.77417274, -34.61314584, -34.31873676,
              -34.44244631, -34.01303105, -34.49034762, -35.26934136, -34.2482448, -34.38298771, -34.41712445, -34.67901969),
            nrow = 34, ncol = 2)

vals<-extract(out,xy)
vals1<-data.frame(xy[,1],xy[,2],vals)
write.table(vals1,"allsoilvalues.txt")

setwd('X:/Old/theta')
theta<- read.table("allsoilvalues.csv",sep=",",quote="",header=F)
names(theta)<-c('x','y','BDW_005','BDW_015','BDW_030','BDW_060','BDW_100','BDW_200','CLY_005','CLY_015','CLY_030','CLY_060','CLY_100','CLY_200','SLT_005','SLT_015','SLT_030','SLT_060','SLT_100','SLT_200','SND_005','SND_015','SND_030','SND_060','SND_100','SND_200') 
#write.table(theta,file="allsoilvalues.csv",sep=",",quote=F,row.names=F, col.names=T)
clay30<-(theta$CLY_005+theta$CLY_015+theta$CLY_030)/3
clay100<-(theta$CLY_060+theta$CLY_100)/2


theta_005<-(0.1958 *tanh ((-0.0167* theta$CLY_005 - 0.0259 *theta$SND_005) + 0.5587 *theta$BDW_005
                          + 1.86) - 0.4692 *tanh (((-0.0074* theta$CLY_005 - 0.0061* theta$SND_005) + 0.9869 *theta$BDW_005)
                                                  - 1.47)) + 0.0063* tanh ((-0.0653 *theta$CLY_005 - 0.0063* theta$SND_005 - 5.30 *theta$BDW_005)
                                                                           + 9.40) + 0.0495

theta_015<-(0.1958 *tanh ((-0.0167* theta$CLY_015 - 0.0259 *theta$SND_015) + 0.5587 *theta$BDW_015
                          + 1.86) - 0.4692 *tanh (((-0.0074* theta$CLY_015 - 0.0061* theta$SND_015) + 0.9869 *theta$BDW_015)
                                                  - 1.47)) + 0.0063* tanh ((-0.0653 *theta$CLY_015 - 0.0063* theta$SND_015 - 5.30 *theta$BDW_015)
                                                                           + 9.40) + 0.0495

theta_030<-(0.1958 *tanh ((-0.0167* theta$CLY_030 - 0.0259 *theta$SND_030) + 0.5587 *theta$BDW_030
                          + 1.86) - 0.4692 *tanh (((-0.0074* theta$CLY_030 - 0.0061* theta$SND_030) + 0.9869 *theta$BDW_030)
                                                  - 1.47)) + 0.0063* tanh ((-0.0653 *theta$CLY_030 - 0.0063* theta$SND_030 - 5.30 *theta$BDW_030)
                                                                           + 9.40) + 0.0495

theta_060<-(0.1958 *tanh ((-0.0167* theta$CLY_060 - 0.0259 *theta$SND_060) + 0.5587 *theta$BDW_060
                          + 1.86) - 0.4692 *tanh (((-0.0074* theta$CLY_060 - 0.0061* theta$SND_060) + 0.9869 *theta$BDW_060)
                                                  - 1.47)) + 0.0063* tanh ((-0.0653 *theta$CLY_060 - 0.0063* theta$SND_060 - 5.30 *theta$BDW_060)
                                                                           + 9.40) + 0.0495

theta_100<-(0.1958 *tanh ((-0.0167* theta$CLY_100 - 0.0259 *theta$SND_100) + 0.5587 *theta$BDW_100
                          + 1.86) - 0.4692 *tanh (((-0.0074* theta$CLY_100 - 0.0061* theta$SND_100) + 0.9869 *theta$BDW_100)
                                                  - 1.47)) + 0.0063* tanh ((-0.0653 *theta$CLY_100 - 0.0063* theta$SND_100 - 5.30 *theta$BDW_100)
                                                                           + 9.40) + 0.0495

theta_200<-(0.1958 *tanh ((-0.0167* theta$CLY_200 - 0.0259 *theta$SND_200) + 0.5587 *theta$BDW_200
                          + 1.86) - 0.4692 *tanh (((-0.0074* theta$CLY_200 - 0.0061* theta$SND_200) + 0.9869 *theta$BDW_200)
                                                  - 1.47)) + 0.0063* tanh ((-0.0653 *theta$CLY_200 - 0.0063* theta$SND_200 - 5.30 *theta$BDW_200)
                                                                           + 9.40) + 0.0495

rootzone_bucket<-theta_005*50+theta_015*100+theta_030*150+theta_060*300+theta_100*400

bucketSize <-data.frame(theta_005*50,theta_015*100,theta_030*150,theta_060*300,theta_100*400,theta_200*1000,rootzone_bucket)
thetaCheck <-data.frame(theta_005,theta_015,theta_030,theta_060,theta_100,theta_200,rootzone_bucket)
aveBucketSize<-ave(bucketSize$rootzone_bucket,FUN = mean)

ET<- as.matrix(read.table("X:/theta/DEPI/silo/dpiET_daily.csv",sep=",",quote="",header=F))
Rain<-as.matrix(read.table("X:/theta/DEPI/silo/outRain.csv",sep=",",quote="",header=F))
MaxT<-as.matrix(read.table("X:/theta/DEPI/silo/outMax.csv",sep=",",quote="",header=F))
MinT<-as.matrix(read.table("X:/theta/DEPI/silo/outMin.csv",sep=",",quote="",header=F))

############################################
# water balance
############################################

result<- array(rep(1, 14*5479*16), dim=c(16, 5479, 14))

days <- seq(from=as.Date('2000-01-01'), to=as.Date("2014-12-31"),by='days' )

for(i in seq_len(dim(result)[1])){
  a=3
  SM=0;SMA=c(0,0,0);SMB=c(0,0,0);SMC=c(0,0,0);SMD=c(0,0,0);SME=c(0,0,0);runoff=0; DeepD=0
  
  for(j in seq_len(dim(result)[2])){
    
    SMA[2]=SMA[1]*.8
    SMA[1]=SMA[1]*.2
    
    SMA[1] = SMA[1]+Rain[i,a]-0.5*ET[i,a]*0.125
    if (SMA[1] < 0){SMA[1]=0}
    if (SMA[1]> bucketSize[i,1]){SMB[1]= SMB[1]+(SMA[1]-bucketSize[i,1]);SMA[1]=bucketSize[i,1];print('bucket a is full')}
    
    SMB[2]=SMB[1]*.05
    SMB[1]=SMB[1]*.95
    
    SMB[1] = SMB[1]+SMA[3]-0.2*ET[i,a]*0.125
    if (SMB[1] < 0){SMB[1]=0}
    if (SMA[1]==bucketSize[i,1] & SMB[1]>bucketSize[i,2]){runoff= SMB[1]-bucketSize[i,2];print('buckets ab are full')}
    if (SMB[1]> bucketSize[i,2]){SMC[1]= SMC[1]+(SMB[1]-bucketSize[i,2]);SMB[1]=bucketSize[i,2];print('bucket b is full')}
    
    SMC[2]=SMC[1]*.05
    SMC[1]=SMC[1]*.95
    
    SMC[1] = SMC[1]+SMB[3]-0.15*ET[i,a]*0.125
    if (SMC[1] < 0){SMC[1]=0}
    if (SMC[1]> bucketSize[i,3]){SMD[1]= SMD[1]+(SMC[1]-bucketSize[i,3]);SMC[1]=bucketSize[i,3];print('smc is full')}
    
    SMD[2]=SMD[1]*.01
    SMD[1]=SMD[1]*.99
    
    SMD[1] = SMD[1]+ SMC[3]-0.10*ET[i,a]*0.125
    if (SMD[1] < 0){SMD[1]=0}
    if (SMD[1]> bucketSize[i,4]){SME[1]= SME[1]+(SMD[1]-bucketSize[i,4]);SMD[1]=bucketSize[i,4];print('smd is full')}
    
    SME[2]=SME[1]*.01
    SME[1]=SME[1]*.99
    
    SME[1] = SME[1]+ SMD[3]-0.05*ET[i,a]*0.125
    if (SME[1] < 0){SME[1]=0}
    
    #if (SME[1]> bucketSize[i,5]){DeepD = DeepD+(SME[1]-bucketSize[i,5]);SME[1]=bucketSize[i,5];print('sme is full')}
    if (SME[1]> bucketSize[i,5]){DeepD = (SME[1]-bucketSize[i,5]);SME[1]=bucketSize[i,5];print('sme is full')}
    
    
    DeepD = DeepD +SME[3]  #lost soil moisture (60-100cm(SME)) due to deep drainage
    SM=SMA[1]+SMB[1]+SMC[1]+SMD[1]+SME[1]
    
    SMA[3]=SMA[2]
    SMB[3]=SMB[2]
    SMC[3]=SMC[2]  
    SMD[3]=SMD[2]
    SME[3]=SME[2]
    
    result[i,j,1] = as.character(days[j])
    #result[i,j,1] = days[j]
    result[i,j,2] = SM
    result[i,j,3] = Rain[i,a]
    result[i,j,4] = ET[i,a]
    result[i,j,5] = Rain[i,a]-ET[i,a] 
    result[i,j,6] = MaxT[i,a]
    result[i,j,7] = MinT[i,a]
    result[i,j,8] = runoff
    result[i,j,9] = DeepD
    result[i,j,10] = SMA[1] 
    result[i,j,11] = SMB[1]
    result[i,j,12] = SMC[1]
    result[i,j,13] = SMD[1]
    result[i,j,14] = SME[1]
    
    runoff = 0
    DeepD = 0
    a=a+1
  }}

for (i in 1:16){

  write.table(result[i,,], file= paste("DEPI_site",i,"csv",sep="."), sep=",",row.names=F, col.names=T)
}
########################################################
# Calculate the correlation
#######################################################
#FL1
Dat1=read.csv("X:/theta/DEPI/Bangerang Soil Moisture{150917_164621}.csv", header=F,stringsAsFactors = F)
WB1<-read.csv("DEPI_site.1.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="",y="Correlation Soil Water Balance(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
#moist1$weighted_ave=moist1$cm_22*0.47+moist1$cm_72*0.45+moist1$cm_112*0.20 # soil moisture weighted average
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor1<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor1
WB1m$Site<-"FL1"
WB <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
########################
#FL2
Dat1=read.csv("X:/theta/DEPI/Birchip soil moisture indivual sensors{150917_164940}.csv", header=F)
WB1<-read.csv("DEPI_site.2.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)

pcor2<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor2

WB1m$Site<-"FL2"
WB2 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
#####################################
#FL3
Dat1=read.csv("X:/theta/DEPI/Brim Moisture{150917_164335}.csv")
WB1<-read.csv("DEPI_site.3.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)", header=F)

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor3<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor3

WB1m$Site<-"FL3"
WB3 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
#####################################
#FL4
Dat1=read.csv("X:/theta/DEPI/Coonooer Bridge Soil Moisture{150917_165044}.csv", header=F)
WB1<-read.csv("DEPI_site.4.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)

pcor4<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor4

WB1m$Site<-"FL4"
WB4 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
########################
#FL5
Dat1=read.csv("X:/theta/DEPI/Elmore Soil Moisture{150917_165526}.csv", header=F)
WB1<-read.csv("DEPI_site.5.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor5<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor5

WB1m$Site<-"FL5"
WB5 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
#################################################################
#FL6
Dat1=read.csv("X:/theta/DEPI/Hamilton Moisture{150917_164835}.csv", header=F)
WB1<-read.csv("DEPI_site.6.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() +labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-0

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor6<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor6

WB1m$Site<-"FL6"
WB6 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL7
Dat1=read.csv("X:/theta/DEPI/Kerang ICC irrigation block{150917_165347}.csv", header=F)
WB1<-read.csv("DEPI_site.7.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor7<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor7

WB1m$Site<-"FL7"
WB7 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL8
Dat1=read.csv("X:/theta/DEPI/Lake Bolac{150917_165152}.csv", header=F)
WB1<-read.csv("DEPI_site.8.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor8<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor8

WB1m$Site<-"FL8"
WB8 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL9
Dat1=read.csv("X:/theta/DEPI/Normanville Moisture{150917_165250}.csv", header=F)
WB1<-read.csv("DEPI_site.9.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor9<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor9

WB1m$Site<-"FL9"
WB9 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL10
Dat1=read.csv("X:/theta/DEPI/Ouyen seperate soil moisture{150917_163855}.csv", header=F)
WB1<-read.csv("DEPI_site.10.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor10<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor10

WB1m$Site<-"FL10"
WB10 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL11
Dat1=read.csv("X:/theta/DEPI/Raywood Moisture{150917_165438}.csv", header=F)
WB1<-read.csv("DEPI_site.11.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor11<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor11

WB1m$Site<-"FL11"
WB11 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL12
Dat1=read.csv("X:/theta/DEPI/Sale Moisture{150917_170134}.csv", header=F)
WB1<-read.csv("DEPI_site.12.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor12<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor12

WB1m$Site<-"FL12"
WB12 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL13
Dat1=read.csv("X:/theta/DEPI/Speed Moisture{150917_164135}.csv", header=F)
WB1<-read.csv("DEPI_site.13.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() +labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+1)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor13<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor13

WB1m$Site<-"FL13"
WB13 <- WB1m
deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
##################################################
#FL14
Dat1=read.csv("X:/theta/DEPI/Taylors Lake Soil Moisture{150917_164446}.csv", header=F)
WB1<-read.csv("DEPI_site.14.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor14<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor14

WB1m$Site<-"FL14"
WB14 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
###############################################################
#FL15
Dat1=read.csv("X:/theta/DEPI/Werrimul Moisture{150911_153530}.csv", header=F)
WB1<-read.csv("DEPI_site.15.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor15<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")

pcor15

WB1m$Site<-"FL15"
WB15 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
###############################################################
#FL16
Dat1=read.csv("X:/theta/DEPI/Youanmite Moisture{150917_165822}.csv", header=F)
WB1<-read.csv("DEPI_site.16.csv",stringsAsFactors = FALSE)
ggplot(WB1, aes(as.Date(V1), V2)) + geom_line() + labs(x="Year",y="Soil moisture(mm/day)")

#WB Model
names(WB1)<-c('Date','SM','Rain','ET','Delta','MaxT','MinT','Runoff','DeepD','sl005cm','sl015cm','sl030cm','sl060cm','sl100cm')
WB1$Date=as.Date(WB1$Date)

WB1<-wbc(WB1) # add discount factors

#Sensor Data
names(Dat1)=c("date","time","cm_30","cm_40","cm_50","cm_60","cm_70","cm_80","cm_90","cm_100","rain")
Dat1$date=as.Date(Dat1$date, format= "%d/%m/%Y")
Dat1$days=as.numeric(Dat1$date)

Dat1<-Dat1[Dat1[,5]>0,]
#Number of sensors(ns)+rain gauges(1)
ns<-8
rg<-1

#Compute daily means for moisture for each depth
moist1<-aggregate(cbind(cm_30,cm_40,cm_50,cm_60,cm_70,cm_80,cm_90,cm_100,rain)~date,data=Dat1,FUN="mean")

#Calculate profile mean
moist1$mean<-rowMeans(moist1[,2:(ns+rg)],na.rm=TRUE)
moist1$weighted_av100=moist1$cm_30*0.35+moist1$cm_40*0.1+moist1$cm_50*0.1+moist1$cm_60*0.1+moist1$cm_70*0.1+moist1$cm_80*0.1+moist1$cm_90*0.1+moist1$cm_100*0.05 # soil moisture weighted average

#Combine data
WB1m <-merge(moist1,WB1,by.x="date",by.y="Date",all.x=T)
pcor16<-cor(WB1m[,2:(ns+rg+3)],WB1m[,(ns+rg+4):(ncol(WB1m))],use="pairwise.complete.obs")
pcor16

WB1m$Site<-"FL16"
WB16 <- WB1m

deepM=WB1m$cm_60*0.05+WB1m$cm_70*0.1+WB1m$cm_80*0.1+WB1m$cm_90*0.1+WB1m$cm_100*0.05 # soil moisture weighted average
subsoilM=WB1m$cm_30*0.17+WB1m$cm_40*0.1+WB1m$cm_50*0.1+WB1m$cm_60*0.05
topsoilM=WB1m$cm_30
sl030cm<-WB1m$sl005cm + WB1m$sl015cm + WB1m$sl030cm
SumM<-data.frame(topsoilM,subsoilM,deepM,sl030cm,WB1m$sl060cm,WB1m$sl100cm)
corSumM1<-cor(WB1m[,c(2,4,7)],WB1m[,c(23,24,25)],use="pairwise.complete.obs")
corSumM2<-cor(SumM[,1:3],SumM[,4:6],use="pairwise.complete.obs")
corSumM2
######################################################
#Stack data into one
######################################################

#1 and 2
commonCols <- intersect(names(WB), names(WB2)) 
commonCols 
data_ave<-rbind(WB[commonCols], WB2[commonCols]) 

#and 3
commonCols <- intersect(names(data_ave), names(WB3)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB3[commonCols]) 

#and 4
commonCols <- intersect(names(data_ave), names(WB4)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB4[commonCols]) 

#and 5
commonCols <- intersect(names(data_ave), names(WB5)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB5[commonCols]) 

#and 6
commonCols <- intersect(names(data_ave), names(WB6)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB6[commonCols]) 

#and 7
commonCols <- intersect(names(data_ave), names(WB7)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB7[commonCols]) 

#and 8
commonCols <- intersect(names(data_ave), names(WB8)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB8[commonCols]) 

#and 9
commonCols <- intersect(names(data_ave), names(WB9)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB9[commonCols]) 

#and 10
commonCols <- intersect(names(data_ave), names(WB10)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB10[commonCols]) 

#and 11
commonCols <- intersect(names(data_ave), names(WB11)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB11[commonCols]) 

#and 12
commonCols <- intersect(names(data_ave), names(WB12)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB12[commonCols]) 

#and 13
commonCols <- intersect(names(data_ave), names(WB13)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB13[commonCols]) 

#and 14
commonCols <- intersect(names(data_ave), names(WB14)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB14[commonCols]) 

#and 15
commonCols <- intersect(names(data_ave), names(WB15)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB15[commonCols]) 

#and 16
commonCols <- intersect(names(data_ave), names(WB16)) 
commonCols 
data_ave<-rbind(data_ave[commonCols], WB16[commonCols]) 


data_ave$Site<-as.factor(data_ave$Site)
##############################################

# Define a vector of months by cutting the month value out of the date


##############################################################################

#Read in spatial coordinates and merge with data

covariates<-readRDS("X:/theta/DEPI/covariatesDepi.Rda")

data_avexy<-merge(data_ave,covariates,all=TRUE,by.x="Site",by.y="SiteID")

#NA omit

pred_mbus<-na.omit(data_avexy)

#Fix up predictors

pred_mbus$Month<-as.factor(format(pred_mbus$date, "%m"))

#pred_mbus$Solar<-cos(pred_mbus$Aspect/(180/pi)) * tan(pred_mbus$Slope/(180/pi)) * 100
saveRDS(pred_mbus, file = paste("X:/theta/DEPI/","pred_mbusdepi.Rda", sep=""))

write.csv(pred_mbus,"pred_mbusDepi.csv")

##########################################################################
#Fit model- Random Forest
##########################################################################
library(randomForest)

rf.model1 <- randomForest(weighted_av100 ~ SM+Rain+ET+Delta+Runoff+DeepD+Temp+df50_rain+df70_rain +    
                          df90_rain+df95_rain+df99_rain+df999_rain+df50_et+df70_et+df90_et+df95_et+df99_et+df999_et+df50_temp+df70_temp+df90_temp+df95_temp+    
                          df99_temp+df999_temp+df50_delta+df70_delta+df90_delta+df95_delta+df99_delta+df999_delta+slope+aspect+SolarRadiation+soilOrderNo+PAWC+Month      
                          ,data=pred_mbus1, ntree = 300, importance = TRUE,do.trace = 20, proximity=TRUE)


print(rf.model1)

plot(rf.model1)

round(importance(rf.model1), 2)

varImpPlot(rf.model1)

#Get out-of-bag predictions
predict.rf<-rf.model1$predicted

###############################################################################
pred_mbus1<-pred_mbus[-c(3:11,19:25,55,57)]
pred_mbus1<-pred_mbus[-c(1:11,55,57)]
sites <- as.vector(unique(pred_mbus1$Site))  

mask1<-NULL
pre<- NULL
for (s in sites){
  print(s)
  mask <-subset(pred_mbus1, Site==s)
  pred2 <- subset(pred_mbus1, Site!=s)
  
  rf.fit  <- ranger(weighted_av100 ~.,data=pred2, num.trees=500,write.forest = TRUE)
  rf.pred <- predict(rf.fit, mask)
  
  mask1<- rbind(mask1,mask)
  pre <- c(pre,rf.pred$predictions)
}

mask1$predicted_ave<-pre

# aggregate yeild into site
predM<-aggregate(predicted_ave ~ Month + site ,mask1,FUN="mean")
obserM<-aggregate(weighted_av100 ~ Month + site ,mask1,FUN="mean")
######################
library(ranger)
#with the model
pred_mbus1<-pred_mbus[-c(2:11,13,48)]
l<-list("FL2","FL4","FL14")
pred_mbus1 <- pred_mbus1[!pred_mbus1$Site %in% l, ]

########

Sites <- as.vector(unique(pred_mbus1$Site))  

mask1<-NULL
pre<- NULL
for (s in Sites){
  print(s)
  mask <-subset(pred_mbus1, Site==s)
  pred2 <- subset(pred_mbus1, Site!=s)
  
  rf.fit  <- ranger(weighted_av100 ~.,data=pred2[-(which(names(pred2)=="Site"))], num.trees=200,write.forest = TRUE)
  rf.pred <- predict(rf.fit, mask)
  
  mask1<- rbind(mask1,mask)
  pre <- c(pre,rf.pred$predictions)
}

mask1$predicted_ave<-pre

#######################################################################
## Concordance correlation plot:
#######################################################################
library(epiR)

lins_con <-epi.ccc(mask1$predicted_ave,mask1$weighted_av100,ci = "z-transform",conf.level = 0.95)

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
lab <- paste("CCC: ", round(lins_con$rho.c[,1], digits = 2), " (95% CI ", 
             specify_decimal(lins_con$rho.c[,2], 2), " - ",
             specify_decimal(lins_con$rho.c[,3], 2), ")", sep = "")


z <- lm(mask1$predicted_ave~mask1$weighted_av100)


par(pty = "s")
plot(mask1$predicted_ave,mask1$weighted_av100,xlim = c(10, 70), ylim = c(10,70),xlab =expression(bold("Observed - Volumetric moisture")), 
     ylab =expression(bold("Predicted - Volumetric moisture")), col=rgb(0, 0, 0, 0.2), pch = 16,main="",font=2)
grid (NULL,NULL, lty = 6, col = "gray") 

abline(0,1,lwd = 2)


rmse<-sqrt(mean((mask1$predicted_ave-mask1$weighted_av100)^2))
legend(x = "topleft", legend = c("Concordance correlation",lab),bty = "n")
text(12,62, paste("RMSE =", round(rmse,4)))
###########################
library(Boruta)
modFitB <- Boruta(weighted_av100 ~.,data=pred_mbus[-c(1:11,19:25,48)], maxRuns = 15,doTrace = 20)# July model with Lawson Grains soil

print(modFitB)

boruta.train<-modFitB
plot(boruta.train, xlab = "", xaxt = "n",mai=c(7,2,3))
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = F,
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = .6)


text(1:ncol(boruta.train$ImpHistory), labels = names(Labels), srt=45, pos=1, xpd=T,offset = -2)

dev.off()


final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
boruta.df[order(boruta.df$meanImp,decreasing = T),]

class(boruta.df)
print(boruta.df)

vimp<-boruta.df[order(boruta.df$meanImp,decreasing = T),]
##################################
library(dplyr)
library(ggplot2)
library(caret)
par(mar=c(0,0,0,0))

var_importance=data.frame(variable =rownames(boruta.df),importance=as.vector(boruta.df[1]))
row.names(var_importance)<-NULL

# str(var_im)

var_importance <- var_importance[order(-var_importance$meanImp),]
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
rownames(var_importance) <- NULL

p <- ggplot(var_importance, aes(x=variable, weight=meanImp, fill=variable))
p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Variable") + ylab("Variable Importance")
p <- p + scale_fill_discrete(name="Variable Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=16))



########################################################################
# just the legend the accuracy and bias from bland and altman graph
legend(x = "topleft", legend = c(expression(bold("Accuracy = 3.5")), 
                                 expression(bold("Concordance = 0.91")), 
                                 expression(bold("Bias = 0.10")), 
                                 expression(bold("n = 13,356"))), bty = "n")


text(x = 15, y = 75, labels = lab)
############################################################################
## Bland and Altman plot (Figure 2 from Bland and Altman 1986):
############################################################################

tmp.mean <- mean(lins_con$blalt$delta)
tmp.sd <- sqrt(var(lins_con$blalt$delta))

plot(lins_con$blalt$mean, lins_con$blalt$delta, pch = 16, 
     xlab = "Average", 
     ylab = "Difference", xlim = c(0,70), 
     ylim = c(-40,40),
     main = "") 
abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")
legend(x = "topleft", legend = c("Mean difference", 
                                 "Mean difference +/-2SD "), lty = c(1,2), bty = "n")

text(10, 25, paste("Mean =", round(tmp.mean, 1), "\nSD =",round(tmp.sd, 1)))


#####################################################################
# lattice graph for correlation ET,Rainfall and Soil water balance
######################################################################
library(ggplot2)
library(reshape2)
library(lattice)
#correlation boxplots
cor<-read.csv("C:/Users/nman2690/Google Drive/multiBucket_unsaturated.csv")
my.theme <- list(
  box.umbrella = list(col = "black"),
  box.rectangle = list(col = "black"),
  plot.symbol   = list(col = "black"), #outlier size and color
  
  panel.background = element_rect(fill = "white"),
  axis.text.x = element_text(face="bold", color="black",size=14),
  axis.text.y = element_text(face="bold", color="black",size=14),
  axis.title=element_text(size=14,face="bold"),
  axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
  
  strip.text.x = element_text( face = 'bold',size = 14, colour = "black"))

panel.grid.minor = element_blank()

bwplot(Correlation~Depth |variable,    ## see the powerful conditional formula 
       data=cor,
       between=list(y=c(-1,1)),
       par.settings = my.theme,
       main="")
######################################################################################

# lattice graph for correlation single,multi(saturated)and multi(unsaturated) buckets
#correlation boxplots
#######################################################################################
cor<-read.csv("X:/theta/DEPI/AllBucketCorrelation1.csv")

#cor$variable=factor(cor$variable,levels=c("Rainfall","ET","Single bucket","Saturated bucket","Unsaturated bucket"))
cor$variable=factor(cor$variable,levels=c("Soil water balance (single bucket)","Soil water balance (multi bucket-saturated)","Soil water balance (multi bucket-unsaturated)","Rainfall","ET"))


my.theme <- list(
  box.umbrella = list(col = "black"),
  box.rectangle = list(col = "black"),
  plot.symbol   = list(col = "black"), #outlier size and color
  
  panel.background = element_rect(fill = "white"),
  axis.text.x = element_text(face="bold", color="black",size=14),
  axis.text.y = element_text(face="bold", color="black",size=14),
  axis.title=element_text(size=14,face="bold"),
  axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
  
  strip.text.x = element_text( face = 'bold',size = 14, colour = "black"))

panel.grid.minor = element_blank()

bwplot(Correlation~Depth |variable,    ## see the powerful conditional formula 
       data=cor,
       between=list(y=c(-1,1)),
       par.settings = my.theme,
       main="")

################################################################

cor<-read.csv("C:/Users/nman2690/Google Drive/excel files/subsoil_corr.csv")

cor$Depth=factor(cor$Depth,levels=c("cm_22","cm_52","cm_72"))

cor<-read.csv("X:\\theta\\DEPI\\subsoilMcorrelation(DepiSaturated)1.csv")
cor$Depth=factor(cor$Depth,levels=c("topsoilM","subsoilM","deepM"))
cor$variable=factor(cor$variable,levels=c("30cm","60cm","100cm"))
my.theme <- list(
  box.umbrella = list(col = "black"),
  box.rectangle = list(col = "black"),
  plot.symbol   = list(col = "black"), #outlier size and color
  
  panel.background = element_rect(fill = "white"),
  axis.text.x = element_text(face="bold", color="black",size=14),
  axis.text.y = element_text(face="bold", color="black",size=14),
  axis.title=element_text(size=14,face="bold"),
  axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
  
  strip.text.x = element_text( face = 'bold',size = 14, colour = "black"))

panel.grid.minor = element_blank()

bwplot(Correlation~Depth | variable,    ## see the powerful conditional formula 
       data=cor,
       between=list(y=c(-1,1)),
       par.settings = my.theme,
       drop.unused.levels = lattice.getOption("drop.unused.levels"),
       layout = c(3, 1),
       title = "fdfdsfdsfsdf",
       main="")


bwplot(Correlation~Depth,    ## see the powerful conditional formula 
       data=cor,
       between=list(y=c(-1,1)),
       par.settings = my.theme,
       layout = c(3, 1),
       main="")

bwplot(Depth~Correlation | variable,    ## see the powerful conditional formula 
       data=cor,
       between=list(y=c(-1,1)),
       par.settings = my.theme,
       drop.unused.levels = lattice.getOption("drop.unused.levels"),
       layout = c(3, 1),
       main="")

#####################################################################
#Covariates
#####################################################################
# calculate the slope, aspect and solar radiation
setwd('X:/')
a <- raster("dem_depi")
x <- terrain(a, opt=c('slope', 'aspect'), unit='degrees')
#plot(x)
slopeaspect <- extract(x,xy)
SolarRadiation<-cos(slopeaspect[,2]/(180/pi)) * tan(slopeaspect[,1]/(180/pi)) * 100

#calculate the soil order
b <- raster("asc")
soilOrderNo<-extract(b,xy)
soilOrder<-read.csv("X:\\theta\\soilOrder.csv",stringsAsFactors=F)
x<-soilOrder[soilOrder$SoilOrderNo%in% soilOrderNo,]

#PAWC -Palntavailable water capacity 0-1m
c<-raster("X:\\Niranjan\\PAWC_1m\\PAWC_1m\\PAWC_1m")
PAWC<-extract(c,xy)
#######################################
d<-raster("X:\\clay30\\clay30\\clay30")
clay<-extract(d,xy)

#######################################
#covariates- Depi

SiteID<-c('FL1','FL2','FL3','FL4','FL5','FL6','FL7','FL8','FL9','FL10','FL11','FL12','FL13','FL14','FL15','FL16')
covariates<-data.frame(SiteID,slopeaspect[1:16,],SolarRadiation[1:16],soilOrderNo[1:16],PAWC[1:16])
names(covariates)<-c("SiteID","slope","aspect","SolarRadiation","soilOrderNo","PAWC")
#saveRDS(covariates, file = paste("X:/theta/DEPI/","covariatesDepi.Rda", sep=""))
#########################################################################
#covariates-Farmlink

SiteID<-c('FL1','FL2','FL3','FL4','FL5','FL6','FL7','FL8','FL9','FL10','FL11','FL12','FL13','FL14','FL15','FL16','FL17','FL18')
Landuse<-c('grazing','grazing','cropping','cropping','cropping','cropping','cropping','grazing','cropping','cropping','grazing','cropping','cropping','cropping','cropping','cropping','cropping','grazing')
covariates<-data.frame(SiteID,slopeaspect[17:34,],SolarRadiation[17:34],soilOrderNo[17:34],PAWC[17:34],Landuse)
names(covariates)<-c("SiteID","slope","aspect","SolarRadiation","soilOrderNo","PAWC","Landuse")
#saveRDS(covariates, file = paste("X:/theta/DEPI/","covariatesFL.Rda", sep=""))
##########################################################################

#############################
#seasonal forecast
Forecast <- read.csv("R:/PRJ-CSIRO_HDR_Data/Models/SeasonalForecast.csv",header = T)
names(Forecast)<- c("date","forecast")
Forecast$date <-as.Date(Forecast$date,"%d/%m/%Y")

# create a raster with no values but desired extent/projection/ncells
r<- raster(nrows=1,ncols=1,xmn=141.0, xmx=149.0, ymn=-39.0, ymx= -34.0 ,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# lapply outputs a list of rasters
k <- lapply(1:nrow(Forecast), function(i) {
  setValues(r, Forecast[i,2])
} )

#stack the list
s <- stack(k)
val2 <- extract(s,xy,df = TRUE)[-1]
names(val2)<-t(Forecast[,1])
saveRDS(val2, file = paste("X:/theta/","SeasonalForecastDPI.Rda", sep = ""))

Forecast<-readRDS("R:/PRJ-CSIRO_HDR_Data/Models/SeasonalForecastOldfield.Rda")                          

Forecast2011<-Forecast[,as.numeric(substr(colnames(Forecast),1,4))==2011]
names(Forecast2011) <-paste("SF_month",as.numeric(substr(colnames(Forecast2011),6,7)),sep = "")

Forecast2012<-Forecast[,as.numeric(substr(colnames(Forecast),1,4))==2012]
names(Forecast2012) <-paste("SF_month",as.numeric(substr(colnames(Forecast2012),6,7)),sep = "")

Forecast2013<-Forecast[,as.numeric(substr(colnames(Forecast),1,4))==2013]
names(Forecast2013) <-paste("SF_month",as.numeric(substr(colnames(Forecast2013),6,7)),sep = "")

Forecast2014<-Forecast[,as.numeric(substr(colnames(Forecast),1,4))==2014]
names(Forecast2014) <-paste("SF_month",as.numeric(substr(colnames(Forecast2014),6,7)),sep = "")

Forecast2015<-Forecast[,as.numeric(substr(colnames(Forecast),1,4))==2015]
names(Forecast2015) <-paste("SF_month",as.numeric(substr(colnames(Forecast2015),6,7)),sep = "")
