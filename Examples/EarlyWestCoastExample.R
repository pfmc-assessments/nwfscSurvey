### AK surveys (slope and triennial)

#load in data
#source in EarlyWestCoast

load(paste(directory,"SA3.dmp",sep="/"))

#remove foreign hauls
foreignHauls <- read.csv(paste(directory,"foreign_hauls.csv",sep="/"))
foreignInd <- !(Dover.AK.28.MAr.2010$HAULJOIN %in% foreignHauls$HAULJOIN)
datB <- Dover.AK.28.MAr.2010[foreignInd,]
cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed)\n")

#get length and age data
datL <- AK.Surveys.Bio.Dover.17.Mar.11$Lengths
foreignInd <- !(datL$HAULJOIN %in% foreignHauls$HAULJOIN)
datL <- datL[foreignInd,]
cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) after removing foreign hauls\n")
datA <- AK.Surveys.Bio.Dover.17.Mar.11$Ages
foreignInd <- !(datA$HAULJOIN %in% foreignHauls$HAULJOIN)
datA <- datA[foreignInd,]
cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) after removing foreign hauls\n")


#set up stratifications and area of strata (requires SA3 file)
strata.AKslope <- read.table(paste(directory,"StrataAKslope.txt",sep="\\"),header=T)
names(strata.AKslope) <- c("name","START_LATITUDE.2","START_LATITUDE.1","BOTTOM_DEPTH.1","BOTTOM_DEPTH.2")
strata.AKslope <- data.frame(name=strata.AKslope[,1],area=NA,strata.AKslope[,c(4,5,2,3)])
strata.AKslope <- StrataAreas.fn(strata.AKslope,SA3)


#StrataAKslope.txt looks like this
#  STRATA   NLat  SLat  MinDepth  MaxDepth
#  A       49.0   45.0   183        549
#  B       49.0   45.0   549        900
#  C       49.0   40.5   900       1280
#  D       45.0   40.5   183        549
#  E       45.0   40.5   549        900
#  F       40.5   34.5   183        549
#  G       40.5   34.5   549        900
#  H       40.5   34.5   900       1280







### AFSC slope
xB <- datB[datB$SURVEY=="AFSC.Slope" & datB$YEAR%in%1997:2001,]
xB$year <- xB$YEAR
#calculate the density (kg/km^2) using net width and distance fished
xB$areaFished <- xB$DISTANCE_FISHED*(xB$NET_WIDTH/1000)
tmp <- sum(is.na(xB$areaFished))
cat("There are",tmp,"instances where area swept could not be calculated due to missing values.\n")
xB$catchPerArea <- xB$WEIGHT/xB$areaFished
xB$catchPerArea[is.na(xB$catchPerArea)&!is.na(xB$areaFished)] <- 0                                  #the tows with no observation of the species
xB <- xB[!is.na(xB$catchPerArea),]
#call the function for the Design based estimates
#use biomass$LNtons for SS3 (with medianBhat)
biomass <- DesignBasedEstBiomass.EWC.fn(xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope)


xL <- datL[datL$SURVEY=="AFSC.Slope" & datL$YEAR%in%1997:2001,]
xL <- xL[!is.na(xL$LENGTH),]
xL$WEIGHT <- xL$SP_TOW_WGHT_KG
xL$NUMBER_FISH <- xL$SP_TOW_NUM
xL$year <- xL$YEAR

#the length comps ready for SS3, by sex (use gender=0 for unsexed)
LFs <- SurveyLFs.EWC.fn(xL,xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope,femaleMale=c(2,1),lgthBins=seq(80,600,20),SS3out=T,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=200)
write.csv(LFs,paste(directory,"ForSS3\\LengthCompsForSS3.AKslope.csv",sep="\\"),row.names=F)
par(mfrow=c(1,2))
plotFreqData.fn(lfsSS,ylim=c(0,62),yaxs="i",ylab="Length")

SurveyLFs.EWC.fn(xL,xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope,femaleMale=c(2,1),lgthBins=c(80,600),SS3out=T,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=200)
SurveyLFs.EWC.fn(xL,xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope,femaleMale=c(2,1),lgthBins=seq(10,80,10),SS3out=T,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=200)

#nTows
tows <- xL[!duplicated(xL$HAULJOIN),]
table(tows$year)
