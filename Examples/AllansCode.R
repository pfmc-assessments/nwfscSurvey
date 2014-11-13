setwd("//nwcfs2/Assessment/VladaGertseva/AllanNWFSC_combo")

install.packages("nwfscSurveyCode", repos="http://R-Forge.R-project.org")
library(nwfscSurveyCode)
updateSurveyCode()

A <- readInAgeComps.fn("AgeComps.csv",headerRow=7)
A$Length <- A$Age
tmp <- SS3LF.fn(A,lgthBins=1,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=40)
par(mfrow=c(1,2))
plotFreqData.fn(tmp,ylab="Age")
#write.csv(tmp,"ForSS3\\AgeCompsSS3_fromAllansCode.csv",row.names=F)

#more control and give options for adding back in unsexed
A <- readInAgeComps.fn("AgeComps.csv",headerRow=7)
AatL <- SS3AgeAtLen.fn(A,lgthBins=seq(4,62,2),ageBins=0:40,partition=3,raw=T,sexRatioUnsexed=0.5,maxSizeUnsexed=40)
#AatL$female[,c("year","LbinLo","LbinHi","F0")]
#AatL$male[,c("year","LbinLo","LbinHi","M0")]
#write.csv(AatL$female,"ForSS3\\AgeAtLenForSS3.female.AllansCode.csv",row.names=F)
#write.csv(AatL$male,"ForSS3\\AgeAtLenForSS3.male.AllansCode.csv",row.names=F)



#tmp[,c("year","F0","M0")]  #just the age 0 fish

#AatL <- GetAges.fn("AgeComps.csv",headerRow=7,lgthBins=seq(4,62,2),ageBins=0:40,raw=T)
#AatL$female[,c("year","LbinLo","LbinHi","F0")]
#AatL$male[,c("year","LbinLo","LbinHi","M0")]
#notice there are very few age zero fish. But, the age comps show that there are more. I think there are unsexed age 0 fish.
