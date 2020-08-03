##################################################################
#
#	Code example how to use the AKFSC Slope "cleaner" fxn
#				called Format.AKSlope.fn
#
#	These data are not yet in the database and hence why
#			this approach is needed.
#
#			Written by Chantel Wetzel
#
####################################################################


devtools::install_github("nwfsc-assess/nwfscSurvey")
library(nwfscSurvey)

# Set the working directory and load dmp files from John Wallace
setwd("C:/_Research/nwfscSurvey_test/sablefish/")
load("Tri.Shelf.and.AFSC.Slope.canary.Catch.24.May.11.dmp")
catch = Tri.Shelf.and.AFSC.Slope.canary.Catch.24.May.11

load("AFSC.Slope.Shelf.sable.bio.5.24.11.dmp")
bio = AK.Surveys.Bio.sablefish.24.May.11

# Now there should be both the catch and the biological data
# loaded in the console under "catch" and "bio". These objects
# are then passed the the Format.AKSlope.fn. The Format.AKSlope.fn
# returns a list ([[1]] cleaned catch data, [[2]] cleaned length
# data, [[3]] cleaned age data if available)

#Filter the data
filter.dat = Format.AKSlope.fn(datTows = catch, datL = bio, start.year = 1997) 

catch = filter.dat$datTows
len = filter.dat$length
age = filter.dat$age