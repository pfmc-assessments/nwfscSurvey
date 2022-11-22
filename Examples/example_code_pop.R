#===============================================================================
# Package test
#===============================================================================

# https://github.com/nwfsc-assess/nwfscSurvey
devtools::install_github("nwfsc-assess/nwfscSurvey", build_vignettes = TRUE)

# Load the packaged
library(nwfscSurvey)
# Look at the vignette
vignette("nwfscSurvey")
# Look at all the functions in the package
ls("package:nwfscSurvey")
?PullCatch.fn

#===============================================================================
#=============          NWFSC Combo          ===================================
#===============================================================================
setwd("Set directory here")
catch = PullCatch.fn(Name = "Pacific ocean perch", SurveyName = "NWFSC.Combo", SaveFile = TRUE, Dir = getwd()) 

bio  = PullBio.fn(Name = "Pacific ocean perch", SurveyName = "NWFSC.Combo", SaveFile = TRUE, Dir = getwd())


head(catch)
head(bio)

# Can pull data based on the general name (Name) of the scientific name(SciName). The default year range (YearRange)
# is set to cover all potential years.  The SurveyName options are: Triennial, AFSC.Slope, NWFSC.Slope, NWFSC.Shelf
# NWFSC.Combo, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, or NWFSC.Video. These data pulls can also be saved to a specified 
# directory using SaveFile = TRUE and Dir = "directory to save file". 
# load("Catch_2018-08-06__NWFSC.Combo_2018-08-06.rda")


# Create Stratafication:
# The stratafication areas are calculated from the SA3 file which is attached to the package.
strata = CreateStrataDF.fn(names=c("shallow_s", "mid_s", "deep_s", "shallow_n", "mid_n", "deep_n"), 
                           depths.shallow = c(55,  200, 300,  55, 200, 300),
                           depths.deep    = c(200, 300, 549, 200, 300, 549),
                           lats.south     = c(32,   32,  32,  42,  42,  42),
                           lats.north     = c(42,   42,  42,  49,  49,  49))

strata

# Look at the number of tows by strata and the number of positive tows for the species.
CheckStrata.fn(dat = catch, strat.df = strata)

# Calculate the design based index
biomass = Biomass.fn(dir = getwd(), dat = catch,  strat.df = strata, printfolder = "forSS", outputMedian = T) 

# Creates a csv file within the "printfolder" that will be saved within the directory location (dir).

# Plot the biomass index
PlotBio.fn(dir = getwd(), dat = biomass, main = "NWFSC WCBBT Survey", dopng = TRUE)
PlotBioStrata.fn(dir = getwd(), survey.name = "NWFSC_WCGBT_Survey", dat = biomass, mfrow.in = c(3,2), sameylim = TRUE, ylim = c(0, 22), dopng = TRUE)

# Plot the CPUE
plot_cpue(dir = getwd(), catch = catch)

# Plot the proportion of positive tows by depth
plot_proportion(data = catch,
                dim = "depth",
                dir = getwd(),
                depth_min = 50,
                depth_max = 500,
                depth_bin_width = 50,
                lat_min = 40,
                lat_max = 49,
                lat_bin_width = 1.0)

# Plot the proportion of positive tows by latitude
plot_proportion(data = catch,
                dim = "lat",
                dir = getwd(),
                depth_min = 50,
                depth_max = 500,
                depth_bin_width = 50,
                lat_min = 40,
                lat_max = 49,
                lat_bin_width = 1.0)

# Plot the proportion of males and female observed by depth
plot_proportion(data = bio,
                dim = "sex",
                dir = getwd(),
                depth_min = 50,
                depth_max = 500,
                depth_bin_width = 50,
                lat_min = 40,
                lat_max = 49,
                lat_bin_width = 1.0)

#============================================================================================
#Length Biological Data 
#============================================================================================
len = bio
len.bins = 11:47

# Plot the observed length  by sex, latitude, and depth
plot_bio_patterns(dir = getwd(), bio = len, col_name = "Length_cm")

# Calculate the effN
n = GetN.fn(dir=getwd(), dat = len, type = "length", species = "shelfrock", printfolder = "forSS")

# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.

# Expand and format length composition data for SS
LFs <- SurveyLFs.fn(dir = getwd(), datL = len, datTows = catch,  
                    strat.df = strata, lgthBins = len.bins, sex = 3, 
                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 26, 
                    nSamps = n, fleet = 7)

# The code offers two options for applying the sex ratio based on expansion stage. The sex ratio will be
# applied based on a tow basis first if sexRatioStage = 1. The other option applies the sex ratio to the
# expanded numbers of fish across a whole strata (sexRatioStage = 2, this was the option applied to the
# NWFSC combo survey data in the past).

plot_comps(data = LFs, dir = getwd())
PlotSexRatio.fn(dir = getwd(), dat = len, data.type = "length", dopng = TRUE, main = "NWFSC WCGBT Survey")

#============================================================================================
#Length Biological Data without expansion 
#============================================================================================

# Development still in process
# If sex = 3, will only examine sexed fish (sex ratio for unsexed fish not currently done)

lengths <- UnexpandedLFs.fn(dir = getwd(),
                           datL = len,
                           lgthBins = len.bins,
                           sex = 3 ) 

#============================================================================================
#Age Biological Data 
#============================================================================================
age = bio
age.bins = 1:40

# Plot the observed age by sex, latitude, and depth
plot_bio_patterns(dir = getwd(), bio = age, col_name = "Age")

n = GetN.fn(dir = getwd(), dat = age, type = "age", species = "shelfrock", printfolder = "forSS")

# Exand and format the marginal age composition data for SS
AFs <- SurveyAFs.fn(dir = getwd(), datA = age, datTows = catch,  
                     strat.df = strata, ageBins = age.bins, 
                     sexRatioStage = 2, sexRatioUnsexed = 0.50, maxSizeUnsexed = 5, 
                     sex = 3, nSamps = n, fleet = 7)

plot_comps(data = AFs, dir = getwd())
PlotVarLengthAtAge.fn(dir = getwd(), dat = age, main = "NWFSC WCGBT Survey", dopng = TRUE) 
PlotSexRatio.fn(dir = getwd(), dat = age, data.type = "age", dopng = TRUE, main = "NWFSC WCGBT Survey")

#============================================================================================
#Age Biological Data without Expansion
#============================================================================================

Ages <- UnexpandedAFs.fn(dir = getwd(),
                        datA = age,
                        ageBins = age.bins,
                        sex = 3 ) 

#============================================================================================
# Conditional Ages
#============================================================================================
Ages <- SurveyAgeAtLen.fn (dir = getwd(), datAL = age, datTows = catch, 
                          strat.df = strata, lgthBins = len.bins, ageBins = age.bins, partition = 0)

