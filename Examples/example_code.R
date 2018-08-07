library(devtools)
devtools::install_github("nwfsc-assess/nwfscSurvey", ref = "development")
library(nwfscSurvey)

catch = PullCatch.fn(Name = "sablefish", SurveyName = "NWFSC.Combo")
bio   = PullBio.fn(Name = "sablefish", SurveyName = "NWFSC.Combo")

# Input the strata definitions following this example:
strata = CreateStrataDF.fn(names=c("shallow_32", "deep_32", 
                                   "shallow_345", "deep_345",
                                   "shallow_405", "deep_405",
                                   "shallow_43", "deep_43",
                                   "shallow_475", "deep_475"),  
                           depths = c(55, 300, 549),
                           lats = c(32, 34.5, 40.5, 43, 47.5, 49)) 


len = bio
Lengths <- SurveyLFs.fn(dir = getwd(), datL = len, datTows = catch,  strat.df = strata, lgthBins = len.bins, 
                    outputStage1 = TRUE, gender = 3, 
                    # These inputs below will apply the sex ratio function to the stage one expansions
                    # If you do not want this set sexRatioStage = 2
                    sexRatioStage = 1, sexRatioUnsexed = 0.5, maxSizeUnsexed = "input value")

age = bio
Ages <- SurveyAFs.fn(dir = getwd(), datA = age, datTows = catch,  strat.df = strata, ageBins = age.bins, 
                     outputStage1 = TRUE, gender = 3,
                    # These inputs below will apply the sex ratio function to the stage one expansions
                    # If you do not want this set sexRatioStage = 2
                     sexRatioStage = 1, sexRatioUnsexed = 0.50, maxSizeUnsexed = "input value")