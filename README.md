nwfscSurvey
===========

Code for analysis of the NWFSC trawl surveys

Note: this code was developed for use by scientists at the Northwest Fisheries Science Center and is intended to work on the specific data products that we have access to using methods specific to the needs of this group. Anyone interested in obtaining survey data should contact the NWFSC to make a formal data request.

Overview
=============



The following functions can be used to generate length comps for the NWFSC shelf-slope bottom trawl survey

    

	# Read length comps
	Data = readInLengthComps.fn(...)
    
	# Plot sex ratio
	plotSexRatio.fn( len=Data )
    
	# Process data
	SS3 = SS3LF.fn(len=Data, lgthBins=seq(14,66,by=2))

