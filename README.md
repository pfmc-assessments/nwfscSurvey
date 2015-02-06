nwfscSurvey
===========

Code for analysis of the NWFSC trawl surveys

Overview
=============



The following functions can be used to generate length comps for the NWFSC shelf-slope bottom trawl survey

    

	# Read length comps
	Data = readInLengthComps.fn(...)
    
	# Plot sex ratio
	plotSexRatio.fn( len=Data )
    
	# Process data
	SS3 = SS3LF.fn(len=Data, lgthBins=seq(14,66,by=2))

