nwfscSurvey
===========

Code for analysis of the NWFSC trawl surveys

Overview
=============



The following functions can be used to generate length comps for the NWFSC shelf-slope bottom trawl survey

    

	# Read length comps
    
	FullData = readInLengthComps.fn(...)
    
	# Plot sex ratio
    
	plotSexRatio.fn( len=Data)
    
	# Process data
    
	SS3 = SS3LF.fn(len=Data, fleet=12, nSamps=Ninput, lgthBins=seq(14,66,by=2)

