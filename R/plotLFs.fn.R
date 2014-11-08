plotLFs.fn <-
function(dat,inch=0.15,ylab="Bins",xlab="Year",zero2NAs=T,...) {
    #a wrapper for the plotFreqData because I originally called it plotLFs.fn and want to keep it compatible with already written analyses
    plotFreqData.fn(dat,inch,ylab,xlab,zero2NAs,...)
}
