createStrataDF.fn <- function(depths,lats,SA3) {
    out <- data.frame(
        name=NA,
        area=NA,
        BOTTOM_DEPTH.1=rep(depths[-length(depths)],length(lats)-1),
        BOTTOM_DEPTH.2=rep(depths[-1],length(lats)-1),
        START_LATITUDE.1=rep(lats[-length(lats)],each=length(depths)-1),
        START_LATITUDE.2=rep(lats[-1],each=length(depths)-1)
    )
    out$name=LETTERS[1:nrow(out)]
    out <- StrataAreas.fn(out,SA3)
}
