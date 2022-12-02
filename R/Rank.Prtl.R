Rank.Prtl <- function(x,na.rm=T,...) {
if(!na.rm==T & !na.rm==TRUE){
trunc(rank(x))/length(x)
} else {
trunc(rank(x))/length(!is.na(x))
}
}