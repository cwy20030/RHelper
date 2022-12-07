# Giant Gnome
#
# This function will loop over strings and find all possible patterns.
#
# @param Decomp strsplit characters
# @return export Patterns
#






Giant_Gnome <- function(Decomp,...){



# Maximum and minimum character length
max_c <- which(unlist(lapply(Decomp,length)) %in% max(unlist(lapply(Decomp,length))))
min_c <- which(unlist(lapply(Decomp,length)) %in% min(unlist(lapply(Decomp,length))))


# Alternative - if there is only one longest string, replace it with the second
if(length(max_c)<2){
  sec_max <- max(unique(unlist(lapply(Decomp,length))[duplicated(unlist(lapply(Decomp,length)))]))
  max_c <- which(unlist(lapply(Decomp,length)) %in%  sec_max )
}

# Alternative - if there is only one shortest string, replace it with the second
if(length(min_c)<2){
  sec_min <- min(unique(unlist(lapply(Decomp,length))[duplicated(unlist(lapply(Decomp,length)))]))
  min_c <- which(unlist(lapply(Decomp,length)) %in%  sec_min )
}



# search for the first not common element and so, get the last matching one
dx_max <- Decomp[max_c]
dx_min <- Decomp[min_c]


######################### First Try with the longest names ###############################
dg <-  lapply(1:(length(dx_max)-1),function(m){

  s <- seq(m+1,length(dx_max))

  dm <-  lapply(s,function(m2){

    d <- which(dx_max[[m2-1]] == dx_max[[m2]])
    # Find the first and the last characters that are best matched
    n_max_match <- max(rle(diff(d))$length)
    if(which(rle(diff(d))$length==n_max_match)==1){
      paste0(dx_max[[m]][d[1:(1+n_max_match)]],collapse = "")
    } else {
      n_min_match <- sum(rle(diff(d))$length[!rle(diff(d))$length %in% n_max_match])
      paste0(dx_max[[m]][d[(n_min_match):(n_min_match+n_max_match)+1]],collapse = "")
    }
  })

  # Identify the most common pattern
  dm.1 <- summary(as.factor(unlist(dm)))
  dm_which <- which.min(unlist(lapply(names(dm.1),nchar)))
  names(dm.1)[[dm_which]]
})

dx_max_final <- names(which.max(summary(as.factor(unlist(dg)))))    # This is the best pattern matched.
#########################             END                 ###############################





######################### First Try with the shortest names ###############################
dg <-  lapply(1:(length(dx_min)-1),function(m){

  s <- seq(m+1,length(dx_min))

  dm <-  lapply(s,function(m2){

    d <- which(dx_min[[m2-1]] == dx_min[[m2]])
    # Find the first and the last characters that are best matched
    n_max_match <- max(rle(diff(d))$length)


    if(which(rle(diff(d))$length==n_max_match)==1){
      paste0(dx_min[[m]][d[1:(1+n_max_match)]],collapse = "")
    } else {
      n_min_match <- sum(rle(diff(d))$length[!rle(diff(d))$length %in% n_max_match])
      paste0(dx_min[[m]][d[(n_min_match):(n_min_match+n_max_match)+1]],collapse = "")
    }
  })

  # Identify the most common pattern
  dm.1 <- summary(as.factor(unlist(dm)))
  dm_which <- which.min(unlist(lapply(names(dm.1),nchar)))
  names(dm.1)[[dm_which]]
})


dx_min_final <- names(summary(as.factor(unlist(dg))))    # This are the best patterns matched.
#########################             END                 ###############################



if(any(dx_min_final %in% dx_max_final)){
  dx_max_final2 <- dx_min_final[dx_min_final %in% dx_max_final]


} else {

  # If the patterns do not match, report all
  dx_max_final2 <- c(dx_min_final,dx_max_final)
}

return(dx_max_final2)

}
