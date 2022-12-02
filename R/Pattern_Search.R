#' A Function to Automatically Search for the Maximum Patterns among Strings
#'
#' This function will loop over strings and find the maximum patterns needed.
#'
#' @param x A vector of strings
#' @param Direction Patterns search from the begining of each string or backward. <default: "Forward">
#' @param Encoding If .csv files would be imported, one can specify 
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @export
#' 




Pattern_Search <-function(x,Direction="Forward",...) {
  
  # If Direction is backward, reverse the characters
  if(Direction=="Backward"){
    x <- lapply(strsplit(x,""),rev)
    x <- unlist(lapply(x,paste0,collapse=""))
  }
  
  
  # decompose characters in strings
  decomposed_x<-strsplit(x,"")
  
  
  # Maximum and minimum character length
  max_c <- which(unlist(lapply(decomposed_x,length)) %in% max(unlist(lapply(decomposed_x,length))))
  min_c <- which(unlist(lapply(decomposed_x,length)) %in% min(unlist(lapply(decomposed_x,length))))
  
  
  # Alternative - if there is only one longest string, replace it with the second
  if(length(max_c)<2){
    sec_max <- max(unique(unlist(lapply(decomposed_x,length))[duplicated(unlist(lapply(decomposed_x,length)))]))
    max_c <- which(unlist(lapply(decomposed_x,length)) %in%  sec_max )
  }
  
  # Alternative - if there is only one shortest string, replace it with the second
  if(length(min_c)<2){
    sec_min <- min(unique(unlist(lapply(decomposed_x,length))[duplicated(unlist(lapply(decomposed_x,length)))]))
    min_c <- which(unlist(lapply(decomposed_x,length)) %in%  sec_min )
  }
  

  
# search for the first not common element and so, get the last matching one
dx_max <- decomposed_x[max_c]
dx_min <- decomposed_x[min_c]

 
######################### First Try with the longest names ###############################  
dg <-  lapply(1:(length(dx_max)-1),function(m){
        
  s <- seq(m+1,length(dx_max))
        
   dm <-  lapply(s,function(m2){
     
       d <- which(dx_max[[m2-1]] == dx_max[[m2]])
       # Find the first and the last characters that are best matched
       n_max_match <- max(rle(diff(d))$length)
       n_min_match <- sum(rle(diff(d))$length[!rle(diff(d))$length %in% n_max_match])+1
       
       paste0(dx_max[[m]][d[(n_min_match+1):(n_min_match+n_max_match)]],collapse = "")
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
    n_min_match <- sum(rle(diff(d))$length[!rle(diff(d))$length %in% n_max_match])+1
    
    paste0(dx_min[[m]][d[(n_min_match+1):(n_min_match+n_max_match)]],collapse = "")
  })
  
  # Identify the most common pattern
  dm.1 <- summary(as.factor(unlist(dm)))
  dm_which <- which.min(unlist(lapply(names(dm.1),nchar)))
  names(dm.1)[[dm_which]]
})


dx_min_final <- names(summary(as.factor(unlist(dg))))    # This are the best patterns matched.
#########################             END                 ###############################



if(any(dx_min_final %in% dx_max_final)){
  final_pattern <- dx_min_final[dx_min_final %in% dx_max_final]

  
} else {
  
  # If the patterns do not match, report all
    
  final_pattern <- c(dx_min_final,dx_max_final)
  
  }


# If Direction is backward, reverse the characters
if(Direction=="Backward"){
  final_pattern <- unlist(lapply(lapply(strsplit(final_pattern,""),rev),paste0,collapse=""))
}


  # if there is no matching element, return an empty vector, else return the common part

if(length(final_pattern)>1) warning("More than one possible patterns detected")
return(final_pattern)
}