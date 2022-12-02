#' A Function to Automatically Search for the Maximum Patterns among Strings
#'
#' This function will loop over strings and find the maximum patterns needed.
#'
#' @param x A vector of strings
#' @param Direction Patterns search from the begining of each string ("Forward"), from the end ("Backward") or "Both" ways. <<default: "Both">
#' @param Match_All A logical parameter indicating whether to only report patterns matched in all elements in x. If FALSE, all possible patterns will be reported. <default: TRUE>
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @export
#'




Pattern_Search <-function(x,Direction="Both",Match_All=TRUE,...) {

  y <- x

######################### Backward ###############################
  # If Direction is backward, reverse the characters
if(Direction=="Backward"){
    y <- lapply(strsplit(x,""),rev)
    y <- unlist(lapply(y,paste0,collapse=""))
  }
#########################   END    ###############################



### decompose characters in strings ---------------------
  decomposed_x<-strsplit(y,"")

### Pattern Genie ---------------
  dx_max_final <- Pattern_Genie(decomposed_x)



######################### Backward ###############################

if(Direction=="Backward"){
### Reverse ---------------
  dx_max_final <- unlist(lapply(lapply(strsplit(dx_max_final,""),rev),paste0,collapse=""))
  }
#########################   END    ###############################




#########################     Both Ways     ###############################
if(Direction=="Both"){
### Reverse the characters ---------
  y <- lapply(strsplit(x,""),rev)
  y <- unlist(lapply(y,paste0,collapse=""))



### decompose characters in strings ----------------
  decomposed_x<-strsplit(y,"")


### Pattern Genie ---------------
  dx_max_final2 <- Pattern_Genie(decomposed_x)

### Reverse ---------------
dx_max_final2 <- unlist(lapply(lapply(strsplit(dx_max_final2,""),rev),paste0,collapse=""))


  #### Combine Them ---------------------------

  dx_max_final <- unique(c(dx_max_final,dx_max_final2))
}
#########################             END                 ###############################







#########################     Second Iteration     ###############################


Retest <-  lapply(dx_max_final,function(dx){
    grepl(dx,x)
  })

names(Retest) <- dx_max_final

TRUE_Len <- sort(unlist(lapply(Retest,function(x){
  length(which(x))
})),decreasing = TRUE)

#########################             END                 ###############################


if(isTRUE(Match_All)){
  Final_Pattern <- names(TRUE_Len)[which(TRUE_Len == length(x))]
  if(length(Final_Pattern)>1) warning("More than one possible patterns detected")
} else {
  Final_Pattern <- list()
  Final_Pattern$All <- names(TRUE_Len)[which(TRUE_Len == length(x))]
  Final_Pattern$Partoal <- names(TRUE_Len)[which(!TRUE_Len == length(x))]
}



  # if there is no matching element, return an empty vector, else return the common part

return(Final_Pattern)
}
