#' A Function to Automatically Search for the Maximum Patterns among Strings
#'
#' This function will loop over strings and find the maximum patterns needed.
#'
#' @param x A vector of strings
#' @param Direction Patterns search from the begining of each string ("Forward"), from the end ("Backward") or "Both" ways. <<default: "Both">
#' @param Match_All A logical parameter indicating whether to only report patterns matched in all elements in x. If FALSE, all possible patterns will be reported. <default: TRUE>
#' @param Method A logical parameter for the search algorithm. There are two algorithms available in the current version: "PG", aka., Pattern_Genie, and "GG", aka Giant_n_Gnome. Pattern_Genie is best suited for multiple patterns that may be present at any given part of a string. Giant_n_Gnome is useful when the patterns desired are at the either end of the string.
#' @param Threshold Cutoff for the percentage of pattern matched. <default: 0.33 i.e., 33%>
#' @return Reutrn best matched patterns in R (i.e., Global Enviroment in R Studio)
#' @export
#' @examples
#' Strings <- names()
#'




Pattern_Search <-function(x,Direction="Both",Match_All=TRUE,Method="GG",Threshold=0.33,...) {

  y <- x

######################### Backward ###############################
  # If Direction is backward, reverse the characters
if(Direction=="Backward"){
    y <- lapply(strsplit(x,""),rev)
    y <- unlist(lapply(y,paste0,collapse=""))
  }
#########################   END    ###############################

if(!Method %in% c("GG","PG")) stop("Please, double check the algorithm specified in Method.")

### decompose characters in strings ---------------------
  decomposed_x<-strsplit(y,"")

### Pattern Genie ---------------
  dx_max_final <- NULL

  if(Method=="PG"){
    dx_max_final <- Pattern_Genie(decomposed_x)
  } else if(Method=="GG"){
    dx_max_final <- Giant_Gnome(decomposed_x)
}


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
  dx_max_final2 <- NULL

  if(Method=="PG"){
    dx_max_final2 <- Pattern_Genie(decomposed_x)
  } else if(Method=="GG"){
    dx_max_final2 <- Giant_Gnome(decomposed_x)
  }

  dx_max_final2 <- unlist(lapply(lapply(strsplit(dx_max_final2,""),rev),paste0,collapse=""))
  #### Combine Them ---------------------------

  if(!is.null(dx_max_final2) & !is.null(dx_max_final)){
    dx_max_final <- unique(c(dx_max_final,dx_max_final2))
  }

}
#########################             END                 ###############################






if(length(dx_max_final)==0){

  print("No clear pattern was found! Please, try another algorithm.")

} else {
#########################     Second Iteration     ###############################


Retest <-  lapply(dx_max_final,function(dx){
    grepl(dx,x)
  })

names(Retest) <- dx_max_final

TRUE_Len <- sort(unlist(lapply(Retest,function(x){
  length(which(x))
})),decreasing = TRUE)

#########################             END                 ###############################




#########################     Return Results    ###############################


 if(isTRUE(Match_All)){
  Final_Pattern <- names(TRUE_Len)[which(TRUE_Len == length(x))]
  if(length(Final_Pattern)>1) warning("More than one possible patterns detected")
} else {
  Final_Pattern <- list()
  Final_Pattern$All <- names(TRUE_Len)[which(TRUE_Len == length(x))]
  Final_Pattern$Partial <- names(TRUE_Len)[which(!TRUE_Len == length(x))]
}



return(Final_Pattern)
}


}
