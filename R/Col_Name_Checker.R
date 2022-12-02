#' An Automatic Pipeline to Check and Correct Column Names
#'
#' This function will loop over all data frames in the work environment. If a dataframe starts with a digit or a punctuation, the pipeline will add a "x" in
#' front of it so the user will not need to wrap `` around the name of a dataframe during computation.
#'
#' @param col.names The name of variables/columns within a dataframe.
#' @return 'Corrected' Names for Dataframes starting with non-alphabatic characters
#' @export
#'

Col_Name_Checker <- function(col.names,...){
  if(!require("stringi",character.only = TRUE)) stop("Package Stringi not found")


  col.names <- stringi::stri_trans_general(col.names,id = "Latin-ASCII")


  for(i in col.names){
    Space <- grepl("\\s", i)
    if(isTRUE(Space)){
      i_n <- gsub("\\s","_",i)
    } else {
      i_n <- i
    }

    Alphb <- unlist(gregexpr('[[:alpha:]]', i_n))
    if(Alphb==-1) i_n <- paste0("x",i_n)


    Punct <- unlist(gregexpr('[[:punct:]]', i_n))
    if(!Punct==-1) i_n <- gsub("[[:punct:]]", "_", i_n)



    col.names[col.names %in% i] <- i_n

  }
 col.names
}

