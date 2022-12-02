#' An Automatic Pipeline to Check and Correct Dataframe Names
#'
#' This function will loop over all data frames in the work environment. If a dataframe starts with a digit or a punctuation, the pipeline will add a "x" in
#' front of it so the user will not need to wrap `` around the name of a dataframe during computation.
#' 
#' @param Simple An operator determines whether to simply add a cap at the begining. If false, all texts before the first alphaba will be moved to the end. <default: TRUE>
#' @return 'Corrected' Names for Dataframes starting with non-alphabatic characters
#' @export
#' 

DF_Name_Checker <- function(Simple=TRUE,...){
  df_list <- Who_is(Type = "Data.frame")
  df_list2 <- Who_is(Type = "List")
  df_list <- cbind(df_list,df_list2)
  
  for(i in df_list){
    Space <- grepl("\\s", i)
    if(isTRUE(Space)){
      i_n <- gsub("\\s","_",i)
    } else {
      i_n <- i
    }
    
    Number <- unlist(gregexpr('[[:digit:]]', i_n))
    
    Punct <- unlist(gregexpr('[[:punct:]]', i_n))
    
    
    Alphb <- unlist(gregexpr('[[:alpha:]]', i_n))
    
    if(!1 %in% Alphb){
      if(!isTRUE(Simple)){
        if(!Punct==-1){
          
          # Scenario 1 Moving everything before a punctuation to the end
          if(Punct[[1]]+1 %in% Alphb & !1 %in% Punct[[1]]){
            i_n <- paste0(substr(i_n,Punct[[1]]+1,nchar(i_n)),substr(i_n,Punct[[1]],Punct[[1]]),substr(i_n,1,Punct[[1]]-1))
          } else {
            # Move everything before the first character to the end
            i_n <- paste0(substr(i_n,Alphb[[1]],nchar(i_n)),substr(i_n,1,Alphb[[1]]-1))
          }
        }
        # Move everything before the first character to the end
        i_n <- paste0(substr(i_n,Alphb[[1]],nchar(i_n)),substr(i_n,1,Alphb[[1]]-1))
      }
      # Simplified Version: Adding x at the begining.
      i_n <- paste0("x",i_n)
      
    } else if(Alphb==-1){
      i_n <- paste0("x",i_n)
    }
    
    assign(i_n,value = get(i),.GlobalEnv)
    
    if(!1 %in% Alphb) rm(list = i,envir = .GlobalEnv)
  }
 
}

