#' A Function to Automatically Merging and Order Variables
#'
#' This function will loop over data in 
#' To avoid phaking, the pipeline will automatically detect underpower study and aborpt. Please, check Peduzzi 1996 for calculating statistical powers.
#'
#' @param df_list A list of data.frame names to merge. Make sure that the data.frame have been imported to your environment. If the user wishes to import data.frame in bulk in a directory, one can use the Auto_Importer function from the library. <e.g., c<"df_1","df_2","df_3">, ***Replace "<>"  with parentheses>
#' @param Reference_df The name of the data.frame containing IDs or unique identifiers to merge and sort the merge data with. If not specified as default, the first data.frame in the df_list <default: NULL>
#' @param Reference The name of the variable in the data.frame that the user wishes to use to merge all data.frame. Please, make sure that all data.frame specified in the data.list contains the same name as specified in the Reference.
#' @param sorting_row Reordering the data.frame based on the sequence of ID in the Reference  <Default: TRUE, i.e., Sorting data.frame by the Reference>
#' @param based A logical indicator determining whether to keep or replace the duplicated variables from the prior data.frame based on the sequence of data.frame name listed. If the data.frame names were listed in the sequence of modified version, it is better to specify using "Backward" tracing.
#' @param Assigned_Name The name for the new merge data.frame 
#' @return Import a merged data.frame into your working environment in R (i.e., Global Enviroment in R Studio)
#' @export
#' 



Auto_Bulk_Merger <- function(df_list,Reference_df=NULL,Reference,sorting_row=TRUE,based=c("Foward","Backward"),Assigned_Name,...){
  

  
  Variables <- rep(list(list()),length(df_list))
  names(Variables) <- df_list
  
  for(i in df_list){
    Variables[[i]] <- unique(names(get(i)))
  }
  
  vl <- unique(unlist(Variables))
  
  
  if(!is.null(Reference_df)){
    Base_df <- get(Reference_df)
    df_name <- df_list
  } else {
    Base_df <- get(df_list[[1]])
    R_name <- df_list[[1]]
    df_name <- df_list[-1]
  }

  
# Combiner   
  if(based=="Forward"){
    for(i in df_names){
      unique.vl <-  names(get(i))[!names(get(i)) %in% names(Base_df)[!names(Base_df) %in% Reference]]
      Base_df <- merge.data.frame(Base_df,get(i)[unique.vl],by=Reference,all = TRUE)
    }
  } else if(based=="Backward"){
    for(i in df_names){
      unique.vl <-  names(Base_df)[!names(Base_df) %in% names(get(i))[!names(get(i)) %in% Reference]]
      Base_df <- merge.data.frame(Base_df[unique.vl],get(i),by=Reference,all= TRUE)
    }
  }

  
  if(!is.null(Reference_df)){
    Base_df <- Base_df[match(get(Reference_df)[[Reference]],Base_df[[Reference]]),]
  } else {
    Base_df <- Base_df[match(get(R_name)[[Reference]],Base_df[[Reference]]),]
  }
  
  
  assign(x = Assigned_Name, value = Base_df,.GlobalEnv)
  
  
  
  
  
  
  
  
  
}