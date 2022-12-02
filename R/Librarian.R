Librarian <- function(df,dictionary_file,Assigned_df_Name,Missing_Identifier=FALSE,Keyword_for_Exclude=FALSE, Variables="All",factor_type=FALSE,numeric_type=FALSE,sorting_columns=FALSE,...){
  
if(!Missing_Identifier==FALSE){
    Missing_df <- dictionary_file$Categories[dictionary_file$Categories$missing=="1" & dictionary_file$Categories$variable %in% names(df),]
  }  else if(Variables=="All" & Keyword_for_Exclude==FALSE){
Missing_df <- dictionary_file$Categories[dictionary_file$Categories$`label:en` %in% c("Missing due to contradictory responses","Missing","Not Applicable","[DO NOT READ] Refused","[DO NOT READ] Don't know/No answer","Refused","Don't know/No answer","Don't Know/No Answer","Don't Know / No Answer","Skipped","[DO NOT READ] Skip recording","Skip Pattern","Skip pattern","Did not complete a DCS visit","Not administered in accommodation strategy","Missing") & dictionary_file$Categories$variable %in% names(df),]
  } else if(!Variables=="All" & !Keyword_for_Exclude==FALSE) {
Missing_df <- dictionary_file$Categories[dictionary_file$Categories$`label:en` %in% Keyword_for_Exclude & dictionary_file$Categories$variable %in% Variables,]
  } else if(!Variables=="All" & Keyword_for_Exclude==FALSE) {
Missing_df <- dictionary_file$Categories[dictionary_file$Categories$`label:en` %in% c("Missing due to contradictory responses","Missing","Not Applicable","[DO NOT READ] Refused","[DO NOT READ] Don't know/No answer","Refused","Don't know/No answer","Don't Know/No Answer","Don't Know / No Answer","Skipped","[DO NOT READ] Skip recording","Skip Pattern","Skip pattern","Did not complete a DCS visit","Not administered in accommodation strategy","Missing") & dictionary_file$Categories$variable %in% Variables,]
 } else if(Variables=="All" & !Keyword_for_Exclude==FALSE) {
Missing_df <- dictionary_file$Categories[dictionary_file$Categories$`label:en` %in% Keyword_for_Exclude & dictionary_file$Categories$variable %in% names(df),]
    warning("All variables were cleaned because the `Variable` parameter was set to `All`!")
 } 

for(i in Missing_df$variable){
    df[[i]][df[[i]] %in% Missing_df$name[Missing_df$variable==i]] <- NA
}

if(!factor_type==FALSE){
  factor_list <- dictionary_file$Variables[dictionary_file$Variables %in% factor_type]  
  df[factor_list] <- lapply(df[factor_list],as.factor)
}  

if(!numeric_type==FALSE){
numeric_list <- dictionary_file$Variables[dictionary_file$Variables %in% numeric_type]  
df[numeric_list] <- lapply(df[numeric_list],as.numeric)
}
  
if(!sorting_columns==FALSE) df <- df[sort(names(df))]
  
Not_in_Ditionary <- names(df)[!names(df) %in% dictionary_file$Variables$name]  
if(!is.null(Not_in_Ditionary)) print(paste0("The following variables were not found in your dictionary:",Not_in_Ditionary))

assign(Assigned_df_Name,value = df,.GlobalEnv)
 }