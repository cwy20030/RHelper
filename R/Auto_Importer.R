#' A Function to Automatically Import Files in a Folder
#'
#' This function will loop over logistic regressions in a data frame specified with at least 1 unadjusted model and an adjusted one, cross-sectionally.
#' To avoid phaking, the pipeline will automatically detect underpower study and aborpt. Please, check Peduzzi 1996 for calculating statistical powers.
#'
#' @import readxl haven readr
#' @param directory Directory of where the files that you want to import <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @param file_type The type of file to import. Currently, supports  ".tsv  ".csv", ".xls(x)",".sas",".sav". <default: ".csv">
#' @param Encoding If .csv files would be imported, one can specify
#' @param Deliminator If the data is stored in .csv file and the Deliminator is not comma as default, please, specify it. <default: ",">
#' @param Keyword Specify keywords to import only specific files based on file names <Default: FALSE, i.e., import all>
#' @param Exclude Specify keywords to exclude specific files from importation <Default: FALSE, i.e., exclude none>
#' @param inList Binary operator identify if to import all files into a large list <Default: FALSE, i.e., import each separately into individual dataframe>
#' @param Meta_List_Name The name assigned to the meta data list if inList is TRUE <Default: df_list>
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @export
#' @examples
#'
#'
#'
#'


Auto_Importer <- function(directory,file_type=".csv",Encoding="UTF-8",Deliminator=",", Keyword= FALSE,Exclude= FALSE, inList=FALSE, Meta_List_Name="df_list",...){


  if(file_type==".xls" | file_type==".xlsx") {
  if(!require("readxl",character.only = TRUE)) stop("Package 'readxl' not found")

  } else if(file_type==".sas" | file_type==".sav"){
  if(!require("haven",character.only = TRUE)) stop("Package 'haven' not found")

  } else if(file_type==".tsv"){
    if(!require("readr",character.only = TRUE)) stop("Package 'readr' not found")
  }

############################################
### Read All files in the directory ###
############################################

  if(length(file_type)>1) warnings("Sorry, for the current version, please keep it to one type of files at a time. Or I may throw up.")


  if(!Keyword==FALSE) {

    files = iconv(list.files(path = directory,pattern = paste0("*",file_type)), from="UTF-8", to="LATIN1")

    if(length(Keyword)>1){
      files_original <- iconv(list.files(path = directory,pattern = paste0("*",file_type)), from="UTF-8", to="LATIN1")

      files <- rep(list(),length(Keyword))

      files <- lapply(Keyword,function(ln){
        Exclusion <- grep(pattern = ln,files_original)
        files_original[Exclusion]
      })



      file_dir = iconv(list.files(path=directory,pattern = paste0("*",file_type),full.names = TRUE), from="UTF-8", to="LATIN1")[Exclusion]

      file_names = gsub(x = files,pattern = file_type ,replacement = "")

    } else {
      Exclusion <- grep(pattern = Keyword,files)
      files <- files[Exclusion]

      file_dir = iconv(list.files(path=directory,pattern = paste0("*",file_type),full.names = TRUE), from="UTF-8", to="LATIN1")[Exclusion]

      file_names = gsub(x = files,pattern = file_type ,replacement = "")
    }



  } else {

files = iconv(list.files(path = directory,pattern = paste0("*",file_type)), from="UTF-8", to="LATIN1")
file_dir = iconv(list.files(path=directory,pattern = paste0("*",file_type),full.names = TRUE), from="UTF-8", to="LATIN1")

file_names = gsub(x = files,pattern = file_type ,replacement = "")

}

if(!isFALSE(Exclude)){
Exclude_names <- unlist(file_names)
Exclude_names <- Exclude_names[grep(Exclude,Exclude_names)]
  for(i in 1:length(files)){
  if(file_names[i] %in% Exclude_names) {
  files[i] <- NA
  file_dir <- NA
  file_names <- NA
  }
    files <- na.omit(files)
    file_dir <- na.omit(file_dir)
    file_names <- na.omit(file_names)
    }

}

  if(isTRUE(inList)){
    df_list <- rep(list(list()),length(file_names))
    names(df_list) <- file_names
  }


if(file_type==".csv"| file_type==".tsv" | file_type==".txt"){
  for(i in 1:length(files)){
    if(file_type==".csv"|file_type==".txt"){
      if(grepl( "UTF",Encoding, fixed = TRUE)){
       files_df <- read.csv(file_dir[i],fileEncoding=Encoding,sep = Deliminator)
    } else {
      files_df <- read.csv(file_dir[i],sep = Deliminator)
    }
      }

    if(file_type==".tsv") files_df <- read_tsv(file_dir[i])

    # Bind to a list or import files individually
    if(isFALSE(inList)){
      assign( x = file_names[i], value = files_df,.GlobalEnv)
    } else {
      df_list[[file_names[i]]] <- files_df
    }
  }

}  else if(file_type==".xls" | file_type==".xlsx") {

  for(i in 1:length(files)){
    sheets <- excel_sheets(file_dir[i])

  if(length(sheets)>1) {
    files_df <- lapply(sheets, function(X) read_excel(file_dir[i], sheet = X))
    files_df <- lapply(files_df, as.data.frame)
    names(files_df) <- sheets

    # Bind to a list or import files individually
    if(isFALSE(inList)){
      assign( x = file_names[i], value = files_df,.GlobalEnv)
    } else {
      df_list[[file_names[i]]] <- files_df
    }


  } else {
    files_df <- read_excel(file_dir[i])
    files_df <- files_df

    # Bind to a list or import files individually
    if(isFALSE(inList)){
      assign( x = file_names[i], value = files_df,.GlobalEnv)
    } else {
      df_list[[file_names[i]]] <- files_df
    }
  }
  }


  } else if(file_type==".sas") {

    for(i in 1:length(files)){
      files_df <- read_sas(file_dir[i])
      files_df <- files_df

      # Bind to a list or import files individually
      if(isFALSE(inList)){
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }


  }  else if(file_type==".sav") {

    for(i in 1:length(files)){
      files_df <- read_spss(file_dir[i])
      files_df <- files_df

      # Bind to a list or import files individually
      if(isFALSE(inList)){
        assign( x = file_names[i], value = files_df,.GlobalEnv)
      } else {
        df_list[[file_names[i]]] <- files_df
      }
    }
   } else {
  print("Currently, we do not support ",file_type," !")
   }


  if(isTRUE(inList)) assign( x = Meta_List_Name, value = df_list,.GlobalEnv)
############################################
###                 End                  ###
############################################

}
