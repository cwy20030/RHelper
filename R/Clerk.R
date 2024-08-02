#' A function to automatically document files' properties contained in the directery/path specified.
#'
#' This function lists and documents all files contained within one or more directories (i.e., computer paths to folders of interest) specified by the user. This is suitable if one wish to import files from multiple folders into a data.list for analysis.
#' @param Dirs Directory of where the files that you want to document <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @keywords Import, Read
#' @export
#' @examples
#'
#' R_USER = Sys.getenv("R_USER")
#' Clerk(R_USER)
#'
#'


Clerk <- function(Dirs, ... ) {



  ############################################
  ### List All files/folders in the Directories ###
  ############################################

  if (is.list(Dirs)) Dirs = unlist(Dirs)

  if (length(Dirs) > 1) Dirs = unique(Dirs)

  FolderCheck = file.info(Dirs)

  if (!any(FolderCheck$isdir))  Dirs[!FolderCheck$isdir] <- gsub("/[^/]*$", "", Dirs[!FolderCheck$isdir])
  Dirs = unique(Dirs)



  Out = lapply(Dirs, function(Directory) {

    files = iconv(list.files(path = Directory,full.names = TRUE), from = "UTF-8", to = "LATIN1")
    temp = data.frame(matrix(nrow = length(files),ncol = 4))
    names(temp) = c("File_Name","File_Type","Folder","File_Path")

    FileCheck = file.info(files)

    temp$File_Name = gsub("^.*/(.*?)\\..*$","\\1",files)

    temp$File_Type = gsub("^.*\\.(.*?)$","\\1",files)
    temp$File_Type[which(FileCheck$isdir)]  = "Folder"
    temp$File_Type[temp$File_Type == temp$File_Name] = ""

    if (any(temp$File_Type %in% c("","Folder")))
      temp$File_Name[temp$File_Type %in% c("","Folder")] = gsub("^.*/","",temp$File_Name[temp$File_Type %in% c("","Folder")])

    temp$File_Path = files
    temp$Folder = rep(gsub("^.*/","",Directory),length(files))

    temp
  })




  Out = do.call(rbind,Out)


  ############################################
  ###                 End                  ###
  ############################################

}
