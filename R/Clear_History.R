Clear_History <- function(Directory) {

  Directory <- paste0(Directory,"/History")

  if(dir.exists(Directory)==FALSE) dir.create(Directory)



  File_Name <- paste0(Sys.Date(),".txt")
  library(readr)
  if(File_Name %in% list.files(Directory)){
    History <- read.delim(paste0(Directory,"/",File_Name),header = FALSE)
    History[[1]] <- as.character(History[[1]])
    History[nrow(History)+2,] <- as.character(timestamp())


    savehistory(file=paste0(Directory,"/","2_",File_Name))
    History2 <- read.delim(paste0(Directory,"/","2_",File_Name),header = FALSE)
    History2[[1]] <- as.character(History2[[1]])
    History <- rbind(History, History2)
    write_delim(History, paste0(Directory,"/",File_Name),col_names = FALSE)

    file.remove(paste0(Directory,"/","2_",File_Name))

  } else {
    savehistory(file=paste0(Directory,"/",File_Name))
  }

  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
