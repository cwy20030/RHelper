VControl = function(Directory,dfName,extension,category){

  Directory = DirSetting(Directory)

  VersionDir = paste0(Directory,"Version History/",category,"/")
  if(!dir.exists(VersionDir))
    dir.create(VersionDir)

  Date = Name_Checker(Sys.Date(),Silent = T)

  file.rename(from = paste0(Direcotry,dfName,extension),
              to = paste0(VersionDir,paste0(dfName,"_",Date),extension))

}
