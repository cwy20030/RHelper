
# Check if the directory is a file or a folder
# @import utils
# @importFrom tools file_ext
# @param Dir Directory of where the files that you want to import (e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/"). If not specified, the default is set to the working directory.
# @return Directory class
#
#
DirChecker = function(Dir){


  # Check Pre-requisit --------
  for (x in c("utils", "tools"))
    if (!requireNamespace(x, quietly = T)) {
      install.packages(x)
      requireNamespace(x, quietly = T)
    }



  # If it is a directory
  if (file.info(Dir)$isdir) class(Dir) = "Folder"

  # If it is a file
  if (!file.info(Dir)$isdir) class(Dir) = tools::file_ext(Dir)



  return(Dir)

}
