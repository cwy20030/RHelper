#  File RHelper/R/DirClass.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2020-2025  C. William Yao, PhD
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Check if a directory is a file or a folder and assign class
# @import utils
# @importFrom tools file_ext
# @param Dir Directory of where the files that you want to import (e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/"). If not specified, the default is set to the working directory.
# @return Directory class
#

DirClass = function(Dir){


  # Check Pre-requisite --------
  for (x in c("utils", "tools"))
    if (!requireNamespace(x, quietly = T)) {
      install.packages(x)
      requireNamespace(x, quietly = T)
    }

  # Check directory existance status
  if (all(!dir.exists(Dir) & !file.exists(Dir))) stop(paste0(Dir," cannot be found. Please, check the spelling.") )

  # If it is a directory
  if (file.info(Dir)$isdir) class(Dir) = "Folder"

  # If it is a file
  if (!file.info(Dir)$isdir) class(Dir) = tools::file_ext(Dir)



  return(Dir)

}
