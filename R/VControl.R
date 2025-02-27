#  File RHelper/R/VControl.R
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
VControl = function(Dir, dfName, extension, category){

  if(is.null(Dir))  Dir = DirSetting(Dir)

  VersionDir = paste0(Dir,"Version History/",category,"/")
  if (!dir.exists(VersionDir))
    dir.create(VersionDir)

  Date = Name_Checker(Sys.Date(),Silent = T)

  file.rename(from = paste0(Dir,dfName,extension),
              to = paste0(VersionDir,paste0(dfName,"_",Date),extension))

}
