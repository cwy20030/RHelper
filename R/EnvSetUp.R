#  File RHelper/R/EnvSetUp.R
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
#
# Setting up Setting Files


EnvSetUp = function(){
  if (!exists("RHSetting",envir = .GlobalEnv)) {

    message("Initiating RHelper Settings...")

    assign("RHSetting",list(),envir = .GlobalEnv)

    message("'RHSetting' is now created.")

  }
}

