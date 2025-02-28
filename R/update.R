#  File RHelper/R/update.R
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
#' @title update
#'
#' @description
#' A function to check the latest release version of RHelper package and update it.
#'
#' @details
#' This function will check the latest version of RHelper and update if needed.
#'
#'
#' @importFrom devtools install_github
#' @importFrom utils packageVersion
#' @keywords Update, RHelper
#' @export
#'
#'


update = function(){

  # Get Description file for Version and URL---------
  Description = utils::packageDescription("RHelper")

  ## Local Version
  LocalVersion = Description$Version
  LocalVersion = as.numeric(gsub("\\.", "\\1",LocalVersion))

  # Check GitHub -------
  ## Repo URL
  ##### Currently it is on GitHub
  repo_url = Description$URL

  temp_file = tempfile()
  utils::download.file(paste0(repo_url, "/raw/master/DESCRIPTION"), temp_file)

  ## Version on GitHub
  GitHubDesc = readLines(temp_file)
  GitVersion = GitHubDesc[grepl("Version: ", GitHubDesc)]
  GitVersion = gsub("Version: ", "",GitVersion)
  GitVersion = as.numeric(gsub("\\.", "\\1",GitVersion))



  if (GitVersion > LocalVersion) {
    if ("package:RHelper" %in% search()) {
    detach("package:RHelper", unload = TRUE, character.only = TRUE)
    }
    devtools::install_github("cwy20030/RHelper", force = T)
  }



  if (GitVersion == LocalVersion)
    message("The latest version of RHelper is already installed.")

}
