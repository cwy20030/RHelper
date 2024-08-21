#' A function to check the latest release version of the package and update it.
#'
#' This function will check the latest version of RHelper and update if needed.
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
    detach("package:RHelper", unload = TRUE)
    devtools::install_github("cwy20030/RHelper", force = T)
  }



  if (GitVersion == LocalVersion)
    message("The latest version of RHelper is already installed.")

}
