#  File RHelper/R/SandBox.R
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
#' @title SandBox
#'
#' @description
#' A Function to create a SandBox (i.e., a temporary environment) to perform code testing or data manipulation without affecting the Global environment
#'
#' @details
#' There are two main methods to use the SandBox.
#' For simple tasks (e.g., categorizing a variable or test a simple function), the function SandBox would be enough.
#' When performing more complex tasks, please, use PlayGround. Similar to the SandBox, the PlayGround also allows users
#' to commit changes to the Global Environment. When complete, use Close() to dissolve the PlayGround and save the memory.
#'
#' @import readr utils
#' @param Expr Any give length of codes to be executed. It is crucial to wrap {} around the code or code chunks.
#' @param Show A logical parameter to allow printing the results of each computation. <Default  = FALSE> Please, note that it is not recommended to use this parameter when computing/processing large matrices or data. The heavy demands to print the results may exhaust the memory.
#' @param Commit A logical indicator to specify whether to add the changes back to the Global Envrionment. This is particularly useful when data manipulations procedures (e.g., categorizing a continuous data)
#' @return Nothing unless committed
#' @keywords Import, Read
#' @export
#' @examples
#'
#' # Create a variable called x
#' x = 2
#'
#'
#' # Use the SandBox function to modify x in the sandbox environment and Commit the changes
#'
#' SandBox({
#'         x <- x + 1
#'         print(x)
#'        })
#'
#' print(x) # The original x value
#'
#'
#' SandBox({
#'         x <- x + 1
#'        },
#'        Commit = TRUE,
#'        Show = TRUE)
#'


SandBox <- function(Expr, Show = TRUE, Commit = FALSE) {

  # Create a new environment for the sandbox
  eSandBox <- new.env()

  # Evaluate the Expression in the sandbox environment
  result <- eval(Expr, envir = eSandBox)

  # Return the result
  if (Show) {
    return(result)
  }


  # If Commit is TRUE, assign all variables in eSandBox to the global environment
  if (Commit) {
    list2env(as.list(eSandBox), envir = .GlobalEnv)
  }
}

