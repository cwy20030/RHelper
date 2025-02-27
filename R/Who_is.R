#  File RHelper/R/Who_is.R
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
#
#' @title Who_is
#'
#' @description
#' A quick function to print the names of all elements within a specific type in the environment.
#'
#' @details
#' This function will print out the names of all data.frame, list, or functions in the environment.
#' It is particularly useful when wanting to save computer memory to optimize the use of R.
#'
#' @param Type a type of elements in your environment. <Options: "Data.frame", "Functions", or "List">
#' @param print_out to identify whether to print out the names of all elements within the environment. <default: FALSE, which will not be seen on your screen>.
#' @return A savable report with AIC index
#' @export
#' @examples
#' #Who_is("Data.frame") # List all data.frames in the global environment.
#'


Who_is <- function(Type = c("Data.frame","Function","List","Value"), print_out = FALSE ){
  if (Type == "Data.frame") {
  Out <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.data.frame(get(x)))]
} else if (Type == "Function") {
  Out <- ls(envir  = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))]
} else if (Type == "List") {
  exc <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.data.frame(get(x)))]
  Out <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.list(get(x)))]
  Out <- Out[!Out %in% exc]
} else {
  Out1 <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.character(get(x)))]
  Out2 <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.factor(get(x)))]
  Out3 <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) is.numeric(get(x)))]
  Out <- c(Out1,Out2,Out3)
}
if (print_out == TRUE) {
  print(Out)
} else {
  Out
}
}
