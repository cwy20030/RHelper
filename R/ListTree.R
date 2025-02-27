#  File RHelper/R/ListTree.R
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
#' @title ListTree
#'
#' @description
#' A function to print out the names of the sublist and their items
#'
#' @details
#' This function will loop over all the sublists and their items within a list and print out their names and structures.
#'
#'
#' @import rlang
#' @param object A list.
#' @param ... Internal parameter space holder
#' @export
#' @examplesIf interactive()
#'
#' # Create a Test List
#'
#' List = list(
#' sublist1 = list(a = 1, b = 2),
#' sublist2 = list(c = 3, d = list(e = 5, f = 6)),
#' sublist3 = list(g = 7, h = list(i = 8, j = list(k = 9)))
#' )
#'
#'
#' # list all names
#' ListTree(List)
#'
#'
#'

ListTree <- function(object, ...) {


  # Handling Hidden List ---------
    Level <- list2(...)
    if (length(Level) == 0) Level$Layer = 0


    if(Level$Layer == 0)   cat(deparse(substitute(object)), "\n")
    indent.str <- paste(rep.int("  ", Level$Layer), collapse = "")
    for (name in names(object)) {
      cat(indent.str, paste0("$", name), "\n")
      if (inherits(object[[name]],"list")) {
        ListTree(object[[name]], Layer = Level$Layer + 1)
      }

  }
}
