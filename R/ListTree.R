#' A function to print out the names of the sublist and their items
#'
#' This function will loop over all the sublists and their items within a list and print out their names and structures.
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
