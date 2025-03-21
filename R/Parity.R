#  File RHelper/R/Parity.R
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
#' @title Parity
#'
#' @description
#' A function to identify the parity of a number (i.e., even or odd)
#'
#' @details
#' This function will check if a number is odd or even. By using the switch subfunction, users may change the Parity of the number (by adding/subtracting 1).
#'
#'
#' @param num a number
#' @param goal a logical operater to ensure that the num is/will be "odd" or "even". This will overwrite the manual switch function.
#' @param switch converting a number from even to odd and vice versa.
#' @param method an operater indicating either to add ("add") or subtract ("minus") 1 from the number to switch the Parity of the number
#' @return If \code{switch} is FALSE, returns a the number's class and Parity. If \code{switch} is TRUE, returns a new number of the opposite Parity. If \code{goal} is set to "odd" or "even", ensure the number to be the designated parity.
#' @keywords even, odd, Parity
#' @export
#' @examplesIf interactive()
#' # example code
#'
#'
#' # Generate an array of possible numbers for data length
#' Pool = 1000:12000
#'
#' # Generate an array of possible numbers for distribution parameters
#' Parameter = 1:9
#'
#'
#' # Randomly select one number from the pool
#' n = sample(Pool, size = 1)
#'
#' # randomly select shape and scale parameters
#' p1 = sample(Parameter, size = 1)
#' p2 = sample(Parameter, size = 1)
#'
#'
#' # Generate a random sample
#' data = rgamma(n, shape = p1, scale = p2)
#'
#' # Try smoothing the data with the medians using moving window via zoo
#' library("lattice")
#' library("zoo")
#'
#' #' ## the length of the window
#' K = floor(n * 0.1)
#'
#' #Identify the Parity
#' Parity(K)
#'
#' # <Note> In zoo, an odd number of window length can often prevent issues when
#' # using moving window.
#'
#' ## To make sure that we can stir up the pot, make sure K is an even number
#' K = Parity(K, goal = "even")
#'
#' # pad the front and the back
#' Head = rep(data[1],K)
#' Tail = rep(data[length(data)],K)
#'
#' dataNew = c(Head,data,Tail)
#'
#' ## Rolling median
#' length(rollmedian(dataNew, k = K)) == length(data)
#'
#' ### If K is odd number then the length of the data would
#' ### likely not be the same as the original data.
#'
#' # To ensure that the window length is odd, set goal to "odd"
#' K = Parity(K, goal = "odd")
#'
#' # Now rerun rollmedian again






Parity = function(num, goal = NULL, switch = FALSE, method = "add"){

  if (!inherits(num,c("numeric","integer"))) stop("num has to be a number.")
  if (!floor(num) == num) stop(paste0("decimal points detected in ",num,"!"))

  # Clear class history
  if (inherits(num,c("even","odd"))) class(num) = class(num)[!class(num) %in% c("even","odd")]

  if (inherits(num,"numeric")) num = as.integer(num)


  # Parity -------
  if (num %% 2 == 1) { # Odd Number

    class(num) = c(class(num), "odd")

  } else { # Even Number

    class(num) = c(class(num), "even")

  }


  # goal ---------
  if(!is.null(goal))
    ## Overwrite Control: switch function
    if(!inherits(num,goal)) switch = TRUE




  # Switch Function -----------
  if (isTRUE(switch)) {

  ## Overwrite Control: Always do add when the num is 0
  if (num == 0) method = "add"


    ## Add 1
  if (method == "add") num = num + 1
    ## Subtract 1
  if (method == "minus") num = num - 1

  class(num) = Parity(num)

  return(num)


  } else if(!is.null(goal)) {

  return(num)

  } else {

  return(class(num))

  }




}

