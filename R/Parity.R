#' Identify the parity of a number (i.e., even or odd)
#'
#' This function will check if a number is odd or even. By using the switch subfunction, users may change the Parity of the number (by adding/subtracting 1).
#' @param num a number
#' @param goal a logical operater to ensure that the num is/will be "odd" or "even". This will overwrite the manual switch function.
#' @param switch converting a number from even to odd and vice versa.
#' @param method an operater indicating either to add ("add") or subtract ("minus") 1 from the number to switch the Parity of the number
#' @return If \code{switch} is FALSE, returns a the number's class and Parity. If \code{switch} is TRUE, returns a new number of the opposite Parity. If \code{goal} is set to "odd" or "even", ensure the number to be the designated parity.
#' @keywords even, odd, Parity
#' @export
#' @examples
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
#' library("zoo")
#'
#' #' ## the length of the window
#' K = floor(length(n) * 0.1)
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

  if (!floor(num) == num) stop(paste0("decimal points detected in ",num,"!"))
  if (class(num) %in% c("numeric")) num = as.integer(num)


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


  return(num)


  } else if(!is.null(goal)) {

  return(num)

  } else {

  return(class(num))

  }




}
