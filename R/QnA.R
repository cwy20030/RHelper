QnA <- function(options, MESSAGE,Others=FALSE) {


  # Add Not Sure or Others
  if (Others) options = c(options,"Other")


  # Print the options for the user
  for (i in 1:length(options)) {
    cat(paste(i, ": ", options[i], "\n", sep = ""))
  }

  # Request the user to select an option
  selected_option <- readline(prompt = paste0("Please select the ",MESSAGE," by entering its number.
Or, Escape <Esc> to abort."))

  # Check if the input is a number and within the range of options
  if (!grepl("^[0-9]+$", selected_option) | as.numeric(selected_option) > length(options) | as.numeric(selected_option) < 1) {
    cat("Invalid selection. Please try again.\n")
    return(QnA(options,MESSAGE))
  }

  if (grepl("Other",options[as.numeric(selected_option)])) options[as.numeric(selected_option)] <- readline(prompt = paste0("Please provide the ",MESSAGE,": "))



  # Return the selected option
  return(options[as.numeric(selected_option)])
}

