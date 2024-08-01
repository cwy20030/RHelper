read_excel_allsheets <- function(filename, tibble = FALSE) {

  # Check Pre-requisit --------
  for(x in c("readxl"))
    if(!requireNamespace(x, quietly = T)){
      install.packages(x)
      requireNamespace(x, quietly = T)
    }


  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
