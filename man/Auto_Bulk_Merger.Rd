% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Auto_Bulk_Merger.R
\name{Auto_Bulk_Merger}
\alias{Auto_Bulk_Merger}
\title{A Function to Automatically Merging and Order Variables}
\usage{
Auto_Bulk_Merger(
  df_list,
  Reference_df = NULL,
  Reference,
  sorting_row = TRUE,
  based = c("Foward", "Backward", "KA"),
  Assigned_Name,
  ...
)
}
\arguments{
\item{df_list}{A list of data.frame names to merge. Make sure that the data.frame have been imported to your environment. If the user wishes to import data.frame in bulk in a directory, one can use the Auto_Importer function from the library. <e.g., c<"df_1","df_2","df_3">, ***Replace "<>"  with parentheses>}

\item{Reference_df}{The name of the data.frame containing IDs or unique identifiers to merge and sort the merge data with. If not specified as default, the first data.frame in the df_list <default: NULL>}

\item{Reference}{The name of the variable in the data.frame that the user wishes to use to merge all data.frame. Please, make sure that all data.frame specified in the data.list contains the same name as specified in the Reference.}

\item{sorting_row}{Reordering the data.frame based on the sequence of ID in the Reference  <Default: TRUE, i.e., Sorting data.frame by the Reference>}

\item{based}{A logical indicator determining whether to keep the duplicated columns (i.e., columns with the same names). If "Forward" is passed, the columns with the shared name will be retrieved from the "df_list", vice versa. If "KA" is passed, both duplicated columns will be kept. In the latter scenario, the column with .x is the original column from the df_list and .y for the Reference_df.}

\item{Assigned_Name}{The name for the new merge data.frame}
}
\value{
Import a merged data.frame into your working environment in R (i.e., Global Enviroment in R Studio)
}
\description{
This function is a bulk merger for data.frames. Users can decide whether to merge based on the structure of the reference data.frame or each individual data.frame within the data list. Users may wish to use the Auto_Importer function in the RHelper.
}
