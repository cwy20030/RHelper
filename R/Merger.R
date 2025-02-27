#  File RHelper/R/Merger.R
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
#' @title Merger
#'
#' @description
#' A Function to Automatically Merging and Order Variables
#'
#' @details
#' This function is a bulk merger for data.frames. Users can decide whether to merge based on the structure of the referencing data.frame or each individual data.frame within the data list. Users may wish to use the Auto_Importer function in the RHelper.
#'
#' @param df_list A list of data.frame names to merge. Make sure that the data.frame have been imported to your environment. If the user wishes to import data.frame in bulk in a directory, one can use the Auto_Importer function from the library. <e.g., c("df_1","df_2","df_3")>
#' @param Reference_df The name of the data.frame containing IDs or unique identifiers to merge and sort the merge data with. If not specified as default, the first data.frame in the df_list <default: NULL>
#' @param ReferenceID The name of the variable in the data.frame that the user wishes to use to merge all data.frame. Please, make sure that all data.frame specified in the data.list contains the same name as specified in the ReferenceID.
#' @param Assigned_Name The name for the new merge data.frame
#' @param sorting_row Reordering the data.frame based on the sequence of ID in the ReferenceID  <Default: TRUE, i.e., Sorting data.frame by the ReferenceID>
#' @param based A logical indicator determining whether to keep the duplicated columns (i.e., columns with the same names). If "Update" is passed, the columns with the shared name will be retrieved from the "df_list". Note that only the shared column from the last listed data.list will be kept when updating. When "Ref" (as Reference) is passed, only the column from the Reference_df will be kept. If "KA" (which stands for keeping all), is passed, both duplicated columns will be kept. In the latter scenario, the column with .x is the original column from the df_list and .y for the Reference_df.
#' @return export a merged data.frame to your working environment in R (i.e., Global Enviroment in R Studio)
#' @keywords merge
#' @export
#' @examples
#'
#' # Create Sample Data
#' data("iris")
#'
#' # Extract variable names
#' Variables = names(iris)
#'
#' # Add row.ID as the ID for each data point
#' iris$ID = row.names(iris)
#'
#'
#' # Splitting the original iris data.frame into 5 data.lists where each
#' # contains an unique variable + ID.
#' Test_list = lapply(Variables,function(x){ iris[c(x,"ID")] })
#'
#' # Assign names to each data.list
#' names(Test_list) = Variables
#'
#' # Merge all files
#' Merger(df_list = Test_list,
#'        ReferenceID = "ID" ,
#'        based = "Update",
#'        Assigned_Name = "iris1")
#'
#' # Check if all values are exactly the same
#' all(iris1[names(iris)] == iris)
#'



Merger <- function(df_list, Reference_df = NULL, ReferenceID, Assigned_Name, sorting_row = TRUE, based = "Update" ){



  Variables <- rep(list(list()),length(df_list))
  names(Variables) <- df_list

  for (i in names(df_list)) {
    Variables[[i]] <- unique(names(df_list[[i]]))
  }

  if (is.null(Reference_df)) {
    Reference_df <- df_list[[1]]
    df_list = df_list[-1]
    warning("No referencing data.frame was specified. Therefore, the first data.list will be used as the reference.")
  }

    Base_df <- Reference_df
    df_Target <- df_list



# Combiner --------------------------
  if (length(based) > 1) {
    based = based[1]
    warning(paste("More than one indicator were assigned to the parameter 'based'. Only the first one will be used.", based))
  }

  if (based == "Ref") {
    for (df in df_Target) {
      unique.vl <-  names(df)[!names(df) %in% names(Base_df)[!names(Base_df) %in% ReferenceID]]
      Base_df <- merge.data.frame(Base_df,df[unique.vl],by = ReferenceID,all = TRUE)
    }
  } else if (based == "Update") {
    for (df in df_Target) {
      unique.vl <-  names(Base_df)[!names(Base_df) %in% names(df)[!names(df) %in% ReferenceID]]
      Base_df <- merge.data.frame(Base_df[unique.vl],df,by = ReferenceID,all = TRUE)
    }
  } else if (based == "KA") {
    for (df in df_Target) {
      Base_df <- merge.data.frame(df,Base_df,by = ReferenceID,all = TRUE)
    }
  }


  # Sort ID -----------------------------
    Base_df <- Base_df[match(Reference_df[[ReferenceID]],Base_df[[ReferenceID]]),]



  assign(x = Assigned_Name, value = Base_df,.GlobalEnv)









}
