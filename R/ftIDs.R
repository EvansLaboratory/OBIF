#' Unique feature IDs
#'
#' The ftIDs function is a function to validate the use of unique identifiers for each features in your original dataset.
#' To test if a desired column "colnum" in your dataset is comprised of unique identifiers, use "method = 0".
#' If the result is exactly 1, then you can use that column as your feature IDs for analysis. Proceed to create your feature IDs list with "method = 1".
#' If the result is not exactly < 1, then switch to "method = 2" to create a generic unique feature ID. Your original feature names are saved as part of your metadata regardless of the method.
#'
#'
#' @param Data A dataframe of your original feature expression values where row are features and columns are samples. Metada is supported per feature in additional columns.
#'
#' @param colnum A numeric value with the number of the column that you want to check/use to esablish your unique feature IDs.
#'
#' @param method A numeric value specifying the unique feature IDs method to use. Use "method = 0" to verify if your "colnum" feature name column is valid. If so, re-run using "method = 1". If not use "method = 2".
#'
#' @return A character vector with the same new unique feature IDs when using method 1 or 2. A single numeric value between 0 to 1 to evaluate your feature names when using method 0
#' @export
#'
#'
ftIDs <- function(Data, colnum, method) {
  test.IDs <- length(unique(Data[colnum,])) / nrow(Data)
  ori.IDs <- Data[[colnum]]
  uni.IDs <- paste("feat.", seq(1:nrow(Data)), sep = "")
  Feat.ID <- ifelse(method == 0, return(test.IDs),
                   ifelse(method == 1, return(ori.IDs),
                          ifelse(method == 2, return(uni.IDs), "Method not valid")))
  return(Feat.ID)
}
