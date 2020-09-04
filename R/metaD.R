#' Title
#'
#' @param Data Text
#' @param colnum1 Text
#' @param colnum2 Textdocu
#'
#' @return Text
#' @export
#'
#'
metaD <- function(Data, colnum1, colnum2) {
  meta.data <- Data[,-c(colnum1:colnum2)] # Extract all metadata (columns without the expresion values)
  return(meta.data)
}
