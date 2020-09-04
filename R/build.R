#' Title
#'
#' @param featIDs Text
#' @param meta.data Text
#' @param dataAR Text
#' @param obffa.res Text
#' @param synex.res Text
#'
#' @return Text
#' @export
#'
build <- function(featIDs, meta.data, dataAR, obffa.res, synex.res) {
  final.res <- data.frame(featIDs, meta.data, dataAR, obffa.res, synex.res)
  return(final.res)
}
