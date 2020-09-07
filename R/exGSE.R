#' Title
#'
#' @param final.labels Text
#' @param col.ft.name Text
#' @param dataAR Text
#'
#' @return Text
#' @export
#'
#'
exGSE <- function(final.labels, col.ft.name, dataAR) {
  all.na <- data.frame (Description = rep("na", length(final.labels$featIDs)))
  exGSE.res.1 <- data.frame(Name = final.labels[,col.ft.name],
                            Description = all.na,
                            dataAR)
  return(exGSE.res.1)
}
