#' RPKM/FPKM/TPM Tresholding
#'
#' The tresh function is used to curate the dataset to include only the features that meet the treshold criteria across all samples.
#' The treshold is user-defined based and relative to the expression values in the dataset as PRKM/FPKM/TPM.
#' This function does not perform transformations on the expresion values.
#'
#' @param dataAR A numeric data matrix with the analyis-ready feature expression values where row are features and columns are samples.
#'
#' @param treshold A numeric value with the desired treshold to apply to the dataset.
#'
#' @return A data frame with the individual p-values per feature for main effects, simple main effects and interaction effects.
#' @export
#'
#' @examples
tresh <- function(dataAR, treshold) {
  dataAR.tresh <- sapply(dataAR,
                         function(x) {ifelse(x < treshold, 1, 0)})
  dataAR.exclude <- rowSums(dataAR.tresh)
  dataAR.clean <- cbind(dataAR,
                        "exclude" = dataAR.exclude)
  dataAR.clean <- subset(dataAR.clean, dataAR.clean$exclude < 1)
  dataAR.clean$exclude <- NULL
  return(dataAR.clean)
}
