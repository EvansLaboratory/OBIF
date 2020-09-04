#' RPKM/FPKM/TPM Tresholding
#'
#' The tresh function is used to curate the dataset to include only the features that meet the treshold criteria across all samples.
#' The treshold is user-defined based and relative to the expression values in the dataset as PRKM/FPKM/TPM.
#' This function does not perform transformations on the expresion values.
#'
#' @param treshold A numeric value with the desired treshold to apply to the dataset.
#' @param expr.val Text
#'
#' @return A data frame with the individual p-values per feature for main effects, simple main effects and interaction effects.
#' @export
#'
#'
tresh <- function(expr.val, treshold) {
  expr.val.tresh <- sapply(expr.val,
                         function(x) {ifelse(x < treshold, 1, 0)})
  expr.val.exclude <- rowSums(expr.val.tresh)
  expr.val.clean <- cbind(expr.val,
                        "exclude" = expr.val.exclude)
  expr.val.clean <- subset(expr.val.clean, expr.val.clean$exclude < 1)
  expr.val.clean$exclude <- NULL
  return(expr.val.clean)
}
