#' DatAR function
#'
#' This function extracts the data matrix m from an original data object. The data matrix m contains the expression values of all the features, as rows, from all the samples, as columns, as ordered for OBIF's input.
#' Hence, the data object "Data" must be specified including the first, "colnum1", and last, "colnum2, column number of the data matrix m within the original data.
#'
#' @param Data A data frame containing the original Omics data being studied by OBIF. As minimum components, it must include the data matrix m, and additional columns can include feature names or metadata as needed.
#' @param colnum1 A numeric value indicating the column number where the data matrix m starts. That is the column of the first unexposed sample.
#' @param colnum2 A numeric value indicating the column number where the data matrix m ends. That is the column of the last combined exposed sample.
#' @param n.ctrl A numeric value indicating the number of columns from the unexposed, control, samples in the data matrix m.
#' @param n.facA A numeric value indicating the number of columns from the single factor A exposed samples in the data matrix m.
#' @param n.facB A numeric value indicating the number of columns from the single factor B exposed samples in the data matrix m.
#' @param n.facAB A numeric value indicating the number of columns from the combined factor A + B exposed samples in the data matrix m.
#'
#' @return A data frame containing the unscaled original values of the data matrix m that will be used for analysis
#' @export
#'
#'
datAR <- function(Data, colnum1, colnum2, n.ctrl, n.facA, n.facB, n.facAB) {
  expr.val <- Data[,colnum1:colnum2] # Extract expression values
  cn.ctrl <- paste("ctrl.", seq(1:n.ctrl), sep = "")
  cn.facA <- paste("facA.", seq(1:n.facA), sep = "")
  cn.facB <- paste("facB.", seq(1:n.facB), sep = "")
  cn.facAB <- paste("facAB.", seq(1:n.facAB), sep = "")
  cn.expr.val <- c(cn.ctrl, cn.facA, cn.facB, cn.facAB)
  colnames(expr.val) <- cn.expr.val # Rename sample names by factor names
  return(expr.val)
}
