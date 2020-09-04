#' Title
#'
#' Text
#'
#' @param Data Text
#' @param colnum1 Text
#' @param colnum2 Text
#' @param n.ctrl Text
#' @param n.facA Text
#' @param n.facB Text
#' @param n.facAB Text
#'
#' @return Text
#' @export
#'

fExpr <- function(Data, colnum1, colnum2, n.ctrl, n.facA, n.facB, n.facAB) {
  expr.val <- Data[,colnum1:colnum2] # Extract expression values
  cn.ctrl <- paste("ctrl.", seq(1:n.ctrl), sep = "")
  cn.facA <- paste("facA.", seq(1:n.facA), sep = "")
  cn.facB <- paste("facB.", seq(1:n.facB), sep = "")
  cn.facAB <- paste("facAB.", seq(1:n.facAB), sep = "")
  cn.expr.val <- c(cn.ctrl, cn.facA, cn.facB, cn.facAB)
  colnames(expr.val) <- cn.expr.val # Rename sample names by factor names
  return(expr.val)
}
