#' DataT function
#'
#' This function transforms the un-scaled original expression values from the data matrix m into the chosen OBIF-scaled expression values that will be used for Full Factorial Analysis. The data matrix m must have already been extracted from the original Omics data using either the DatAR or fExpr functions.
#' The scaling method should've already been chosen from the results of the analys of the function normD. The data matrix m should not have any outliers identified by HCsam, and if so, reanalysis with norm D is required to choose the best data scaling.
#' The scaling methods include: 0, original; 1, background correction only; 2, log2 transformation only; 3, quantile normalization only; 4, background correction + log2 transformation; 5, background correction + quantile normalization; 6, log2 transformation + quantile normalization; 7, background correction + log2 transformation + quantile normalization.
#'
#' @param expr.val A data frame containing the unscaled original values of the data matrix m that will be used for analysis.
#' @param method A numeric value indicating the scaling strategy of the data matrix m as indicated above.
#'
#' @return A data frame containing the OBIF-scaled values of the data matrix m that will be used for Full Factorial Analysis.
#' @export
#'
#'
dataT <- function(expr.val, method) {
  # Create transformed and scaled datasets
  data.0 <- methods::new("ExpressionSet", exprs=as.matrix(expr.val))
  data.1 <- lumi::lumiB(data.0, method = "bgAdjust.affy")
  data.2 <- log2(expr.val)
  data.3 <- preprocessCore::normalize.quantiles(data.matrix(expr.val))
  data.4 <- lumi::lumiT(data.1, "log2")
  data.5 <- lumi::lumiN(data.1, method = "quantile")
  data.6 <- preprocessCore::normalize.quantiles(data.matrix(data.2))
  data.7 <- lumi::lumiN(data.4, method = "quantile")

  # Turned datasets into dataframes
  data.0 <- Biobase::exprs(data.0)
  data.0 <- as.data.frame(data.0)
  data.1 <- Biobase::exprs(data.1)
  data.1 <- as.data.frame(data.1)
  data.2 <- as.data.frame(data.2)
  data.3 <- as.data.frame(data.3)
  data.4 <- Biobase::exprs(data.4)
  data.4 <- as.data.frame(data.4)
  data.5 <- Biobase::exprs(data.5)
  data.5 <- as.data.frame(data.5)
  data.6 <- as.data.frame(data.6)
  data.7 <- Biobase::exprs(data.7)
  data.7 <- as.data.frame(data.7)

  data.AR <- ifelse(method == 0, return(data.0),
                    ifelse(method == 1, return(data.1),
                           ifelse(method == 2, return(data.2),
                                  ifelse(method == 3, return(data.3),
                                         ifelse(method == 4, return(data.4),
                                                ifelse(method == 5, return(data.5),
                                                       ifelse(method == 6, return(data.6),
                                                              ifelse(method == 7, return(data.7),
                                                                     "Method not valid"))))))))
  return(data.AR)
}
