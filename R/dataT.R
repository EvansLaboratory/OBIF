#' Title
#'
#' Text
#'
#' @param expr.val Text
#' @param method Text
#'
#' @return Text
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
