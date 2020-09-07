#' Title
#'
#' @param dataset Text
#' @param col.samples Text
#'
#' @return Text
#' @export
#'
#' @import stats ClassDiscovery rafalib grDevices
samHC <- function(dataset, col.samples) {
  hcl.data <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  dataset, metric =  "pearson"), method = "ward.D2")
  qc.hcl.data <- rafalib::myplclust(hcl.data, labels = hcl.data$labels, lab.col = col.samples, main = "Sample Hierarchical Clustering", sub = "Review for potential outliers")
  true.qc.hcl <- grDevices::recordPlot()
  return(true.qc.hcl)
}
