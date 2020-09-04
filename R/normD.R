#' Title
#'
#' @param expr.val Text
#' @param QCplot Text
#' @param col.samples Text
#'
#' @return Text
#' @export
#'
#'
normD <- function(expr.val, QCplot, col.samples) {
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

  # Evaluate data distribution as quality control
  melt.data.0 <- reshape2::melt(data.0, id.vars = NULL)
  melt.data.1 <- reshape2::melt(data.1, id.vars = NULL)
  melt.data.2 <- reshape2::melt(data.2, id.vars = NULL)
  melt.data.3 <- reshape2::melt(data.3, id.vars = NULL)
  melt.data.4 <- reshape2::melt(data.4, id.vars = NULL)
  melt.data.5 <- reshape2::melt(data.5, id.vars = NULL)
  melt.data.6 <- reshape2::melt(data.6, id.vars = NULL)
  melt.data.7 <- reshape2::melt(data.7, id.vars = NULL)

  #### Data set up for linear modeling of feature expression
  mtx.data.0 <- as.matrix(data.0)
  mtx.data.1 <- as.matrix(data.1)
  mtx.data.2 <- as.matrix(data.2)
  mtx.data.3 <- as.matrix(data.3)
  mtx.data.4 <- as.matrix(data.4)
  mtx.data.5 <- as.matrix(data.5)
  mtx.data.6 <- as.matrix(data.6)
  mtx.data.7 <- as.matrix(data.7)

  ## 1.A Using a violin plot
  qc.vp.data.0 <- ggplot2::ggplot(melt.data.0, ggplot2::aes(x = melt.data.0$variable, y = melt.data.0$value, color = melt.data.0$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.0", subtitle = deparse(substitute(Original))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.1 <- ggplot2::ggplot(melt.data.1, ggplot2::aes(x = melt.data.1$variable, y = melt.data.1$value, color = melt.data.1$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.1", subtitle = deparse(substitute(BgC))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.2 <- ggplot2::ggplot(melt.data.2, ggplot2::aes(x = melt.data.2$variable, y = melt.data.2$value, color = melt.data.2$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.2", subtitle = deparse(substitute(L2T))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.3 <- ggplot2::ggplot(melt.data.3, ggplot2::aes(x = melt.data.3$variable, y = melt.data.3$value, color = melt.data.3$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.3", subtitle = deparse(substitute(QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.4 <- ggplot2::ggplot(melt.data.4, ggplot2::aes(x = melt.data.4$variable, y = melt.data.4$value, color = melt.data.4$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.4", subtitle = deparse(substitute(BgC + L2T))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.5 <- ggplot2::ggplot(melt.data.5, ggplot2::aes(x = melt.data.5$variable, y = melt.data.5$value, color = melt.data.5$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.5", subtitle = deparse(substitute(BgC + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.6 <- ggplot2::ggplot(melt.data.6, ggplot2::aes(x = melt.data.6$variable, y = melt.data.6$value, color = melt.data.6$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.6", subtitle = deparse(substitute(L2T + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.vp.data.7 <- ggplot2::ggplot(melt.data.7, ggplot2::aes(x = melt.data.7$variable, y = melt.data.7$value, color = melt.data.7$variable)) + ggplot2::geom_violin() + ggplot2::geom_boxplot(width = 0.2) + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle("Data.7", subtitle = deparse(substitute(BgC + L2T + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")

  QCnorm.VP <- cowplot::plot_grid(qc.vp.data.0,
            qc.vp.data.1,
            qc.vp.data.2,
            qc.vp.data.3,
            qc.vp.data.4,
            qc.vp.data.5,
            qc.vp.data.6,
            qc.vp.data.7)

  # 1.B Using a density plot
  qc.dp.data.0 <- ggplot2::ggplot(melt.data.0, ggplot2::aes(melt.data.0$value, color = melt.data.0$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.0", subtitle = deparse(substitute(Original))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.1 <- ggplot2::ggplot(melt.data.1, ggplot2::aes(melt.data.1$value, color = melt.data.1$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.1", subtitle = deparse(substitute(BgC))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.2 <- ggplot2::ggplot(melt.data.2, ggplot2::aes(melt.data.2$value, color = melt.data.2$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.2", subtitle = deparse(substitute(L2T))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.3 <- ggplot2::ggplot(melt.data.3, ggplot2::aes(melt.data.3$value, color = melt.data.3$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.3", subtitle = deparse(substitute(QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.4 <- ggplot2::ggplot(melt.data.4, ggplot2::aes(melt.data.4$value, color = melt.data.4$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.4", subtitle = deparse(substitute(BgC + L2T))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.5 <- ggplot2::ggplot(melt.data.5, ggplot2::aes(melt.data.5$value, color = melt.data.5$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.5", subtitle = deparse(substitute(BgC + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.6 <- ggplot2::ggplot(melt.data.6, ggplot2::aes(melt.data.6$value, color = melt.data.6$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.6", subtitle = deparse(substitute(L2T + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.dp.data.7 <- ggplot2::ggplot(melt.data.7, ggplot2::aes(melt.data.7$value, color = melt.data.7$variable)) + ggplot2::geom_density() + ggplot2::scale_color_manual(values = col.samples) + ggplot2::ggtitle(label="Data.7", subtitle = deparse(substitute(BgC + L2T + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")

  QCnorm.DP <- cowplot::plot_grid(qc.dp.data.0,
            qc.dp.data.1,
            qc.dp.data.2,
            qc.dp.data.3,
            qc.dp.data.4,
            qc.dp.data.5,
            qc.dp.data.6,
            qc.dp.data.7)

  # 1.C Using a box plot
  qc.bp.data.0 <- ggplot2::ggplot(melt.data.0, ggplot2::aes(x = melt.data.0$variable, y = melt.data.0$value, color = melt.data.0$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.0", subtitle = deparse(substitute(Original))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.1 <- ggplot2::ggplot(melt.data.1, ggplot2::aes(x = melt.data.1$variable, y = melt.data.1$value, color = melt.data.1$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.1", subtitle = deparse(substitute(BgC))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.2 <- ggplot2::ggplot(melt.data.2, ggplot2::aes(x = melt.data.2$variable, y = melt.data.2$value, color = melt.data.2$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.2", subtitle = deparse(substitute(L2T))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.3 <- ggplot2::ggplot(melt.data.3, ggplot2::aes(x = melt.data.3$variable, y = melt.data.3$value, color = melt.data.3$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.3", subtitle = deparse(substitute(QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.4 <- ggplot2::ggplot(melt.data.4, ggplot2::aes(x = melt.data.4$variable, y = melt.data.4$value, color = melt.data.4$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.4", subtitle = deparse(substitute(BgC + L2T))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.5 <- ggplot2::ggplot(melt.data.5, ggplot2::aes(x = melt.data.5$variable, y = melt.data.5$value, color = melt.data.5$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.5", subtitle = deparse(substitute(BgC + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.6 <- ggplot2::ggplot(melt.data.6, ggplot2::aes(x = melt.data.6$variable, y = melt.data.6$value, color = melt.data.6$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.6", subtitle = deparse(substitute(L2T + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")
  qc.bp.data.7 <- ggplot2::ggplot(melt.data.7, ggplot2::aes(x = melt.data.7$variable, y = melt.data.7$value, color = melt.data.7$variable)) + ggplot2::geom_boxplot(notch=TRUE) + ggplot2::scale_color_manual(values = col.samples)  + ggplot2::ggtitle(label="Data.7", subtitle = deparse(substitute(BgC + L2T + QNm))) + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), legend.position = "none")

  QCnorm.BP <- cowplot::plot_grid(qc.bp.data.0,
            qc.bp.data.1,
            qc.bp.data.2,
            qc.bp.data.3,
            qc.bp.data.4,
            qc.bp.data.5,
            qc.bp.data.6,
            qc.bp.data.7)

  ## 1.D Using hierarchical clutersing
  hcl.data.0 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.0, metric =  "pearson"), method = "ward.D2")
  hcl.data.1 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.1, metric =  "pearson"), method = "ward.D2")
  hcl.data.2 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.2, metric =  "pearson"), method = "ward.D2")
  hcl.data.3 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.3, metric =  "pearson"), method = "ward.D2")
  hcl.data.4 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.4, metric =  "pearson"), method = "ward.D2")
  hcl.data.5 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.5, metric =  "pearson"), method = "ward.D2")
  hcl.data.6 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.6, metric =  "pearson"), method = "ward.D2")
  hcl.data.7 <- stats::hclust(ClassDiscovery::distanceMatrix(dataset =  data.7, metric =  "pearson"), method = "ward.D2")

  QCnorm.HC <- function(){
    graphics::par(mfrow = c(3,3))
    qc.hcl.data.0 <- rafalib::myplclust(hcl.data.0, labels = hcl.data.0$labels, lab.col = col.samples, main = "Data.0", sub = "Original")
    qc.hcl.data.1 <- rafalib::myplclust(hcl.data.1, labels = hcl.data.1$labels, lab.col = col.samples, main = "Data.1", sub = "BgC")
    qc.hcl.data.3 <- rafalib::myplclust(hcl.data.3, labels = hcl.data.3$labels, lab.col = col.samples, main = "Data.3", sub = "QNm")
    qc.hcl.data.4 <- rafalib::myplclust(hcl.data.4, labels = hcl.data.4$labels, lab.col = col.samples, main = "Data.4", sub = "BgC + L2T")
    qc.hcl.data.5 <- rafalib::myplclust(hcl.data.5, labels = hcl.data.5$labels, lab.col = col.samples, main = "Data.5", sub = "BgC + QNm")
    qc.hcl.data.7 <- rafalib::myplclust(hcl.data.7, labels = hcl.data.7$labels, lab.col = col.samples, main = "Data.7", sub = "BgC + L2T + QNm")
    graphics::par(mfrow = c(1,1))
    true.HC <- grDevices::recordPlot()
    return(true.HC)
  }

  normD.QC <- ifelse(QCplot == "VP", return(QCnorm.VP),
                    ifelse(QCplot == "DP", return(QCnorm.DP),
                           ifelse(QCplot == "BP", return(QCnorm.BP),
                                  ifelse(QCplot == "HC", return(QCnorm.HC), "Method not valid"))))
  return(normD.QC)
}
