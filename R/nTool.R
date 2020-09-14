#' Sample number-based parameter making function for OBIF
#'
#' Text
#'
#' @param param Text
#' @param n.ctrl Text
#' @param n.facA Text
#' @param n.facB Text
#' @param n.facAB Text
#'
#' @return Text
#' @export
#'
nTool <- function(param, n.ctrl, n.facA, n.facB, n.facAB) {
  # Sample color palette
  col.samples <- c(rep("#000000", n.ctrl), rep("#8dd3c7", n.facA), rep("#fb8072", n.facB), rep("#80b1d3", n.facAB))
  # Sample column names
  cn.ctrl <- paste("ctrl.", seq(1:n.ctrl), sep = "")
  cn.facA <- paste("facA.", seq(1:n.facA), sep = "")
  cn.facB <- paste("facB.", seq(1:n.facB), sep = "")
  cn.facAB <- paste("facAB.", seq(1:n.facAB), sep = "")
  cn.expr.val <- c(cn.ctrl, cn.facA, cn.facB, cn.facAB)
  # One-way ANOVA-like design matrix  for Expression Analysis
  si.orig <- data.frame(Factors=sapply(cn.expr.val,function(x)substr(x,1,regexpr('\\.',x)[1]-1)))
  si.orig$Factors <- factor(si.orig$Factors, levels=c('ctrl','facA','facB','facAB'))
  si <- si.orig
  design <- stats::model.matrix(~1+si$Factors)
  colnames(design) <- levels(si$Factors)
  # One-way ANOVA-like contrast matrix for Contrast Analysis
  cont.mtx.des <- limma::makeContrasts(SME.facA = "facAB-facB", SME.facB = "facAB-facA", Inter.fAxfB = "facAB-facA-facB",  levels=design)
  # Two-way ANOVA-like design matrix  - Reciprocal -
  si.orig2x2 <- data.frame("facA" = c(rep("0", n.ctrl), rep("1", n.facA), rep("0", n.facB), rep("1", n.facAB)),
                           "facB" = c(rep("0", n.ctrl), rep("0", n.facA), rep("1", n.facB), rep("1", n.facAB)))
  si.orig2x2$facA <- factor(si.orig2x2$facA, levels=c("0", "1"))
  si.orig2x2$facB <- factor(si.orig2x2$facB, levels=c("0", "1"))
  si2x2 <- si.orig2x2
  design2x2 <- stats::model.matrix(~1+si2x2$facA*si2x2$facB)
  colnames(design2x2) <- cbind('ctrl','facA','facB','Inter')
  # Two-way ANOVA-like contrast matrix - Reciprocal -
  cont.mtx.des2x2 <- limma::makeContrasts(SME.facA = "Inter+facA", SME.facB = "Inter+facB", facAB = "Inter+facA+facB",  levels= design2x2)

  nParam <- ifelse(param == "color", return(col.samples),
                     ifelse(param == "names", return(cn.expr.val),
                            ifelse(param == "design", return(design),
                                   ifelse(param == "contrast", return(cont.mtx.des),
                                          ifelse(param == "design2x2", return(design2x2),
                                                 ifelse(param == "contrast2x2", return(cont.mtx.des2x2),
                                                        ifelse(param == "si", return(si), "Parameter not valid")))))))
}
