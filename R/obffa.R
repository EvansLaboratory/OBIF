obffa <- function(dataAR, design, contrast) {
  fit <- limma::lmFit(dataAR, design)
  fit.ebayes <- limma::eBayes(fit)
  adj.Pvalues.lmBH <- apply(fit.ebayes$p.value, 2, stats::p.adjust, method="BH")
  adj.Pvalues.lmBonf <- apply(fit.ebayes$p.value, 2, stats::p.adjust, method="bonferroni")
  fit2 <- limma::contrasts.fit(fit,contrast)
  fit2.ebayes <- limma::eBayes(fit2)
  adj.Pvalues.contrBH <- apply(fit2.ebayes$p.value, 2, stats::p.adjust, method="BH")
  adj.Pvalues.contrBonf <- apply(fit2.ebayes$p.value, 2, stats::p.adjust, method="bonferroni")
  res.ffa <- data.frame(pval.exp=fit.ebayes$p.value[,2:4],
                        pval.con=fit2.ebayes$p.value,
                        adjpvBH.exp=adj.Pvalues.lmBH[,2:4],
                        adjpvBH.con=adj.Pvalues.contrBH,
                        adjpvBonf.exp=adj.Pvalues.lmBonf[,2:4],
                        adjpvBonf.con=adj.Pvalues.contrBonf)
  return(res.ffa)
}
