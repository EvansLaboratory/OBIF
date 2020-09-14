#' Export to IPA
#'
#' @param final.labels Text
#' @param col.ft.name Text
#' @param obffa.res Text
#' @param FtD.method Text
#'
#' @return Text
#' @export
#'
#'
exIPA <- function(final.labels, col.ft.name, obffa.res, FtD.method) {
  all.sel <- data.frame (p.sel = rep(0.01, length(final.labels$featIDs)))
  exIPA.res <- data.frame(feat.id = final.labels[,col.ft.name],
                          #A.DEMs
                          d.log2FC.A = final.labels$d.facA,
                          p.val.A = obffa.res[,FtD.method+1],
                          p.sel.A = all.sel,
                          #B.DEMs
                          d.log2FC.B = final.labels$d.facB,
                          p.val.B = obffa.res[,FtD.method+2],
                          p.sel.B = all.sel,
                          #AB.DEMs
                          d.log2FC.AB = final.labels$d.facAB,
                          p.val.AB = obffa.res[,FtD.method+3],
                          p.sel.AB = all.sel,
                          #EP.I
                          d.log2FC.EP1 = final.labels$d.facAB,
                          p.val.EP1 = obffa.res[,FtD.method+3],
                          p.sel.EP1 = c(ifelse(final.labels$EPs == "I", 0.01, 1)),
                          #EP.II
                          d.log2FC.EP2 = final.labels$d.facAB,
                          p.val.EP2 = obffa.res[,FtD.method+3],
                          p.sel.EP2 = c(ifelse(final.labels$EPs == "II", 0.01, 1)),
                          #EP.III
                          d.log2FC.EP3 = final.labels$d.facAB,
                          p.val.EP3 = obffa.res[,FtD.method+3],
                          p.sel.EP3 = c(ifelse(final.labels$EPs == "III", 0.01, 1)),
                          #EP.IV
                          d.log2FC.EP4 = final.labels$d.facAB,
                          p.val.EP4 = obffa.res[,FtD.method+3],
                          p.sel.EP4 = c(ifelse(final.labels$EPs == "IV", 0.01, 1)),
                          #EP.V
                          d.log2FC.EP5 = final.labels$d.facAB,
                          p.val.EP5 = obffa.res[,FtD.method+3],
                          p.sel.EP5 = c(ifelse(final.labels$EPs == "V", 0.01, 1)),
                          #EP.VI
                          d.log2FC.EP6 = final.labels$d.facAB,
                          p.val.EP6 = obffa.res[,FtD.method+3],
                          p.sel.EP6 = c(ifelse(final.labels$EPs == "VI", 0.01, 1)),
                          #EP.VII
                          d.log2FC.EP7 = final.labels$d.facAB,
                          p.val.EP7 = obffa.res[,FtD.method+3],
                          p.sel.EP7 = c(ifelse(final.labels$EPs == "VII", 0.01, 1)),
                          #EP.VIII
                          d.log2FC.EP8 = final.labels$d.facAB,
                          p.val.EP8 = obffa.res[,FtD.method+3],
                          p.sel.EP8 = c(ifelse(final.labels$EPs == "VIII", 0.01, 1)),
                          #EP.Concordant
                          d.log2FC.EPConc = final.labels$d.facAB,
                          p.val.EPConc = obffa.res[,FtD.method+3],
                          p.sel.EPConc = c(ifelse(final.labels$DualTx == "Concordant" , 0.01, 1)),
                          #EP.Discordant
                          d.log2FC.EPDisc = final.labels$d.facAB,
                          p.val.EPDisc = obffa.res[,FtD.method+3],
                          p.sel.EPDisc = c(ifelse(final.labels$DualTx == "Discordant" , 0.01, 1)),
                          #EP.A-Dominant
                          d.log2FC.EPADom = final.labels$d.facAB,
                          p.val.EPADom = obffa.res[,FtD.method+3],
                          p.sel.EPADom = c(ifelse(final.labels$DualTx == "facA-Dominant" , 0.01, 1)),
                          #EP.B-Dominant
                          d.log2FC.EPBDom = final.labels$d.facAB,
                          p.val.EPBDom = obffa.res[,FtD.method+3],
                          p.sel.EPBDom = c(ifelse(final.labels$DualTx == "facB-Dominant" , 0.01, 1)),
                          #EP.Cooperative
                          d.log2FC.EPCoop = final.labels$d.facAB,
                          p.val.EPCoop = obffa.res[,FtD.method+3],
                          p.sel.EPCoop = c(ifelse(final.labels$SingleTx == "Cooperative" , 0.01, 1)),
                          #EP.Competitive
                          d.log2FC.EPComp = final.labels$d.facAB,
                          p.val.EPComp = obffa.res[,FtD.method+3],
                          p.sel.EPComp = c(ifelse(final.labels$SingleTx == "Competitive" , 0.01, 1)),
                          #EP.iDEMs
                          d.log2FC.iDEMs = final.labels$d.facAB,
                          p.val.iDEMs = obffa.res[,FtD.method+3],
                          p.sel.iDEMs = obffa.res$pval.con.Inter.fAxfB,
                          #EP.iDEMs-Synergistic
                          d.log2FC.iSyn = final.labels$d.facAB,
                          p.val.iSyn = obffa.res[,FtD.method+3],
                          p.sel.iSyn = c(ifelse(final.labels$DoubleIS == "Synergistic.Dual" & obffa.res$pval.con.Inter.fAxfB < 0.05, 0.01, 1)),
                          #EP.iDEMs-Antagonistic
                          d.log2FC.iAnt = final.labels$d.facAB,
                          p.val.iAnt = obffa.res[,FtD.method+3],
                          p.sel.iAnt = c(ifelse(final.labels$DoubleIS == "Antagonistic.Dual" & obffa.res$pval.con.Inter.fAxfB < 0.05, 0.01, 1))
  )
  return(exIPA.res)
}
