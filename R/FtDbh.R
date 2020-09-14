#' Feature Discovery with BH-method by OBIF
#'
#' @param final.results Text
#' @param l2fc.co Text
#' @param pval.co Text
#'
#' @return Text
#' @export
#'
#'
FtDbh <- function(final.results, l2fc.co, pval.co) {
  # Label features with by DEM per group using pval
  final.labels <- final.results
  final.labels$A.DEM <- ifelse(abs(final.labels$d.facA) >= l2fc.co & final.labels$adjpvBH.exp.facA <= pval.co,"Yes", "No")
  final.labels$B.DEM <- ifelse(abs(final.labels$d.facB) >= l2fc.co & final.labels$adjpvBH.exp.facB <= pval.co,"Yes", "No")
  final.labels$AB.DEM <- ifelse(abs(final.labels$d.facAB) >= l2fc.co & final.labels$adjpvBH.exp.facAB <= pval.co,"Yes", "No")

  # Label your AB.DEM by factorial effects
  final.labels$AB.SME.A <- ifelse (final.labels$AB.DEM == "Yes" & final.labels$pval.con.SME.facA <= 0.05,
                                   "Yes", "No")
  final.labels$AB.SME.B <- ifelse (final.labels$AB.DEM == "Yes" & final.labels$pval.con.SME.facB <= 0.05,
                                   "Yes", "No")
  final.labels$AB.iDEM <- ifelse (final.labels$AB.DEM == "Yes" & final.labels$pval.con.Inter.fAxfB <= 0.05,
                                  "Yes", "No")

  # Label your AB.DEM by expression profiles
  final.labels$EPs <- ifelse (final.labels$UDDualTx == "Concordant.Up", "I",
                              ifelse (final.labels$UDDualTx == "Concordant.Down", "II",
                                      ifelse (final.labels$UDDualTx == "Discordant.Up", "III",
                                              ifelse (final.labels$UDDualTx == "Discordant.Down", "IV",
                                                      ifelse (final.labels$UDDualTx == "facA-Dominant.Up", "V",
                                                              ifelse (final.labels$UDDualTx == "facA-Dominant.Down", "VI",
                                                                      ifelse (final.labels$UDDualTx == "facB-Dominant.Up", "VII",
                                                                              ifelse (final.labels$UDDualTx == "facB-Dominant.Down", "VIII",
                                                                                      "Error.No.EP"))))))))

  # Label your AB.DEM by iDEMs
  final.labels$f.IS <- final.labels$AbsIScore
  final.labels$f.IS.iDEMs <- ifelse (final.labels$AB.iDEM == "Yes", final.labels$f.IS, log2(1))

  final.labels$iDEMs <- ifelse (final.labels$AB.iDEM == "Yes",
                                paste("iDEM.", final.labels$DoubleIS, sep = ""), "non-iDEM")

  return(final.labels)
}
