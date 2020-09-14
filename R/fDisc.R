#' Feature Discovery by OBIF
#'
#' @param final.results Text
#' @param pval.co Text
#' @param pval.sel Text
#' @param l2fc.co Text
#'
#' @return Text
#' @export
#'
#'
fDisc <- function(final.results, l2fc.co, pval.co, pval.sel) {
  # Label features with by DEM per group using pval
  DEMs.pval <- data.frame(
    A.DEM = ifelse (abs(final.results$d.facA) >= l2fc.co & final.results$pval.exp.facA <= pval.co,"Yes", "No"),
    B.DEM = ifelse (abs(final.results$d.facB) >= l2fc.co & final.results$pval.exp.facB <= pval.co,"Yes", "No"),
    AB.DEM = ifelse (abs(final.results$d.facAB) >= l2fc.co & final.results$pval.exp.facAB <= pval.co, "Yes", "No"))

  # Label features with by DEM per group using BH
  DEMs.BH <- data.frame(
    A.DEM = ifelse (abs(final.results$d.facA) >= l2fc.co & final.results$adjpvBH.exp.facA <= pval.co,"Yes", "No"),
    B.DEM = ifelse (abs(final.results$d.facB) >= l2fc.co & final.results$adjpvBH.exp.facB <= pval.co,"Yes", "No"),
    AB.DEM = ifelse (abs(final.results$d.facAB) >= l2fc.co & final.results$adjpvBH.exp.facAB <= pval.co, "Yes", "No"))

  # Label features with by DEM per group using Bonf
  DEMs.Bonf <- data.frame(
    A.DEM = ifelse (abs(final.results$d.facA) >= l2fc.co & final.results$adjpvBonf.exp.facA <= pval.co,"Yes", "No"),
    B.DEM = ifelse (abs(final.results$d.facB) >= l2fc.co & final.results$adjpvBonf.exp.facB <= pval.co,"Yes", "No"),
    AB.DEM = ifelse (abs(final.results$d.facAB) >= l2fc.co & final.results$adjpvBonf.exp.facAB <= pval.co, "Yes", "No"))

  # Error in pval.sel
  DEMs.Err <- data.frame(Err.DEM = paste("Err.DEM.", seq(1:nrow(final.results)), sep = ""))

  # Merge selected classification by pval.sel
  final.labels <- cbind.data.frame(final.results,
                                   ifelse(pval.sel == "pval", DEMs.pval,
                                          ifelse(pval.sel == "BH", DEMs.BH,
                                                 ifelse(pval.sel == "Bonf", DEMs.Bonf, DEMs.Err))))

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
  final.labels$f.IS.iDEMs <- ifelse (final.labels$AB.iDEM == "Yes",
                                     paste("iDEM.", final.labels$DoubleIS, sep = ""), "non-iDEM")

  return(final.labels)
}
