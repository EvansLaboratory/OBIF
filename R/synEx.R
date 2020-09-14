#' Synergy-labeled Expression value calculations
#'
#' Text
#'
#' @param dataAR Text
#' @param si Text
#'
#' @return Text
#' @export
#'
synEx <- function(dataAR, si) {
  ints.mtx.arr <- as.matrix(dataAR)

  # Expression value calculations
  m.l2.vals <- data.frame(m.ctrl = apply(ints.mtx.arr[, si$Factors == "ctrl"], 1, mean),
                          m.facA = apply(ints.mtx.arr[, si$Factors == "facA"], 1, mean),
                          m.facB = apply(ints.mtx.arr[, si$Factors == "facB"], 1, mean),
                          m.facAB = apply(ints.mtx.arr[, si$Factors == "facAB"], 1, mean))
  d.log2FC <- data.frame(d.facA = m.l2.vals[, "m.facA"] - m.l2.vals[, "m.ctrl"],
                         d.facB = m.l2.vals[, "m.facB"] - m.l2.vals[, "m.ctrl"],
                         d.facAB = m.l2.vals[, "m.facAB"] - m.l2.vals[, "m.ctrl"])

  # Modified Combination Index calculations
  modCI <- cbind.data.frame(RawCI = (d.log2FC[,3])/(d.log2FC[,1]+d.log2FC[,2]),
                            AbsCI = abs((d.log2FC[,3])/(d.log2FC[,1]+d.log2FC[,2])))

  # OBIF Interaction Score calculations
  IntScore <- data.frame(AbsIScore = log2(modCI$AbsCI))
  IntScore$DoubleIS <- ifelse(IntScore$AbsIScore > 0, "Synergistic",
                          ifelse(IntScore$AbsIScore < 0, "Antagonistic", "Error1"))

  # Expression Profiles categorization
  # Downregulated or Upregulated per treatment
  ExpProf <- data.frame(UD.A = ifelse (d.log2FC[,1] < 0 , "Down", ifelse ( d.log2FC[,1] > 0, "Up", "Unchanged")),
                        UD.B = ifelse ( d.log2FC[,2] < 0 , "Down", ifelse ( d.log2FC[,2] > 0, "Up", "Unchanged")),
                        UD.AB = ifelse ( d.log2FC[,3] < 0 , "Down", ifelse ( d.log2FC[,3] > 0, "Up", "Unchanged")))
  # Assign & Verify SingleTx group
  ExpProf$SingleTx <- ifelse(ExpProf[,1] == "Up" & ExpProf[,2] == "Up", "Cooperative",
                             ifelse(ExpProf[,1] == "Down" & ExpProf[,2] == "Down", "Cooperative",
                                    ifelse(ExpProf[,1] == "Unchanged" & ExpProf[,2] == "Unchanged", "Cooperative",
                                           ifelse(ExpProf[,1] == "Up" & ExpProf[,2] == "Down", "Competitive",
                                                  ifelse(ExpProf[,1] == "Down" & ExpProf[,2] == "Up", "Competitive",
                                                         ifelse(ExpProf[,1] == "Up" & ExpProf[,2] == "Unchanged", "Competitive",
                                                                ifelse(ExpProf[,1] == "Down" & ExpProf[,2] == "Unchanged", "Competitive",
                                                                       ifelse(ExpProf[,1] == "Unchanged" & ExpProf[,2] == "Up", "Competitive",
                                                                              ifelse(ExpProf[,1] == "Unchanged" & ExpProf[,2] == "Down", "Competitive",
                                                                                     "Error2")))))))))
  # Assign & Verify DualTx group
  ExpProf$DualTx <- ifelse(ExpProf$SingleTx == "Cooperative" & ExpProf[,1] == "Up" & ExpProf[,3] == "Up", "Concordant",
                           ifelse(ExpProf$SingleTx == "Cooperative" & ExpProf[,1] == "Down" & ExpProf[,3] == "Down", "Concordant",
                                  ifelse(ExpProf$SingleTx == "Cooperative" & ExpProf[,1] == "Up" & ExpProf[,3] == "Down", "Discordant",
                                         ifelse(ExpProf$SingleTx == "Cooperative" & ExpProf[,1] == "Down" & ExpProf[,3] == "Up", "Discordant",
                                                ifelse(ExpProf$SingleTx == "Competitive" & ExpProf[,1] == "Up" & ExpProf[,3] == "Up", "facA-Dominant",
                                                       ifelse(ExpProf$SingleTx == "Competitive" & ExpProf[,1] == "Down" & ExpProf[,3] == "Down", "facA-Dominant",
                                                              ifelse(ExpProf$SingleTx == "Competitive" & ExpProf[,2] == "Up" & ExpProf[,3] == "Up", "facB-Dominant",
                                                                     ifelse(ExpProf$SingleTx == "Competitive" & ExpProf[,2] == "Down" & ExpProf[,3] == "Down", "facB-Dominant",
                                                                            ifelse(ExpProf$SingleTx == "Cooperative" & ExpProf[,3] == "Unchanged", "No.AB.DEM",
                                                                                   ifelse(ExpProf$SingleTx == "Competitive" & ExpProf[,3] == "Unchanged", "No.AB.DEM",
                                                                                          "Error3"))))))))))
  # Assign SingleTx & Dual group related to expression in dual exposure
  ExpProf$UDSingleTx <- paste(ExpProf$SingleTx, ExpProf$UD.AB, sep = ".")
  ExpProf$UDDualTx <- paste(ExpProf$DualTx, ExpProf$UD.AB, sep = ".")

  # Summarize all results
  expr.calcs <- data.frame(m.l2.vals, d.log2FC, modCI, IntScore, ExpProf)

  return(expr.calcs)
}
