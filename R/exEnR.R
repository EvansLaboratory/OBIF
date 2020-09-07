#' Title
#'
#' @param final.labels Text
#' @param col.ft.name Text
#'
#' @return Text
#' @export
#'
#'
exEnR <- function(final.labels, col.ft.name) {
  # Subset the DEMs per group
  A.DEMS <- subset(final.labels, A.DEM == "Yes")
  B.DEMS <- subset(final.labels, B.DEM == "Yes")
  AB.DEMS <- subset(final.labels, AB.DEM == "Yes")
  EP.I <- subset(AB.DEMS, EPs == "I")
  EP.II <- subset(AB.DEMS, EPs == "II")
  EP.III <- subset(AB.DEMS, EPs == "III")
  EP.IV <- subset(AB.DEMS, EPs == "IV")
  EP.V <- subset(AB.DEMS, EPs == "V")
  EP.VI <- subset(AB.DEMS, EPs == "VI")
  EP.VII <- subset(AB.DEMS, EPs == "VII")
  EP.VIII <- subset(AB.DEMS, EPs == "VIII")
  EP.Conc <- subset(AB.DEMS, DualTx == "Concordant")
  EP.Disc <- subset(AB.DEMS, DualTx == "Discordant")
  EP.ADom <- subset(AB.DEMS, DualTx == "facA-Dominant")
  EP.BDom <- subset(AB.DEMS, DualTx == "facB-Dominant")
  EP.COOP <- subset(AB.DEMS, SingleTx == "Cooperative")
  EP.COMP <- subset(AB.DEMS, SingleTx == "Competitive")
  iDEMs <- subset(AB.DEMS, AB.iDEM == "Yes")
  iSyn <- subset(iDEMs, DoubleIS == "Synergistic")
  iAnt <- subset(iDEMs, DoubleIS == "Antagonistic")

  # Create list of vectors for DEMs per group
  exEnR.res <- list(
    f.A.DEMS = A.DEMS[,col.ft.name],
    f.B.DEMS = B.DEMS[,col.ft.name],
    f.AB.DEMS = AB.DEMS[,col.ft.name],
    f.EP.I = EP.I[,col.ft.name],
    f.EP.II = EP.II[,col.ft.name],
    f.EP.III = EP.III[,col.ft.name],
    f.EP.IV = EP.IV[,col.ft.name],
    f.EP.V = EP.V[,col.ft.name],
    f.EP.VI = EP.VI[,col.ft.name],
    f.EP.VII = EP.VII[,col.ft.name],
    f.EP.VIII = EP.VIII[,col.ft.name],
    f.EP.Conc = EP.Conc[,col.ft.name],
    f.EP.Disc = EP.Disc[,col.ft.name],
    f.EP.ADom = EP.ADom[,col.ft.name],
    f.EP.BDom = EP.BDom[,col.ft.name],
    f.EP.COOP = EP.COOP[,col.ft.name],
    f.EP.COMP = EP.COMP[,col.ft.name],
    f.iDEMs = iDEMs[,col.ft.name],
    f.iSyn = iSyn[,col.ft.name],
    f.iAnt = iAnt[,col.ft.name])

  return(exEnR.res)
}
