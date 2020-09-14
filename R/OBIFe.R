#' OBIF Euler Diagram for DEMs
#'
#' @param final.labels Text
#'
#' @return Text
#' @export
#'
#' @import eulerr graphics
OBIFe <- function(final.labels) {
  # Subset the DEMs per group
  A.DEMS <- subset(final.labels, A.DEM == "Yes")
  B.DEMS <- subset(final.labels, B.DEM == "Yes")
  AB.DEMS <- subset(final.labels, AB.DEM == "Yes")

  # Make a copy of subsets to work from
  c.A.DEMS <- A.DEMS
  c.B.DEMS <- B.DEMS
  c.AB.DEMS <- AB.DEMS
  c.AB.iDEMs <- subset(c.AB.DEMS, AB.iDEM == "Yes")

  # Create list of vectors for DEMs per group for Euler Diagram
  venn.list <- list(facA.dems = c.A.DEMS$featIDs,
                    facB.dems = c.B.DEMS$featIDs,
                    facAB.dems =c.AB.DEMS$featIDs)

  venn.list.iDEMs <- list(facA.dems = c.A.DEMS$featIDs,
                          facB.dems = c.B.DEMS$featIDs,
                          facAB.dems =c.AB.DEMS$featIDs,
                          facAB.idems = c.AB.iDEMs$featIDs)

  # Plotting Venn Diagram with Euler
  Euler.DEMs <- plot(euler(venn.list),
                     quantities = TRUE,
                     edges = c("#8dd3c7","#fb8072","#80b1d3"),
                     fills = FALSE,
                     labels= TRUE)

  Euler.iDEMs <- plot(euler(venn.list.iDEMs),
                      quantities = TRUE,
                      edges = c("#8dd3c7","#fb8072","#80b1d3","#e7298a"),
                      fills = FALSE,
                      labels= TRUE)
  Euler.DEMs

  return(Euler.DEMs)
}
