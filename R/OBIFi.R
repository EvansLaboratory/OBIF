#' Title
#'
#' @param final.labels Text
#'
#' @return Text
#' @export
#'
#' @import graphics
OBIFi <- function(final.labels) {
  # Subset the DEMs per group
  A.DEMS <- subset(final.labels, A.DEM == "Yes")
  B.DEMS <- subset(final.labels, B.DEM == "Yes")
  AB.DEMS <- subset(final.labels, AB.DEM == "Yes")

  # Make a copy of subsets to work from
  c.A.DEMS <- A.DEMS
  c.B.DEMS <- B.DEMS
  c.AB.DEMS <- AB.DEMS
  c.AB.iDEMs <- subset(c.AB.DEMS, AB.iDEM == "Yes")

  # Make IS plot
  cond_col_isplot = function (value) {ifelse (value == "iDEM.Antagonistic", "orange",
                                              ifelse (value == "iDEM.Synergistic", "purple", "lightgrey"))}

  ISplot <- c.AB.DEMS[order(c.AB.DEMS$AbsIScore),]

  true.plot <- barplot(ISplot$AbsIScore,
                       horiz = TRUE,
                       col = cond_col_isplot(ISplot$iDEMs),
                       border = NA,
                       main = "Interaction Score Plot")
  true.plot
  return(true.plot)
}
