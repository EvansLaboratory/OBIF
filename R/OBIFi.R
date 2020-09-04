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

  ISplot <- c.AB.iDEMs[order(c.AB.iDEMs$AbsIScore),]

  true.plot <- barplot(ISplot$AbsIScore,
          horiz = TRUE,
          col = cond_col_isplot(ISplot$f.IS.iDEMs),
          border = NA)

  return(true.plot)
}
