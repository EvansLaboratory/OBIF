#' Title
#'
#' @param final.labels Text
#'
#' @return Text
#' @export
#'
#' @import gtools stats ggplot2 ggbiplot
OBIFp <- function(final.labels) {
  # Subset the DEMs per group
  A.DEMS <- subset(final.labels, A.DEM == "Yes")
  B.DEMS <- subset(final.labels, B.DEM == "Yes")
  AB.DEMS <- subset(final.labels, AB.DEM == "Yes")

  # Make a copy of subsets to work from
  c.A.DEMS <- A.DEMS
  c.B.DEMS <- B.DEMS
  c.AB.DEMS <- AB.DEMS
  c.AB.iDEMs <- subset(c.AB.DEMS, AB.iDEM == "Yes")

  # Set colors
  pca.col.pal <- c("I" = "#1f78b4",
                   "II" ="#a6cee3",
                   "III" = "#e31a1c",
                   "IV"="#fb9a99",
                   "V"="#33a02c",
                   "VI"="#b2df8a",
                   "VII"="#ff7f00",
                   "VIII"="#fdbf6f")

  FC.ExPr <- data.frame(A = logratio2foldchange(c.AB.DEMS$d.facA, base = 2),
                        B = logratio2foldchange(c.AB.DEMS$d.facB, base = 2),
                        AB  = logratio2foldchange(c.AB.DEMS$d.facAB, base = 2))

  # PCA
  data.class <- c.AB.DEMS$EPs
  data.pca <- prcomp(FC.ExPr, center = T, scale. = T)
  g <- ggbiplot(data.pca,
                obs.scale = 1,
                var.scale = 1,
                groups = data.class,
                ellipse = F,
                circle = F)
  g
  fin.g <- g +
    theme_bw() +
    xlim(-2, 2) +
    ylim(-2, 2) +
    theme(legend.direction = 'horizontal', legend.position = 'top') +
    scale_color_manual(name = "Expression Profiles", values = pca.col.pal, limits = names(pca.col.pal))
  fin.g
}
