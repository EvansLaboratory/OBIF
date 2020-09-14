#' OBIF Circos Plot for features summary
#'
#' @param final.labels Text
#'
#' @return Text
#' @export
#'
#' @import plyr circlize
#'
OBIFc <- function(final.labels) {
  # Assign a position to each DEM per group to be used as x coordinante for Circos plot in circlize
  final.labels <- ddply(final.labels,"A.DEM", transform, x1.A = sample(0:(length(A.DEM)-1), length(A.DEM), replace = FALSE))
  final.labels$x2.A <- final.labels$x1.A+1
  final.labels <- ddply(final.labels,"B.DEM", transform, x1.B = sample(0:(length(B.DEM)-1), length(B.DEM), replace = FALSE))
  final.labels$x2.B <- final.labels$x1.B+1
  final.labels <- ddply(final.labels,"AB.DEM", transform, x1.AB = sample(0:(length(AB.DEM)-1), length(AB.DEM), replace = FALSE))
  final.labels$x2.AB <- final.labels$x1.AB+1

  final.labels$AB.EP <- ifelse (final.labels$AB.DEM == "Yes", final.labels$UDDualTx,
                                ifelse (final.labels$A.DEM == "Yes", "A.DEMs",
                                        ifelse (final.labels$B.DEM == "Yes", "B.DEMs","No.DEMs")))

  final.labels <- ddply(final.labels,"AB.EP", transform, x1.EP = sample(0:(length(AB.EP)-1), length(AB.EP), replace = FALSE))
  final.labels$x2.EP <- final.labels$x1.EP+1

  final.labels$c.l2fc <- ifelse (final.labels$AB.DEM == "Yes", final.labels$d.facAB,
                                 ifelse (final.labels$A.DEM == "Yes", final.labels$d.facA,
                                         ifelse (final.labels$B.DEM == "Yes", final.labels$d.facB,"0")))

  # Assign their positions ordered by log2FC within each group
  final.labels <- ddply(final.labels,.(AB.EP), mutate, x1.ord = order(c.l2fc))
  final.labels$x2.ord <- final.labels$x1.ord + 1

  # Subset the DEMs per group
  A.DEMS <- subset(final.labels, A.DEM == "Yes")
  B.DEMS <- subset(final.labels, B.DEM == "Yes")
  AB.DEMS <- subset(final.labels, AB.DEM == "Yes")

  EP.1 <- subset(AB.DEMS, UDDualTx == "Concordant.Up")
  EP.2 <- subset(AB.DEMS, UDDualTx == "Concordant.Down")
  EP.3 <- subset(AB.DEMS, UDDualTx == "Discordant.Up")
  EP.4 <- subset(AB.DEMS, UDDualTx == "Discordant.Down")
  EP.5 <- subset(AB.DEMS, UDDualTx == "facA-Dominant.Up")
  EP.6 <- subset(AB.DEMS, UDDualTx == "facA-Dominant.Down")
  EP.7 <- subset(AB.DEMS, UDDualTx == "facB-Dominant.Up")
  EP.8 <- subset(AB.DEMS, UDDualTx == "facB-Dominant.Down")

  No.EP <- subset(AB.DEMS, EPs == "Error.No.EP")
  nrow(No.EP)
  # If = 0, continue as usual. Errors in Up and Down labeling does not affect any of the DEMs.
  # If > 1, Detect and remove DEMs that could not be classified as EP, or the features that are not DEMs, and consider whether its valuable to maintain or not.
  # Verify the No.EP with View(NoEP)

  # Assign their unique factors for final dataframe to use in circos
  c.A.DEMS <- data.frame(c.factors = rep("A.DEMs", nrow(A.DEMS)))
  c.B.DEMS <-data.frame(c.factors = rep("B.DEMs", nrow(B.DEMS)))
  c.AB.DEMS <- data.frame(c.factors = rep("AB.DEMs", nrow(AB.DEMS)))

  c.EP.1 <- data.frame(c.factors = rep("I", nrow(EP.1)))
  c.EP.2 <- data.frame(c.factors = rep("II", nrow(EP.2)))
  c.EP.3 <- data.frame(c.factors = rep("III", nrow(EP.3)))
  c.EP.4 <- data.frame(c.factors = rep("IV", nrow(EP.4)))
  c.EP.5 <- data.frame(c.factors = rep("V", nrow(EP.5)))
  c.EP.6 <- data.frame(c.factors = rep("VI", nrow(EP.6)))
  c.EP.7 <- data.frame(c.factors = rep("VII", nrow(EP.7)))
  c.EP.8 <- data.frame(c.factors = rep("VIII", nrow(EP.8)))

  # Assign their unique positions for random order to use in circos
  c.A.DEMS$c.x1 <- A.DEMS$x1.A
  c.A.DEMS$c.x2 <- A.DEMS$x2.A
  c.B.DEMS$c.x1 <- B.DEMS$x1.B
  c.B.DEMS$c.x2 <- B.DEMS$x2.B
  c.AB.DEMS$c.x1 <- AB.DEMS$x1.AB
  c.AB.DEMS$c.x2 <- AB.DEMS$x2.AB

  c.EP.1$c.x1 <- EP.1$x1.EP
  c.EP.1$c.x2 <- EP.1$x2.EP
  c.EP.2$c.x1 <- EP.2$x1.EP
  c.EP.2$c.x2 <- EP.2$x2.EP
  c.EP.3$c.x1 <- EP.3$x1.EP
  c.EP.3$c.x2 <- EP.3$x2.EP
  c.EP.4$c.x1 <- EP.4$x1.EP
  c.EP.4$c.x2 <- EP.4$x2.EP
  c.EP.5$c.x1 <- EP.5$x1.EP
  c.EP.5$c.x2 <- EP.5$x2.EP
  c.EP.6$c.x1 <- EP.6$x1.EP
  c.EP.6$c.x2 <- EP.6$x2.EP
  c.EP.7$c.x1 <- EP.7$x1.EP
  c.EP.7$c.x2 <- EP.7$x2.EP
  c.EP.8$c.x1 <- EP.8$x1.EP
  c.EP.8$c.x2 <- EP.8$x2.EP

  # Assign their unique log2FC for final dataframe to use in circos
  c.A.DEMS$c.lfc <- A.DEMS$d.facA
  c.B.DEMS$c.lfc <- B.DEMS$d.facB
  c.AB.DEMS$c.lfc <- AB.DEMS$d.facAB

  c.EP.1$c.lfc <- EP.1$d.facAB
  c.EP.2$c.lfc <- EP.2$d.facAB
  c.EP.3$c.lfc <- EP.3$d.facAB
  c.EP.4$c.lfc <- EP.4$d.facAB
  c.EP.5$c.lfc <- EP.5$d.facAB
  c.EP.6$c.lfc <- EP.6$d.facAB
  c.EP.7$c.lfc <- EP.7$d.facAB
  c.EP.8$c.lfc <- EP.8$d.facAB

  # Integrate the rest of dataset with the factors and positions
  c.A.DEMS <- cbind.data.frame(c.A.DEMS, A.DEMS)
  c.B.DEMS <- cbind.data.frame(c.B.DEMS, B.DEMS)
  c.AB.DEMS <- cbind.data.frame(c.AB.DEMS, AB.DEMS)

  c.EP.1 <- cbind.data.frame(c.EP.1, EP.1)
  c.EP.2 <- cbind.data.frame(c.EP.2, EP.2)
  c.EP.3 <- cbind.data.frame(c.EP.3, EP.3)
  c.EP.4 <- cbind.data.frame(c.EP.4, EP.4)
  c.EP.5 <- cbind.data.frame(c.EP.5, EP.5)
  c.EP.6 <- cbind.data.frame(c.EP.6, EP.6)
  c.EP.7 <- cbind.data.frame(c.EP.7, EP.7)
  c.EP.8 <- cbind.data.frame(c.EP.8, EP.8)

  c.AB.iDEMs <- subset(c.AB.DEMS, AB.iDEM == "Yes")

  # Combine DEMs dataframes to a final dataframe to be used in circos
  c.all.DEMs.df <- rbind.data.frame(c.A.DEMS, c.B.DEMS, c.AB.DEMS)
  c.all.EP.df <- rbind.data.frame(c.A.DEMS, c.B.DEMS, c.EP.1,c.EP.2,c.EP.3,c.EP.4,c.EP.5,c.EP.6,c.EP.7,c.EP.8)

  #Subset the shared DEMs between groups
  links.AnAB <- subset.data.frame(x = c.AB.DEMS,
                                  subset = c.AB.DEMS$A.DEM == "Yes" &
                                    c.AB.DEMS$AB.DEM == "Yes")
  links.BnAB <- subset.data.frame(x = c.AB.DEMS,
                                  subset = c.AB.DEMS$B.DEM == "Yes" &
                                    c.AB.DEMS$AB.DEM == "Yes")
  links.AnB <- subset.data.frame(x = c.A.DEMS,
                                 subset = c.A.DEMS$A.DEM == "Yes" &
                                   c.A.DEMS$B.DEM == "Yes")

  # Create beds of linked DEMs between groups
  bed1.AnAB <- data.frame(c.factors = rep("A.DEMs", nrow(links.AnAB)),
                          start = links.AnAB$x1.A,
                          end = links.AnAB$x1.A)
  bed2.AnAB <- data.frame(c.factors = rep("AB.DEMs", nrow(links.AnAB)),
                          start = links.AnAB$x1.AB,
                          end = links.AnAB$x1.AB)

  bed1.BnAB <- data.frame(c.factors = rep("B.DEMs", nrow(links.BnAB)),
                          start = links.BnAB$x1.B,
                          end = links.BnAB$x1.B)
  bed2.BnAB <- data.frame(c.factors = rep("AB.DEMs", nrow(links.BnAB)),
                          start = links.BnAB$x1.AB,
                          end = links.BnAB$x1.AB)

  bed1.AnB <- data.frame(c.factors = rep("A.DEMs", nrow(links.AnB)),
                         start = links.AnB$x1.A,
                         end = links.AnB$x1.A)
  bed2.AnB <- data.frame(c.factors = rep("B.DEMs", nrow(links.AnB)),
                         start = links.AnB$x1.B,
                         end = links.AnB$x1.B)

  # Create beds of linked DEMs between groups and Expression profiles
  ep.bed1.AnAB <- data.frame(c.factors = rep("A.DEMs", nrow(links.AnAB)),
                             start = links.AnAB$x1.A,
                             end = links.AnAB$x1.A)
  ep.bed2.AnAB <- data.frame(c.factors = links.AnAB$EPs,
                             start = links.AnAB$x1.EP,
                             end = links.AnAB$x2.EP)

  ep.bed1.BnAB <- data.frame(c.factors = rep("B.DEMs", nrow(links.BnAB)),
                             start = links.BnAB$x1.B,
                             end = links.BnAB$x1.B)
  ep.bed2.BnAB <- data.frame(c.factors = links.BnAB$EPs,
                             start = links.BnAB$x1.EP,
                             end = links.BnAB$x2.EP)

  ep.bed1.AnB <- data.frame(c.factors = rep("A.DEMs", nrow(links.AnB)),
                            start = links.AnB$x1.A,
                            end = links.AnB$x1.A)
  ep.bed2.AnB <- data.frame(c.factors = rep("B.DEMs", nrow(links.AnB)),
                            start = links.AnB$x1.B,
                            end = links.AnB$x1.B)

  # Make the region columns for circos.genomic functions ----
  # For random feature order keep c.x1 and c.x2 as $c.x1 and $c.x2, for order by log2 FC change them to $x1.ord and $x2.ord
  c.start <- cbind.data.frame(c.factors = c.all.EP.df$c.factors,
                              c.x1 = c.all.EP.df$c.x1,
                              c.x2 = c.all.EP.df$c.x2)

  # Initialize the circos plot sectors and outer most track
  circos.clear()
  circos.par(start.degree = 90,
             clock.wise = FALSE,
             gap.degree = 2)
  circos.genomicInitialize(data = c.all.EP.df,
                           plotType = NULL)

  # Antagonistic and synergistic iDEMs and their Interaction Score ----
  # Make the genomic format
  c.IS.iDEMs <- cbind.data.frame(c.start,
                                 c.value = ifelse (
                                   c.start$c.factors == "A.DEMs" |
                                     c.start$c.factors == "B.DEMs",
                                   0, c.all.EP.df$f.IS.iDEMs))

  cond_col_is = function (value) {ifelse (value < 0, "orange",
                                          ifelse (value > 0, "purple",
                                                  ifelse (CELL_META$sector.index == "A.DEMs" | CELL_META$sector.index == "B.DEMs", "white",
                                                          "lightgrey")))}

  circos.par(track.margin = c(0.0025,0),
             cell.padding = c(0.0,0,0.0,0))
  circos.genomicTrackPlotRegion(data = c.IS.iDEMs,
                                bg.col = NA,
                                bg.border = NA,
                                track.height = 0.25,
                                ylim = range(c.IS.iDEMs$c.value),
                                panel.fun = function(region, value, ...) {
                                  circos.genomicLines(region,
                                                      value,
                                                      type = "h",
                                                      baseline = 0,
                                                      lwd = 2,
                                                      col = cond_col_is(value),
                                                      ...)
                                })

  c.rect.smeA <- cbind.data.frame(c.start,
                                  c.value = ifelse (c.start$c.factors == "A.DEMs" | c.start$c.factors == "B.DEMs", 0,
                                                    ifelse (c.all.EP.df$AB.SME.A == "Yes", 1, 4)))
  c.rect.smeB <- cbind.data.frame(c.start,
                                  c.value = ifelse (c.start$c.factors == "A.DEMs" | c.start$c.factors == "B.DEMs", 0,
                                                    ifelse (c.all.EP.df$AB.SME.B == "Yes", 2, 4)))
  c.rect.fAxfB <- cbind.data.frame(c.start,
                                   c.value = ifelse (c.start$c.factors == "A.DEMs" | c.start$c.factors == "B.DEMs", 0,
                                                     ifelse(c.all.EP.df$AB.iDEM == "Yes", 3, 4)))

  c.rect.effs <- cbind.data.frame(c.start,
                                  c.v1 = c.rect.smeA$c.value,
                                  c.v2 = c.rect.smeB$c.value,
                                  c.v3 = c.rect.fAxfB$c.value
  )

  cond_col_eff = function (value) {ifelse (value == 1, "#1b9e77",
                                           ifelse (value == 2, "#d95f02",
                                                   ifelse (value == 3, "#e7298a",
                                                           ifelse (value == 4, "#f0f0f0","white"))))}

  circos.par(track.margin = c(0, 0.0025),
             cell.padding = c(0,0,0,0))
  circos.genomicTrackPlotRegion(data = c.rect.effs,
                                bg.col = "#f0f0f0",
                                stack = TRUE,
                                bg.border = NA,
                                track.height = 0.1,
                                panel.fun = function(region, value, ...) {
                                  circos.genomicRect(region,
                                                     value,
                                                     border = cond_col_eff(value),
                                                     col = cond_col_eff(value),
                                                     ...)})

  cond_col_ep = function (value) {ifelse (CELL_META$sector.index == "I", "#1f78b4",
                                          ifelse (CELL_META$sector.index == "II", "#a6cee3",
                                                  ifelse (CELL_META$sector.index == "III", "#e31a1c",
                                                          ifelse (CELL_META$sector.index == "IV", "#fb9a99",
                                                                  ifelse (CELL_META$sector.index == "V", "#33a02c",
                                                                          ifelse (CELL_META$sector.index == "VI", "#b2df8a",
                                                                                  ifelse (CELL_META$sector.index == "VII", "#ff7f00",
                                                                                          ifelse (CELL_META$sector.index == "VIII","#fdbf6f","white"))))))))}
  circos.par(track.margin = c(0.0025,0.0025),
             cell.padding = c(0.000,0,0.000,0))

  circos.genomicTrackPlotRegion(data = c.all.EP.df,
                                bg.col = NA,
                                bg.border = NA,
                                track.height = 0.05,
                                ylim = range(c.all.EP.df$c.x1),
                                panel.fun = function(region, value, ...) {
                                  circos.genomicRect(region,
                                                     value,
                                                     border = cond_col_ep(value),
                                                     col = cond_col_ep(value),
                                                     ...)})

  c.l.l2fc <- cbind.data.frame(c.start,
                               c.v1 = c.all.EP.df$c.lfc)
  c.l.up <- cbind.data.frame(c.start,
                             c.v1 = ifelse (c.all.EP.df$c.lfc > 0, c.all.EP.df$c.lfc, 0))
  c.l.down <- cbind.data.frame(c.start,
                               c.v1 = ifelse (c.all.EP.df$c.lfc < 0, c.all.EP.df$c.lfc, 0))

  cond_col_l2fc = function (value) {ifelse (value < 0, "blue",
                                            ifelse (value > 0, "red", "white"))}

  circos.par(track.margin = c(0.0025,0.0025),
             cell.padding = c(0.000,0,0.000,0))

  circos.genomicTrackPlotRegion(data = c.l.l2fc,
                                bg.col = NA,
                                bg.border = "lightgrey",
                                track.height = 0.20,
                                ylim = range(c.l.l2fc$c.v1),
                                panel.fun = function(region, value, ...) {
                                  circos.genomicLines(region,
                                                      value,
                                                      type = "h",
                                                      baseline = 0,
                                                      col = cond_col_l2fc(value),
                                                      ...)
                                })

  cond_col_dems = function (value) {ifelse (CELL_META$sector.index == "A.DEMs", "#8dd3c7",
                                            ifelse (CELL_META$sector.index == "B.DEMs", "#fb8072",
                                                    "#80b1d3"))}

  circos.par(track.margin = c(0.0025,0),
             cell.padding = c(0,0,0,0))

  circos.genomicTrackPlotRegion(data = c.l.down,
                                bg.col = NA,
                                bg.border = "lightgrey",
                                track.height = 0.05,
                                ylim = range(c.l.down$c.v1),
                                panel.fun = function(region, value, ...) {
                                  circos.genomicRect(region,
                                                     value,
                                                     border = cond_col_dems(value),
                                                     col = cond_col_dems(value),
                                                     ...)})


  circos.par(track.margin = c(0.0025,0.0025),
             cell.padding = c(0.000,0,0.000,0))

  circos.genomicLink(region1 = ep.bed1.AnAB,
                     region2 = ep.bed2.AnAB,
                     border = NA,
                     col = "#8dd3c7")

  circos.genomicLink(region1 = ep.bed1.BnAB,
                     region2 = ep.bed2.BnAB,
                     border = NA,
                     col = "#fb8072")

  draw.sector(get.cell.meta.data("cell.start.degree", sector.index = "VIII") + 1.25,
              get.cell.meta.data("cell.end.degree", sector.index = "I") - 1.25,
              rou1 = get.cell.meta.data("cell.top.radius", track.index = 1) + 0.01,
              rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 5) - 0.05,
              col = "NA",
              border = "blue",
              lwd = 1)

  OBIF.sum <- grDevices::recordPlot()
  return(OBIF.sum)
}
