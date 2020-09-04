#' Title
#'
#' @param dataAR Text
#' @param design2x2 Text
#' @param result Text
#'
#' @return Text
#' @export
#'
obint <- function(dataAR, design2x2, result) {
  # Data set up for linear modeling of feature expression
  ints.mtx.arr <- as.matrix(dataAR)
  pv.inter <- vector("numeric", length = nrow(ints.mtx.arr))
  for (i in 1:nrow(ints.mtx.arr)) {ints.tmp.arr <- data.frame(exp = ints.mtx.arr[i, ], design2x2)}

  # Fitting the expression model for Omics-based interaction analysis
  ints.fit.arr <- stats::lm(exp ~ 0 + facA + facB + facA*facB, data = ints.tmp.arr)

  #Two-way ANOVA-like summary of Omics-based interaction analysis
  sum.ints.fit.arr <- lumi::summary(ints.fit.arr)

  #Interaction plots
  inter.plot <- function(){
    graphics::par(mfrow = c(2,1))
    ints.plot1.arr <- with(ints.tmp.arr, stats::interaction.plot(facA, facB, exp, main="Omics-based Interaction (Factor A vs Factor B)"))
    ints.plot2.arr <- with(ints.tmp.arr, stats::interaction.plot(facB, facA, exp, main="Omics-based Interaction (Factor B vs Factor A)"))
    graphics::par(mfrow = c(1,1))
    true.plots <- grDevices::recordPlot()
    return(true.plots)
  }

  intan.res <- ifelse(result == "modelfit", return(ints.fit.arr),
                     ifelse(result == "modelsum", return(sum.ints.fit.arr),
                            ifelse(result == "interplots", return(inter.plot), "Result not valid")))

  return(intan.res)
}
