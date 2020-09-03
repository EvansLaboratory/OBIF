tresh <- function(dataAR, treshold) {
  dataAR.tresh <- sapply(dataAR,
                         function(x) {ifelse(x < treshold, 1, 0)})
  dataAR.exclude <- rowSums(dataAR.tresh)
  dataAR.clean <- cbind(dataAR,
                        "exclude" = dataAR.exclude)
  dataAR.clean <- subset(dataAR.clean, exclude < 1)
  dataAR.clean$exclude <- NULL
  return(dataAR.clean)
}
