#' Build function
#'
#' This function is used to finalize the Omics Screening component of OBIF, and prepares a complete dataset that will be used for the Feature Discovery component of OBIF.
#'
#' @param featIDs A vector object containing the unique IDs or names of the features of the Omics dataset
#' @param meta.data A data frame containing the additional metadata to be associated with each feature
#' @param dataAR A data frame containing the OBIF-scaled analysis ready expression values of all features per samples
#' @param obffa.res A data frame with all, or selected, results from the Full Factorial Analysis
#' @param synex.res A data frame with all, or selected, results from the relative expression calculations and scorings
#'
#' @return A data frame containing the merged columns of all the inputs. This dataset is to be used for Feature Discovery analysis.
#' @export
#'
build <- function(featIDs, meta.data, dataAR, obffa.res, synex.res) {
  final.res <- data.frame(featIDs, meta.data, dataAR, obffa.res, synex.res)
  return(final.res)
}
