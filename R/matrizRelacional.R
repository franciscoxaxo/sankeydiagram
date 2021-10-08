#' Title matrizRelacional
#' @description Function that transforms a data set into a relationship matrix
#' between two criteria. The required format of the input dataframe is 7 columns:
#'  "Id", "Time.of.Screening", "Title", "Authors", "Question", "List.of.Reviewers"
#'   and "Answers".
#' @param data_df dataframe that will be worked
#' @param criterio1 criterion of the column "Questions"
#' @param criterio2 criterion of the column "Questions"
#'
#' @return An relational matrix
#' @export
#' @import dplyr
#'
#' @examples
matrizRelacional<- function(data_df, criterio1, criterio2){
  data_df    <- data_df[-1]
  subset1    <- dplyr::filter(data_df, Question == criterio1)
  subset2    <- dplyr::filter(data_df, Question == criterio2)
  labelsCol  <- as.vector(unique(subset2[[7]]))
  labelsRow  <- as.vector(unique(subset1[[7]]))
  rowSubset1 <- nrow(subset1)
  rowSubset2 <- nrow(subset2)

  row <- length(labelsRow)
  col <- length(labelsCol)

  baseMatrix <- matrix(0, nrow = row, ncol = col)
  row.names(baseMatrix) <- labelsRow
  colnames(baseMatrix) <- labelsCol

  for(i in 1:rowSubset1){
    valueSubset1 <- subset1[i, 7]
    index1 <- subset1[i, 1]
    index_row_matrix <- which(row.names(baseMatrix)== valueSubset1)
    for(j in 1:rowSubset2){
      if(index1 == subset2[j, 1]){
        valueSubset2 <- subset2[j, 7]
        index_col_matrix <- which(colnames(baseMatrix) == valueSubset2)
        baseMatrix[index_row_matrix, index_col_matrix] = baseMatrix[index_row_matrix, index_col_matrix] + 1
      }
    }
  }

  return(baseMatrix)
}
