#' Title CxF Columnas a Filas
#' Sort a dataframe that contains "responses in multiple columns" in a single column
#' @param data dataframe
#'
#' @return a CSV file
#' @export
#' @import utils
#' @examples
CxF <- function(data){
  i..ActiveScreener.Id <- 1; Time.of.Screening <- 1 ; Title <- 1 ; Authors <- 1
  Question <- 1; List.of.Reviewers <- 1; Answers <- 1
  df <- data.frame(i..ActiveScreener.Id , Time.of.Screening, Title, Authors, Question, List.of.Reviewers, Answers)
  rm(i..ActiveScreener.Id ); rm(Time.of.Screening); rm(Title); rm(Authors); rm(Question); rm(List.of.Reviewers);
  rm(Answers)
  for(j in 1:nrow(data)){
    for(i in 7:ncol(data)){
      if(!is.na(data[j, i])){
        aux <- c(data[j, 1:6], data[[j, i]])
        names(aux) <- c("i..ActiveScreener.Id", "Time.of.Screening", "Title", "Authors",
                        "Question", "List.of.Reviewers","Answers")
        df <- rbind(df, aux)
      }
    }
  }
  df = df[-1, ]
  write.csv(df, "data.csv", sep = ";", fileEncoding = "UTF-8")

}
