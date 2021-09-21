#' Title sankey
#' @description PLot a sankey diagram from a matrix
#' @param matriz adjacency matrix
#' @param title plot title
#'
#' @return A sankey diagram plot
#' @export
#' @import plotly
#' @examples sankey(matrix(40, 4, 10))
sankey <- function(matriz, title = "Basic Sankey Diagram"){

  matriz <- as.data.frame(matriz)
  label <- c(row.names(matriz), colnames(matriz))
  #print(label)
  source <- NULL
  target <- rep((nrow(matriz)):(nrow(matriz)+ncol(matriz) - 1), nrow(matriz))
  #print(target)
  value  <- NULL
  for(i in 1:nrow(matriz)){
    aux <- rep(i - 1, ncol(matriz))
    source <- c(source, aux)
    for(j in 1:ncol(matriz)){
      value <- c(value, matriz[i, j])
    }

  }

  fig <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",

    node = list(
      label = label,

      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),

    link = list(
      source = source,
      target = target,
      value =  value
    )
  )
  fig <-  plotly::layout(fig,
    title = title,
    font = list(
      size = 10
    )
  )
  return(fig)
}

