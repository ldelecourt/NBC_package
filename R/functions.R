#' Discretization from cuts
#'
#' @description
#' Discretization from cuts made by the \emph{mdlp} function from \emph{library(discretization)} during the \emph{fit}
#' on the training dataset.
#'
#' @usage
#' discretisation(column, cuts)
#'
#' @param column the column of a dataframe that will be discretize, must be numeric
#' @param cuts a list of cuts made by \emph{mdlp} usually NBAYES$cuts
#'
#' @return a dataframe with all row discretize
#'
#' @examples
#' data(iris)
#' train <- iris
#' modele <- fit(Species ~ ., train, laplace=0)
#' disc <- discretisation(iris[1], modele$cuts[[1]])
#'
#' @export



# Discretisation à partir de cuts obtenu après la discretisation d'un autre jeu de données
discretisation <- function(column, cuts) {
  nombre_cut_total <- length(cuts)

  for (i in 1:nrow(column)) {

    for (k in 1:nombre_cut_total) {
      if (column[i,1] < cuts[1]) {
        column[i,1] <- 1
        break

      } else if (column[i,1] > cuts[nombre_cut_total]) {
        column[i,1] <- nombre_cut_total + 1
        break

      } else { for (j in 1:nombre_cut_total-1) {
        if ((cuts[j] < column[i,1]) && (column[i,1] < cuts[j+1])) {
          column[i,1] <- j+1
          break
        }
      }
        break
      }
    }
    #print(column[i,1])
  }
  return(column)
}

