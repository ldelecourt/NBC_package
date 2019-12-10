#' Discretization
#'
#' @description
#' Discretization using the mdlp() function from the package discretization
#'
#' @usage
#' discret(colonne, Y)
#'
#' @param colonne the column of a dataframe that will be discretize, must be numeric
#' @param Y the column of a dataframe containing modalities to predict
#'
#' @return df_disc a dataframe with the variable colonne discretize, cuts a list giving the cuts made by the mdlp()
#' function
#'
#'
#' @examples
#' data(iris)
#' column <- iris[1]
#' Y <- iris[5]
#' disc <- discret(column, Y)
#'
#'
#' @importFrom discretization mdlp
#'
#' @export


discret <- function(colonne, Y) {
  df <- mdlp(cbind(colonne, Y))
  df_disc <- df$Disc.data[-2]
  cuts <- df$cutp
  return(list(df=df_disc, cuts=cuts))
}
