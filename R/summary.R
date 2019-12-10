#' Summary of a NBAYES object
#'
#' @description
#' summary the information of a NBAYES object
#'
#' @usage summary.NBAYES(NBAYES)
#'
#' @param NBAYES an NBAYES object
#'
#' @return print all the different informations about the object NBAYES
#'
#'
#' @examples
#' data(iris)
#' train <- iris
#' modele <- fit(Species ~ ., train)
#' summary(modele)
#'
#' @export


#Surcharge de la methode summary qui retourne un objet pour acc?der aux diff?rents ?l?ments
summary.NBAYES<-function(NBAYES){
  print(NBAYES)
  return(NBAYES)
}
