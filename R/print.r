#' Print NBAYES object attributes
#'
#' @description
#' print different information about an NBAYES object
#'
#' @usage
#' print(NBAYES)
#'
#' @param NBAYES an NBAYES object
#'
#' @return print all the different informations about the object NBAYES
#'
#' @examples
#' data(iris)
#' train <- iris
#' modele <- fit(Species ~ ., train)
#' print(modele)
#'
#' @export

#Surcharge de la methode print
print.NBAYES<-function(NBAYES){
  for (i in 1:length(NBAYES$table_proba_cond)){
    cat("Probabilit?s conditionnelles de la variable",names(NBAYES$table_proba_cond[i])," :","\n")
    print(NBAYES$table_proba_cond[[i]])
    cat("\n")
  }
  cat("Probabilit?s ? priori :","\n")
  print(NBAYES$prior)
}
