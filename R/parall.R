#' Posterior probabilities
#'
#' @description
#' Get the posterior probabilites from a row of a test dataset using the NBAYES object created while \strong{fit}
#' \emph{proba_1_obs} gives the probabilities of the observation (row) to belong to the different modalities
#' of the variable to predict Y.
#'
#' @usage
#' proba_1_obs(ligne_df, nbayes)
#'
#' @param ligne_df row of the test dataframe to discretize
#' @param nbayes the nbayes object return by fit() function
#'
#' @return a vector containing the conditionnal probability for each modality to predict
#'
#'
#' @export




proba_1_obs <- function(ligne_df, nbayes) {
  table_conditionnelle <- nbayes$table_proba_cond
  n_mod_predire <- dim(table_conditionnelle[[1]])[2]
  prior <- nbayes$prior
  variable_explicative <- colnames(table_conditionnelle[[1]])


  # On calcul les prediction sur les données discrétisée
  #proba1 <- seq(1, n_mod_predire)
  proba2 <- seq(1, n_mod_predire)
  for (j in 1:n_mod_predire) {
    #P <- prior[j]
    P2 <- log(prior[j])
    for (i in names(table_conditionnelle)) {
      #P <- P * table_conditionnelle[[i]][as.character(ligne_df[i]), j]
      P2 <- P2 + log(table_conditionnelle[[i]][as.character(ligne_df[i]), j])
    }
    #proba1[j] <- P
    proba2[j] <- P2
  }
  #return(proba1)
  return(proba2)
  #return(list(proba1=proba1, proba2=proba2))
}



