#' Prediction method for NBC
#'
#' @description
#' Compute posterior probabilities and class based on Naive Bayes Classifier
#'
#' @usage
#' predict.NBAYES(object_NBAYES, data_test, type="both", parallel=FALSE)
#'
#' @param object_NBAYES an object of class NBAYES build using \emph{fit}
#' @param data_test a dataframe containing the variables to predict
#' @param type a character type, it will define what the function will return. Can be "posterior", if the user want
#' to get only the posterior probabilities for the different predict class. "class" which give the user only the modality
#' of the prediction. And "both" that will return both "posterior" and "class" informations.
#' @param parallel a logic that allow the user to compute the function using parallel computation.
#'
#' @return a dataframe containing the posterior probabilities, the predict class or both of them.
#'
#' @examples
#' data(iris)
#' train <- iris
#' test <- iris[-5]
#' model <- fit(Species ~ ., train, laplace=1, parallel=FALSE)
#' res <- predict(model, test, type="class")
#'
#'
#' @import stats
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel detectCores
#'
#' @export


#### FONCTION PREDICTION POUR UN OBJET NBAYES ####
predict.NBAYES <- function(object_NBAYES, data_test, type="both", parallel=FALSE) {       # ajouter une option type('class' ou 'posterior')
  # Checking dataframe
  # Si l'arguement object_NBAYES est bien un objet NBAYES
  if (class(object_NBAYES) != "NBAYES") {
    stop("The object you gave is not a NBAYES object")
  }
  # Si l'utilisateur a bien rentrer un des trois type
  if ((type!="class") & (type!="posterior") & (type!="both")) {
    stop('Wrong argument type! Must be "class", "posterior" or "both"')
  }

  # Si l'utilisateur passe un dataframe contenant déjà la variable à prédire, l'algorithme va la reconnaître
  # et ne s'appuiera pas dessus pour calculer les prédictions, ses dernières resteront donc juste.
  # En revanche si l'utilisateur passe un dataframe ayant des noms de colonnes différents que lors du fit,
  # l'algorithme stop!!


  # On a besoin de plusieurs variables générées par le fit
  table_conditionnelle <- object_NBAYES$table_proba_cond
  n_mod_predire <- dim(table_conditionnelle[[1]])[2]
  prior <- object_NBAYES$prior
  nvar <- ncol(data_test)
  variable_explicative <- colnames(table_conditionnelle[[1]])


  # Le code se décompose (comme le fit) en deux partie. La 1er partie correspondant au traitement de données
  # ayant subit une discrétisation dans le fit, la 2eme au dataframe

  condition1 <- sum(sapply(data_test, class) == "numeric")
  condition2 <-sum(sapply(data_test, class) == "integer")

  # Si des variables à discrétiser sont présentes
  if ((condition1 > 0) | (condition2 > 0)) {
    # discretization des colonnes ayant ete discretiser dans le fit()
    # la condition permet de ne pas discrétiser la variable a predire qui n'existe pas dans les donnees test
    condition <- object_NBAYES$condition
    var_a_predire <- object_NBAYES$var_a_predire


    # Toujours la possibilité pour l'utilisateur d'effectué les calculs en parallèle
    if (parallel==TRUE) {
      # Initialisation de la parallelisation
      nb_cores <- detectCores() - 1
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)

      # Condition sur le nombre de colonee et si la variable à predire est présente ou pas dans le jeu de donnee test
      if (sum(names(condition) == var_a_predire) == 1) {
        condition <- condition[-which(names(condition) == var_a_predire)]

        # Discretisation des colonnes suivant le même découpage réaliser par la fonction mdlp() dans le fit
        # On boucle en parallele sur la variable condition, permettant de savoir quelle colonne doit subir
        # la discretisation (voir doc discretisation() pour plus d'information)
        data_test <- foreach(i=1:length(condition), .combine=cbind, .export=c("discretisation")) %dopar%
          discretisation(data_test[names(condition[i])] , object_NBAYES$cuts[[condition[i]]])
        stopCluster(cl)

      } else {

        # Discretisation des colonnes suivant le même découpage réaliser par la fonction mdlp() dans le fit
        data_test <- foreach(i=1:length(condition), .combine=cbind, .export=c("discretisation")) %dopar%
          discretisation(data_test[names(condition[i])] , object_NBAYES$cuts[[condition[i]]])
        stopCluster(cl)
      }


      # On calcul les prediction sur les données discrétisée, calcul du posterior
      ###### PARALLELISATION ######
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      # On boucle en parallele sur toutes les lignes du dataframe l'execution de la fonction proba_1_obs (Cf doc)
      # et retourne un dataframe
      pred <- foreach(i=1:nrow(data_test), .combine=rbind, .export=c("proba_1_obs")) %dopar% proba_1_obs(data_test[i,], object_NBAYES)
      stopCluster(cl)


      # Cette partie est la même que celle présentée précédemment mais de manière séquentielle
      # Seul le calcul des probabilité a posteriori se font toujours en parallèle
      # Discretisation à partir des cuts
    } else if (parallel==FALSE) {
      if (sum(names(condition) == var_a_predire) == 1) {
        condition <- condition[-which(names(condition) == var_a_predire)]

        for (j in 1:length(condition)) {
          data_test[names(condition[j])] <- discretisation(data_test[names(condition[j])] , object_NBAYES$cuts[[condition[j]]])
        }

      } else {
        for (j in 1:length(condition)) {
          data_test[names(condition[j])] <- discretisation(data_test[names(condition[j])] , object_NBAYES$cuts[[condition[j]]])
        }
      }

      # On calcul les prediction sur les données discrétisée
      ###### PARALLELISATION ######
      nb_cores <- detectCores() - 1
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      pred <- foreach(i=1:nrow(data_test), .combine=rbind, .export=c("proba_1_obs")) %dopar% proba_1_obs(data_test[i,], object_NBAYES)
      stopCluster(cl)
    }

    # Arriver ici, nous avons un dataframe (pred), contenant les probabilités a posteriori correspondant
    # à chaque modalité de la variable à prédire. Nous allons normaliser cela avec l'évidence

    # calcul de l'évidence
    evidence_par <- seq(1, nrow(pred))
    for (i in 1:nrow(pred)) {
      evidence_par[i] <- abs(sum(pred[i,]))
    }

    # Normalisation de la prediction
    pred <- (pred/evidence_par) + 1

    # On harmonise le nom des lignes
    pred <- as.data.frame(pred)
    rownames(pred) <- 1:nrow(pred)
    colnames(pred) <- unique(variable_explicative)

    # creation de la colonne "class"
    classe <- seq(1, nrow(pred))

    # Suite à l'utilisation de la fonction profviz(), on a vu que cette partie du code pouvait prendre
    # du temps, notamment pour les grosses bases. Ce qui justifie sa potentielle parallélisation.
    # Ici on cherche juste à retrouver la valeur maximum de posterior pour chaque ligne pour ainsi définir
    # à quelle classe l'observation appartient.
    if (parallel==TRUE) {
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      classe <- foreach(i=1:nrow(pred), .combine=c) %dopar%
        colnames(pred)[which.max(pred[i,])]
      stopCluster(cl)
    } else if (parallel==FALSE) {
      for (i in 1:nrow(pred)) {
        classe[i] <- colnames(pred)[which.max(pred[i,])]
      }
    }


    # Concatene les deux, posterior et class
    pred <- cbind.data.frame(pred, classe)
    colnames(pred)[ncol(pred)] <- "class"


    # We're almost done!
    print("Prediction is done!")


    # Formatage resultat
    if (type == "class") {
      pred <- pred["class"]
    }
    else if (type == "posterior"){
      pred <- pred[-(ncol(pred))]
    }
    else {
      return(list(prediction=pred))
    }

    return(list(prediction=pred))
  }


  # Dans les cas où aucune discrétisation n'a été effectué lors du fit, nous passons directement à l'étape
  # de calcul des posterior

  ###### ELSE ###### Pas besoin de discretiser
  else {
    # On calcul les prediction sur les données (qualitatives)
    ###### PARALLELISATION ######
    nb_cores <- detectCores() - 1
    cl <- makeCluster(nb_cores)

    registerDoParallel(cl)
    pred <- foreach(i=1:nrow(data_test), .combine=rbind, .export=c("proba_1_obs")) %dopar% proba_1_obs(data_test[i,], object_NBAYES)
    stopCluster(cl)

    evidence_par <- seq(1, nrow(pred))
    for (i in 1:nrow(pred)) {
      evidence_par[i] <- abs(sum(pred[i,]))
    }

    # Normalisation de la prediction
    pred <- (pred/evidence_par) + 1

    # On harmonise le nom des lignes
    pred <- as.data.frame(pred)
    rownames(pred) <- 1:nrow(pred)
    colnames(pred) <- unique(variable_explicative)

    # creation de la colonne "class"
    classe <- seq(1, nrow(pred))

    # Recherche de la classe équivalent au posterior maximum
    if (parallel==TRUE) {
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      classe <- foreach(i=1:nrow(pred), .combine=c) %dopar%
        colnames(pred)[which.max(pred[i,])]
      stopCluster(cl)
    } else if (parallel==FALSE) {
      for (i in 1:nrow(pred)) {
        classe[i] <- colnames(pred)[which.max(pred[i,])]
      }
    }

    # Concatene les deux
    pred <- cbind.data.frame(pred, classe)
    colnames(pred)[ncol(pred)] <- "class"
    # We're almost done!
    print("Prediction is done!")

    # Formatage resultat
    if (type == "class") {
      pred <- pred["class"]
    }
    else if (type == "posterior"){
      pred <- pred[-(ncol(pred))]
    }
    else {
      return(list(prediction=pred))
    }

    return(list(prediction=pred))
  }
}

