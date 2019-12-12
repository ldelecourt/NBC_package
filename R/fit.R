#' Fitting the NBC model
#'
#' @description
#' fit is used to fit the Naive Bayes Classifier model. It can compute with all types of variables,
#' applying discretisation on quantitative variables and need a qualitative predictive variable.
#'
#' @usage
#' fit(formula, data, laplace=1, parallel=FALSE)
#'
#' @param formula an object of class formula.
#' @param data a dataframe containing the variables in the model.
#' @param laplace an integer that will be use to do the Laplace smoothing.
#' @param parallel a logic that allow the user to compute the function using parallel computation.
#'
#' @return an NBAYES object containing several information that can be useful for the user and had to pass to the
#' predict function. The informations depends if the fit had to discretize or not.
#' For both cases the fit return:
#' @return \strong{NBAYES$table_proba_cond}, a table containing he conditionnal probabilities
#' @return \strong{NBAYES$prior}, a table full by the prior probabilities
#'
#' Plus, if discretization occured:
#' @return \strong{NBAYES$condition}, a dataframe with names of the column that was discretize and their column number
#' @return \strong{NBAYES$cuts}, a list of cuts made by discretization
#' @return \strong{NBAYES$var_a_predire}, the name of the variable to predict
#'
#'
#' @examples
#' data(iris)
#' df <- iris
#' model <- fit(Species ~ ., df, laplace=1, parallel=FALSE)
#'
#' @import discretization
#' @import stats
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom parallel detectCores
#'
#' @export


# Creation du modele (fit())
fit <- function(formula, data, laplace=1, parallel=FALSE) {
  # Nom de l'objet en sortie
  NBAYES <- list()

  # Some checks
  laplace <- as.integer(laplace)
  if ((class(laplace) != "integer") || laplace < 0) {
    stop("laplace parameter must be an integer greater than 0!")
  }

  # On extrait le dataframe de l'argument 'formula'
  # en supprimant les lignes comportant des NA
  formula <- as.formula(formula)
  df <- na.omit(model.frame(formula=formula, data=data))

  # Check dataframe, on supprime les colonnes de complex
  condition <- sum(sapply(df, class) == "complex") > 0
  if (condition) {
    stop("complex data type detected, do not handle that type of variables")
  }

  # Nombre de colonne df
  n_var <- ncol(df)
  # variable a predire
  Y <- df[1]
  # On supprime Y de df
  df <- df[-1]


  # on discretize la variable a predire si besoin, non pertinent mais utile pour les tests
  if (class(Y[,1]) == "numeric" || class(Y[,1]) == "integer") {
    Y <- mdlp(cbind(Y, Y))$Disc.data[-2]
  }


  # On cree une variable contenant que les variables prédictive quantitative
  condition1 <- sapply(df, class) == "numeric"
  condition2 <- sapply(df, class) == "integer"
  if ((sum(condition1) > 0 | sum(condition2)) > 0) {
    quanti <- df[c(which(condition1))]
    quanti <- cbind(quanti, df[c(which(condition2))])
  }

  # la même chose pour les variables qualitatives:
  # si la variable quanti existe on met dans quali seulement les variables qualitatives
  # sinon quali équivaut à l'intégralité de df
  condition1 <- sapply(df, class) == "character"
  condition2 <- sapply(df, class) == "factor"
  condition3 <- sapply(df, class) == "logic"
  if (exists('quanti') && is.data.frame(get('quanti'))) {
    quali <- df[c(which(condition1))]
    quali <- cbind(quali, df[c(which(condition2))])
    quali <- cbind(quali, df[c(which(condition3))])
  } else {
    quali <- df
  }


  # Le programme va se découper en deux partie:
  # La 1er partie qui suit se lance si et seulement si toutes les variables sont qualitatives
  # La 2ème partie se lance si le dataframe initiale est composé de variables à discrétiser

  # Si toutes les variables sont qualitatives
  if (length(quali) == n_var-1) {
    # On recree le df
    quali <- cbind(Y, quali)
    # Table frequence variable a predire
    table_Y <- table(quali[ ,1])

    # On prévient l'utilisateur de l'usage de laplace par défaut ou non
    if (laplace == 1) {
      warning("Laplace smoothing at 1 is used by default !!!")
    } else {
      warning("Laplace smoothing at ", laplace, " is used !!!")
    }

    # Cette partie permet le calcul des probabilités conditionnelles pour toutes les modalités de chaque variable
    # On commence à 2 car la 1er colonne est la variable à prédire dans ce cas
    # et on boucle sur l'ensemble des variables
    for (i in 2:ncol(quali)) {
      # Table de frequence variable explicative
      table_freq <- table(quali[ ,i], quali[ ,1])

      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut

      # Calcul des probabilites conditionnelles pour toutes les modalités de chaque variable
      nrow_tbl <- nrow(table_freq)
      # Si la vriables n'a qu'une modalité...
      if (nrow_tbl == 1) {
        proba_explicative <- prop.table(table_freq, 1)
      # Sinon...
      } else if (nrow_tbl > 1) {
        proba_explicative <- prop.table(table_freq, 2)
      }

      # On insert chaque table de probabilités conditionnelles dans la liste table_proba_cond
      # de l'objet que l'on veut retourner NBAYES
      NBAYES$table_proba_cond[[colnames(quali[i])]] <- proba_explicative
    }

    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + laplace)
    # On insere cette table dans l'objet NBAYES
    NBAYES$prior <- proba_Y


    # We're almost done, le fit est terminé, nous avons tous les éléments nécessaire à la prédiction future
    print("Fitting is done!")

    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)


    #### ELSE ####
    # Dans le cas où, au moins, une des variables du dataframe passé par l'utilisateur est quantitative,
    # les calculs de probabilités pour l'ensemble des variables se fait ici:
  } else {
    # L'utilisateur à le choix d'effectuer les calculs en parallele ou pas
    if (parallel==FALSE) {
      # Discretisaion des variables numeric
      quanti <- cbind(quanti, Y)
      df_disc <- mdlp(quanti)

      # On concatene les quanti et quali pour retrouver le DF originel discretisé
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)

      # Dans le cas de la parallélisation, on effectue la discrétisation colonne par colonne en parallèle
      # la fonction mdlp() doit prendre en dernière colonne du dataframe passé la colonne de la variable à prédire (Y)
      # d'où le cbind() dans la fonction discret() (voir sa doc pour plus d'information)
    } else if (parallel==TRUE) {
      nb_cores <- detectCores() - 1
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      quanti_discret <- foreach(i=1:ncol(quanti), .combine="c", .export=c("discret", "mdlp")) %dopar%
        discret(quanti[,i], Y)
      stopCluster(cl)


      # Etape de récupération des cuts générés par la fonction discret()
      df_discret <- df[1]
      df_discret <- df_discret[-1]
      cuts <- seq(2, length(quanti_discret), by=2)
      for (i in cuts) {
        cuts[i/2] <- quanti_discret[[i]]
        df_discret <- cbind(df_discret, quanti_discret[[i-1]])
      }
      colnames(df_discret) <- colnames(quanti)

      # On crée le même format de sortie que ce que génère la fonction mdlp() sans parallelisme.
      # Raison pratique pour l'utilisation des cuts dans la fonction predict()
      df_disc <- c()
      df_disc[["Disc.data"]] <- df_discret
      df_disc[["cutp"]] <- cuts

      # On concatene les quanti et quali pour récupérer le dataframe originel discrétisé
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)
      df_disc$Disc.data <- cbind(df_disc$Disc.data, Y)
    }

    # Le calcul des probabilités conditionnelles sur ce dataframe se fait de la même manière
    # que dans le cas de variables seulement qualitatives

    # Table frequence variable a predire
    table_Y <- table(Y)

    if (laplace == 1) {
      warning("Laplace smoothing at 1 is used by default !!!")
    } else {
      warning("Laplace smoothing at ", laplace, " is used !!!")
    }
    for (i in which(names(df_disc$Disc.data) != names(Y))) {
      # Table de frequence variable explicative
      table_freq <- table(df_disc$Disc.data[ ,i], Y[ ,1])

      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut

      # Probabilite conditionnelle pour toutes les modalités de chaque variable
      nrow_tbl <- nrow(table_freq)
      if (nrow_tbl == 1) {
        proba_explicative <- prop.table(table_freq, 1)
      } else if (nrow_tbl > 1) {
        proba_explicative <- prop.table(table_freq, 2)
      }
      NBAYES$table_proba_cond[[colnames(df_disc$Disc.data[i])]] <- proba_explicative
    }

    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + laplace)
    NBAYES$prior <- proba_Y


    # La différence mageur entre la partie nécessitant la discrétisation est les valeurs retourné
    # Pour permettre la discrétisation homogène des variables lors de la prediction ne devons renvoyer
    # certains éléments en plus

    # Donnees a passer dans le predict()
    # Colonnes ayant subit discretisation
    NBAYES$condition     <- which(sapply(df_disc$Disc.data, class) == "integer")
    # Les cuts de la discretisation
    NBAYES$cuts          <- df_disc$cutp
    # Le nom de la variable a predire
    NBAYES$var_a_predire <- formula[[2]]


    # We're almost done
    print("Fitting is done!")

    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)
  }
}


