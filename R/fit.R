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
#' @return an NBAYES object containing several information that can be useful for the user but more important
#' for the predict function
#'
#' @examples
#' data(iris)
#' df <- iris
#' model <- NBC::fit(Species ~ ., data, laplace=1, parallel=FALSE)
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

  # On extrait le dataframe de l'argument 'formula'
  # en supprimant les lignes comportant des NA
  formula <- as.formula(formula)
  df <- na.omit(model.frame(formula = formula, data=data))

  # Nombre de colonne df
  n_var <- ncol(df)
  # variable a predire
  Y <- df[1]
  # On supprime Y de df
  df <- df[-1]

  # on discretize la variable a predire si besoin
  if (class(Y[,1]) == "numeric" || class(Y[,1]) == "integer") {
    Y <- mdlp(cbind(Y, Y))$Disc.data[-2]
  }


  # Variable quanti
  condition1 <- sapply(df, class) == "numeric"
  condition2 <- sapply(df, class) == "integer"
  if ((sum(condition1) > 0 | sum(condition2)) > 0) {
    quanti <- df[c(which(condition1))]
    quanti <- cbind(quanti, df[c(which(condition2))])
  }

  # Variable quali
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

  # On met toutes les colonnes en Factor
  #df <- as.data.frame(lapply(df, factor))

  if (length(quali) == n_var-1) {
    # On recree le df
    quali <- cbind(Y, quali)
    # Table frequence variable a predire
    table_Y <- table(quali[ ,1])

    warning("Laplace smoothing at 1 is used by default !!!")
    for (i in 2:ncol(quali)) {
      # Table de frequence variable explicative
      table_freq <- table(quali[ ,i], quali[ ,1])

      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut

      # Probabilite conditionnelle pour toutes les modalités de chaque variable
      proba_explicative <- prop.table(table_freq, 2)
      NBAYES$table_proba_cond[[colnames(quali[i])]] <- prop.table(table_freq, 2)
    }

    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + 1)
    NBAYES$prior <- proba_Y

    # We're almost done
    print("Fitting is done!")

    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)


    #### ELSE ####
  } else {
    if (parallel==FALSE) {
      # Discretisaion des variables numeric
      quanti <- cbind(quanti, Y)
      df_disc <- mdlp(quanti)

      # On concatene les quanti et quali pour retrouver le DF originel discretise
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)

    } else if (parallel==TRUE) {
      nb_cores <- detectCores()
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      quanti_discret <- foreach(i=1:ncol(quanti), .combine="c", .export=c("discret", "mdlp")) %dopar%
        discret(quanti[,i], Y)
      stopCluster(cl)

      df_discret <- df[1]
      df_discret <- df_discret[-1]
      cuts <- seq(2, length(quanti_discret), by=2)
      for (i in cuts) {
        cuts[i/2] <- quanti_discret[[i]]
        df_discret <- cbind(df_discret, quanti_discret[[i-1]])
      }
      colnames(df_discret) <- colnames(quanti)

      df_disc <- c()
      df_disc[["Disc.data"]] <- df_discret
      df_disc[["cutp"]] <- cuts

      # On concatene les quanti et quali
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)
      df_disc$Disc.data <- cbind(df_disc$Disc.data, Y)
    }



    # Table frequence variable a predire
    table_Y <- table(Y)

    warning("Laplace smoothing at 1 is used by default !!!")
    for (i in which(names(df_disc$Disc.data) != names(Y))) {
      # Table de frequence variable explicative
      table_freq <- table(df_disc$Disc.data[ ,i], Y[,1])

      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut

      # Probabilite conditionnelle pour toutes les modalités de chaque variable
      proba_explicative <- prop.table(table_freq, 2)
      NBAYES$table_proba_cond[[colnames(df_disc$Disc.data[i])]] <- prop.table(table_freq, 2)
    }

    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + 1)
    NBAYES$prior <- proba_Y

    # Donnees a passer dans le predict()
    NBAYES$discretiz     <- TRUE
    # Colonnes ayant subit discretisation
    NBAYES$condition     <- which(sapply(df_disc$Disc.data, class) == "integer")
    # Nbre de colonne du jeu de donnee d'entrainement
    NBAYES$n_col_train   <- ncol(df)
    NBAYES$cuts          <- df_disc$cutp
    NBAYES$var_a_predire <- formula[[2]]

    # We're almost done
    print("Fitting is done!")

    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)
  }
}


