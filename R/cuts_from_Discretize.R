library(RWeka)
library(stringr)

# Fonction pour récupérer les cuts de Discretize()
get_cuts <- function(colonne) {
  cutp <- c()
  print(levels(unique(colonne)))
  for (i in 2:length(unique(colonne))) {
    cutp[[i-1]] <- sapply(str_extract_all(levels(unique(colonne))[i], "-?[0-9.]+"), function(x) max(as.numeric(x)))
  }
  return(cutp)
}
