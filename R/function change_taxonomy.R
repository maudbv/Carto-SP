# Function to replace a taxonomy by another in a column listing obects

change_taxonomy = function (df = SPdata,
                            col = "Taxons",
                            index = data_taxons ,
                            col_init = "TAXONS TABLEAU",
                            col_finale = "TAXONS FINAUX") {
  
  colonne = as.data.frame(df)[, col]
  
  # Étape 1 : créer un vecteur de correspondance
  dico <- setNames(index[, col_finale], index[, col_init])
  
  # Étape 2 : fonction pour corriger une cellule
  corriger_cellule <- function(cellule) {
    noms <- trimws(unlist(strsplit(cellule, ",")))              # séparer et nettoyer
    noms_corriges <- dico[noms]                                 # remplacements
    noms_corriges <- unique(na.omit(noms_corriges))             # enlever NA et doublons
    paste(noms_corriges, collapse = ", ")                       # reformater
  }
  
  # Étape 3 : appliquer à toute la colonne
  colonne_corrigee <- sapply(colonne, corriger_cellule)

  return(colonne_corrigee)
 }

