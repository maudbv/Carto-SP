# Fonction générique pour transformer une colonne d'élements multiples en une matrice de comptage.
col2presence <- function(df = SPdata,
                         colonne = "Taxons_corrected",
                         colonnes_a_garder = NULL,
                         nouveaux_noms = NULL,
                         binaire = TRUE,
                         rename_rows = "Nom_projet") {
  
    # Vérifier si la colonne existe
    if (!colonne %in% names(df)) {
      stop("La colonne spécifiée n'existe pas dans le data.frame.")
    }
    
    # Séparer les éléments dans la colonne cible
    listes <- lapply(strsplit(df[[colonne]], ","), function(x) trimws(x))
    names(listes) = df$Nom
    
    # Obtenir tous les éléments uniques
    elements_uniques <- sort(unique(unlist(listes)))
    
    # Créer la matrice de comptage
    matrice_occurrence <- t(sapply(listes, function(x) {
      table_fact <- table(factor(x, levels = elements_uniques))
      as.integer(table_fact)
    }))
    
    # Renomer les colonnes
    colnames(matrice_occurrence) <- elements_uniques
     
  if (binaire) {
    # Transformer en data.frame binaire (présence = 1, absence = 0)
    df_presence <- as.data.frame((matrice_occurrence > 0) * 1)
  }
  
    # Garder les colonnes demandées (si précisé)
    if (!is.null(colonnes_a_garder)) {
      if (!all(colonnes_a_garder %in% names(df))) {
        stop("Une ou plusieurs colonnes à garder n'existent pas dans le data.frame.")
      }
      df_garde <- df[ , colonnes_a_garder, drop = FALSE]
      
      # Renommer les colonnes si demandé
      if (!is.null(nouveaux_noms)) {
        if (length(nouveaux_noms) != length(colonnes_a_garder)) {
          stop("Le nombre de nouveaux noms ne correspond pas au nombre de colonnes à garder.")
        }
        colnames(df_garde) <- nouveaux_noms
      }
      
      # Combiner avec la matrice de présence
      df_resultat <- cbind(df_garde, df_presence)
    } else {
      df_resultat <- df_presence
    }
    
    # Renommer les lignes
    
    return(list(objects = elements_uniques,
                data = df_resultat ))
}
