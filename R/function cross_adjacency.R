# Fonction: faire correspondre deux tables de comptages avec nom des lignes en commun
# crée une nouvelle table de comptage avec en colonne les colonnes de table 1, 
# et en ligne les colonnes de table 2
# Exemple: compter le nombre de projets de SP concernant un milieu et un taxon donné
# Permet de réaliser un diagramm Sankey des Taxons dans les milieux.

cross_adjacency <- function( mat1 = SP_taxons,
                             mat2 = SP_milieux,
                             cols1 = taxons,
                             cols2 = milieux) {
  
  # # Make sure rownames correspond
  # if(!all(rownames(mat1) == rownames(mat2))) stop ("les noms de lignes (= noms de projets) ne correspondent pas")
  
  
  # Make sure the column names are correct
  if(ncol(select(mat1, any_of(cols1))) != length(cols1)) warning("not all cols1 are in mat1")
     if(ncol(select(mat2, any_of(cols2))) != length(cols2)) warning("not all cols1 are in mat1")
  
  # Prepare dataframe to fill:
  out = data.frame(matrix(NA,
                          nrow = length(cols1),
                          ncol = length(cols2)))
  rownames(out) = cols1
  names(out) = cols2
  
  # Go through all the columns in the i matrix:
  for (i in cols1) {
    
    # extract the rows for which the i-th column items are present (= 1 or more):
    tax_rows = which(mat1[ ,i] > 0 )
    
    # Extract the same rows in mat2 and count how many of each column there are for this subset of mat2:
    count_mat2 = colSums(mat2[tax_rows, cols2])
    
    # replace correpsonding line in output table: 
    out[i,] =  count_mat2
  }
  
  return(out)
}