# FUNCTION: Fonction générique pour réaliser un graph de type Sankey 
# ex: les objets sont des projets de science participative, 
# les deux types de groupes peuvent être des  taxa et de milieux.
# 
# df:         dataframe de type matrice biadjacente avec les projets en ligne,
#               et contenant les colonnes corredpondant aux noms listés dans "groupes_col"
# groupes_gauche:  vecteur de caractères correspondant aux nom de colonne à utiliser 
#               pour définir la matrice bi-adjacente, càd le nom des groupes. 
#               Chaque cellule est numérique et représente l'épaisseur des liens dans les graphe.
# groupes_droite:  Titre à utiliser pour désigner groupements

sankey_SP = function(marginal_table = NULL,
                     df = NULL,
                     gauche = NULL,
                     droite = NULL
                         ) {
require(tidyr)
require(dplyr)
require(networkD3)
  
if (is.null(marginal_table)) {
# Créer la matrice de comptage: 
  df = data.frame(gauche = df[,gauche],
                  droite = df[,droite])
  colnames(df) = c("gauche", "droite")
  marginal_table = df %>%
    count(gauche,droite,.drop = TRUE) %>%
    pivot_wider(names_from = droite,
                values_from = n, values_fill = 0) %>%
    select(-1) %>%
    as.matrix() 
    rownames(marginal_table) = unique(df$gauche)
}
  
   
# build the igraph object ####
  mat = as.matrix(marginal_table)
graph <- graph_from_biadjacency_matrix( 
  mat, 
  weighted = TRUE,
  directed = TRUE,
  multiple = FALSE,
  mode = "out")

# Graph for NetworkD3 #####
N = data.frame(name = V(graph)$name)

# Extract edgelist:
E =  data.frame(as_edgelist(graph, names = FALSE))
colnames(E) <- c("source", "target")

# Zero index the edgelist
E = E - 1

# Add weights
E$value = E(graph)$weight

# Plot
 p <- sankeyNetwork(Links = E,
              Nodes = N,
              Source = 'source',
              Target = 'target',
              Value = 'value',
              NodeID = 'name',
              units = 'TWh',
              fontSize = 12,
              nodeWidth = 30)


  return(p)
  
} 
