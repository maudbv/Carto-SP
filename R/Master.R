# Cartographie des projets de Sciences Participatives

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(igraph)
library(networkD3)
library(visNetwork)
library(stringr)
library(readxl)
library(RColorBrewer)


# Charger les fonctions nécessaires ####
source("R/function col2presence.r")
source("R/function change_taxonomy.r")
source("R/function cross_adjacency.r")
source("R/carto_SP_graph.r")
source("R/sankey_SP.r")

# Importer et formatter les données ####
source("R/Import data.r")


# Graph 1: Projets et milieux ####
graph_milieux <- carto_SP_graph(df = SP_milieux,
                                groupes_col = milieux,
                                groupes_titre = "Milieux",
                                couleurs_col = "type_participants",
                                couleurs_label = types_part$court,
                                titre = "Milieux explorés par les projets de sciences participatives",
                                hauteur = "600px")


# Graph bonus: Projets et Taxons ####
graph_taxons <- carto_SP_graph(df = SP_taxons,
                                groupes_col = taxons,
                                groupes_titre = "Taxons",
                                couleurs_col = "type_participants",
                                couleurs_label = types_part$court,
                                titre = "Taxons représentés par les projets de sciences participatives",
                                hauteur = "600px")

                            
# Graph 2: Projets et partenaires ####
graph_partenaires <- carto_SP_graph(df = SP_acteurs,
                                groupes_col = types_acteurs,
                                groupes_titre = "Type de partenaire",
                                couleurs_col = "type_participants",
                                couleurs_label = types_part$court,
                                titre = "Cartographie des partenaires des projets de sciences participatives",
                                hauteur = "600px")

# Graph 3: Sankey diagram taxons et milieux : aléatoire, bof ####

# Matrice de comptage
taxons_milieux <- cross_adjacency(mat1 = SP_taxons,
                                  mat2 = SP_milieux,
                                  cols1 = taxons,
                                  cols2 = milieux)
# diagram de Sankey
sankey_SP(marginal_table = taxons_milieux)

# Graph bonus: Acteurs ####
# Matrice de comptage
acteurs_milieux <- cross_adjacency(mat1 = SP_milieux,
                                  mat2 = SP_acteurs,
                                  cols1 = milieux,
                                  cols2 = types_acteurs)
# diagram de Sankey
sankey_SP(marginal_table = acteurs_milieux)

# # Graph bonus: 

# diagram de Sankey effort:
sankey_SP(df = SPdata, 
          gauche = "Type participants",
          droite = "Effort")

sankey_SP(df = SPdata, 
          gauche = "Effort",
          droite = "Objet")

sankey_SP(df = SPdata, 
          gauche = "Type participants",
          droite = "Donnée de contexte")

# diagram de Sankey types participant et effort ####
SP_participants = col2presence(df = SPdata, colonne = "Type participants")
participants = SP_participants$objects
SP_participants = SP_participants$data
participants_milieux <- cross_adjacency(mat1 = SP_participants,
                                   mat2 = SP_milieux,
                                   cols1 = participants,
                                   cols2 = milieux)
sankey_SP(marginal_table = participants_milieux)


