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


# Load functions
source("R/function col2matrix.r")

# Importer et formatter les données
source("R/Import data.r")

# Importer fonction pour dessiner les graphes: 
source("R/carto_SP_graph.r")

# Graph 1: Projets et milieux
graph_milieux <- carto_SP_graph(df = SP_milieux,
                                groupes_col = milieux,
                                groupes_titre = "Milieux",
                                couleurs_col = "type_participants",
                                couleurs_label = types_part$court,
                                titre = "Milieux explorés par les projets de sciences participatives",
                                hauteur = "600px")
                                
                            
# Graph 2: Projets et partenaires
graph_partenaires <- carto_SP_graph(df = SP_acteurs,
                                groupes_col = types_acteurs,
                                groupes_titre = "Type de partenaire",
                                couleurs_col = "type_participants",
                                couleurs_label = types_part$court,
                                titre = "Cartographie des partenaires des projets de sciences participatives",
                                hauteur = "600px")

# Graph 3: Sankey diagram taxons et milieux

