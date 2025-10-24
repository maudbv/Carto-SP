# FUNCTION: Fonction générique pour réaliser un graph bipartite non hierarchique connectant objets à des groupes
# ex: les objets sont des projets de science participative, 
# les groupes peuvent être des types de partenaires, ou de milieux.
# 
# df:         dataframe de type matrice biadjacente avec les projets en ligne,
#               et contenant les colonnes corredpondant aux noms listés dans "groupes_col"
# groupes_col:  vecteur de caractères correspondant aux nom de colonne à utiliser 
#               pour définir la matrice bi-adjacente, càd le nom des groupes. 
#               Chaque cellule est numérique et représente l'épaisseur des liens dans les graphe.
# groupes_titre:  Titre à utiliser pour désigner groupements
# couleurs_col:   nom de colonne à utiliser pour coder les couleurs des projets
# couleurs_label: vecteur de caractères, listant les noms uniques et l'ordre de la légende couleur
# titre:          chaine de caractère affichée comme titre en haut du graph.

carto_SP_graph = function(df = SP_acteurs,
                          groupes_col = types_acteurs,
                          groupes_titre = "Type de partenaires",
                          couleurs_col = "type_participants",
                          couleurs_label = types_part$court,
                          selection_col = "type_participants",
                          selection_titre = "Type de participants",
                          titre = "",
                          hauteur = "600px") {
  
# build the igraph object ####
  mat = as.matrix(dplyr::select(df, any_of(groupes_col)))

graph2 <- graph_from_biadjacency_matrix( 
  mat, 
  weighted = TRUE,
  directed = TRUE,
  multiple = FALSE,
  mode = "out")

if (length(couleurs_label) == 0 ) {
  couleurs_label = levels(as.factor(df[, couleurs_col]))
}
                                                   

# Graph avec vizNetwork #####

# Define nodes
nodes_proj <- data.frame(
  id = 1 : length(vertex_attr(graph2)$name),
  as.data.frame(vertex_attr(graph2))
  )

# Add label
nodes_proj$label <- ""
nodes_proj$label[ nodes_proj$type] <- nodes_proj$name[nodes_proj$type] 

# type of node
nodes_proj$layer =  c(1,2)[nodes_proj$type + 1]

# define style (base on type of structure)
nodes_proj$style =  c("projet",groupes_titre)[nodes_proj$type + 1]
nodes_proj$style[nodes_proj$type == FALSE] = df[, couleurs_col]
nodes_proj$style = factor(x = nodes_proj$style,
                          levels = c(levels(as.factor(df[, couleurs_col])),
                                     groupes_titre))

# add info about nodes
nodes_proj$def <- NA
nodes_proj[match(df$Nom_projet, nodes_proj$name),"def"] <-
  df[na.omit(match(nodes_proj$name, df$Nom_projet)),"def"]
nodes_proj$def[ nodes_proj$type] <- ""

# info partenaires
nodes_proj$partenaires <- NA
nodes_proj[match(df$Nom_projet, nodes_proj$name),"partenaires"] <-
  df[na.omit(match(nodes_proj$name, df$Nom_projet)),"partenaires"]
nodes_proj$partenaires[ nodes_proj$type] <- ""

# info partenaires
nodes_proj$selection <- NA
nodes_proj[match(df$Nom_projet, nodes_proj$name),"selection"] <-
  df[na.omit(match(nodes_proj$name, df$Nom_projet)),selection_col]
nodes_proj$selection[nodes_proj$type] <- ""


# colors ####
styles_col = data.frame( style = levels(nodes_proj$style) ,
                         label = c(couleurs_label,"Type de partenaire"),
                         col =  c( brewer.pal(n = length(couleurs_label),
                                              name = "Dark2"),
                                   "lightgrey"))


# format nodes ####
nodes_proj <-  data.frame(
  nodes_proj,
  # control shape of nodes_proj
  shape = c("square","ellipse")[nodes_proj$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p style=\"font-color: white;\"> <i>",
                 nodes_proj$style,
                 "</i><br><b>",
                 nodes_proj$name,
                 "</b><br>" ,
                 nodes_proj$def,
                 "<br> <br> <i>" ,
                 nodes_proj$partenaires,
                 "</i> <br></p> "
  ),
  # color
  color = styles_col$col[as.numeric(as.factor(nodes_proj$style))],
  # color = list(background = styles_col$col[as.numeric(as.factor(nodes_proj$style))],
  #      border = "blue", 
  #      highlight = "yellow"),
  
  # text format
  font.color = "black", 
  # font.size = c(10,20)[nodes_proj$style+1],
  
  # shadow
  shadow = FALSE 
)

# Format Edges ####
edges_proj <- as.data.frame(as_edgelist(graph2, names = FALSE))
colnames(edges_proj) <- c("from", "to")
edges_proj$width <- (E(graph2)$weight * 1.5)^1.5


# plot ####
  p <- visNetwork::visNetwork(
    nodes_proj ,
    edges_proj,
    height = hauteur,
    width = "100%",
    main = list(
      text = titre,
      style = "font-family:Arial;color:'black';font-size:18px;
      text-align:center;text-align:center; max-width:800px;
      margin: auto; word-break: break-word; line-height: 1.4'")) %>%
  
    visNodes(
      font = list(size = 60)
    ) %>%

    visEdges(
      shadow = FALSE,
      color = list(color = "darkgrey", highlight = "#C62F4B")
    ) %>%
  
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
               autoResize = FALSE,
               selectedBy = list(variable =  "selection", 
                                 multiple = FALSE,
                                 highlight = TRUE,
                                 main =  selection_titre,
                                 values = unique(nodes_proj$selection[which(nodes_proj$layer ==1)])
               ),
               manipulation = FALSE) %>%
  
    visPhysics(enabled = FALSE) %>%
  
    visInteraction(navigationButtons = FALSE) %>%
  
    visIgraphLayout(layout = "layout.fruchterman.reingold",
                    type = "full") 
  
# Legend nodes ####
lnodes <- data.frame(label = styles_col$label[1:6], 
                     shape =  "square",
                     size = 6,
                     color = styles_col$col[1:6],
                     font = list( color = styles_col$col[1:6],
                                  size = 25, 
                                  face = "Arial",
                                  align = "left")) 


p <- p %>%
    visInteraction(tooltipStyle = 'position: fixed; text-align: left; visibility:hidden;padding: 5px;font-family: verdana;font-size:14px;font-color:#00000;
                   background-color: white;-moz-border-radius: 3px;*-webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
                   box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2); max-width:400px;
                   word-break: break-word;
                    line-height: 1.4') %>%
    visLegend(addNodes = lnodes,
              zoom = FALSE,  
              width = 0.20, stepY = 100,
              position = "left",
              useGroups = FALSE)
return(p)
}

