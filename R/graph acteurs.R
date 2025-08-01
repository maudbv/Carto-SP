# GRaph 2 
# 2/ Une représentation centrée sur les acteurs mobilisés. Relier chaque programme avec les types d'acteurs impliqués (ceux mentionnés dans les colonnes "nom de la structure animatrice" et "responsable scientifique") que l'on classe en 5 grands types (cf index acteurs : académique, associatif, gestionnaire d'espaces, collectivité, entreprise). Et indiquer par un code couleur le type de participants impliqués dans chaque programme (colonne J)


# TO DO: 
#  maj liste de porteurs >2
# épaisseur des liens=  nombre de connections.

# build the igraph object ####
graph2 <- graph_from_biadjacency_matrix(
  SP_acteurs[,types_act],
  weighted = TRUE,
  directed = TRUE,
  mode = "out")


# Graph avec vizNetwork #####

# Define nodes
nodes_proj_acteurs <- data.frame(
  id = 1 : length(vertex_attr(graph2)$name),
  as.data.frame(vertex_attr(graph2))
  )

# Add label
nodes_proj_acteurs$label <- ""
nodes_proj_acteurs$label[ nodes_proj_acteurs$type] <- nodes_proj_acteurs$name[ nodes_proj_acteurs$type] 

# type of node
nodes_proj_acteurs$layer =  c(1,2)[nodes_proj_acteurs$type + 1]

# define style (base on type of structure)
nodes_proj_acteurs$style =  c("projet","str_type")[nodes_proj_acteurs$type + 1]
nodes_proj_acteurs$style[nodes_proj_acteurs$type == FALSE] = SP_acteurs$`Type participants`

# add info about nodes
nodes_proj_acteurs$def <- NA
nodes_proj_acteurs[match(SP_acteurs$Nom, nodes_proj_acteurs$name),"def"] <-
  SP_acteurs[na.omit(match(nodes_proj_acteurs$name, SP_acteurs$Nom)),"Résumé de l'observatoire"]
nodes_proj_acteurs$def[ nodes_proj_acteurs$type] <- nodes_proj_acteurs$name[ nodes_proj_acteurs$type]

# A FAIRE: créer des acronymes de projets
# nodes_proj_acteurs$name <- hyp_mat[match(nodes_proj_acteurs$label,
#                                       hyp_mat$Acronym),
#                                 "Hypothesis"]
# nodes_proj_acteurs$name [ nodes_proj_acteurs$type] <-    nodes_proj_acteurs$def[ nodes_proj_acteurs$type]


# give full node names 


# colors ####
styles_col = data.frame( style = levels(as.factor(nodes_proj_acteurs$style)) ,
                         label = c(type_part$court,"structure"),
                         col =  brewer.pal(n = 7, name = "Dark2"))
styles_col[styles_col$style == "str_type","col"] = "lightgrey"

# format nodes ####
nodes_proj_acteurs <-  data.frame(
  nodes_proj_acteurs,
  # control shape of nodes_proj_acteurs
  shape = c("square","ellipse")[nodes_proj_acteurs$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p style=\"font-color: white;\"> <i>",
                 nodes_proj_acteurs$style,
                 "</i><br><b>",
                 nodes_proj_acteurs$name,
                 "</b><br>" ,
                 nodes_proj_acteurs$def,
                 "<br></p> "
  ),
  # color
  # color = styles_col$col[match(nodes_proj_acteurs$style, styles_col$style)],
   color = styles_col$col[as.numeric(as.factor(nodes_proj_acteurs$style))],

  
  # texf format
  font.color = "black", 
  # font.size = c(10,20)[nodes_proj_acteurs$style+1],
  
  # shadow
  shadow = FALSE 
)

# Format Edges ####
edges_proj_acteurs <- as.data.frame(as_edgelist(graph2, names = FALSE))
colnames(edges_proj_acteurs) <- c("from", "to")

# plot ####
plot_hyp_trait_network <- function(n = nodes_proj_acteurs, e = edges_proj_acteurs) {
  
  p <- visNetwork::visNetwork(
    n ,
    e,
    height = "600px",
    width = "100%",
    main = list(
      text = "Les types de porteurs de projets de sciences participatives",
      style = "font-family:Arial;color:#0085AF;font-size:18px;
      text-align:center;text-align:center; max-width:400px;
      margin: auto; word-break: break-word; line-height: 1.4'")) %>%
    visNodes(
      font = list(size = 80)
    ) %>%
    
    visEdges(
      shadow = FALSE,
      color = list(color = "#0085AF", highlight = "#C62F4B")
    ) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
               autoResize = FALSE,
               selectedBy = list(variable = "style", 
                                 multiple = FALSE,
                                 highlight = TRUE,
                                 main = "Type de participants",
                                 values = levels(as.factor(nodes_proj_acteurs$style))[1:6]
               ),
               manipulation = FALSE) %>%
    
    visPhysics(    enabled = FALSE,
                   
                   stabilization = c(
                     enabled = TRUE,
                     iterations =1000,
                     updateInterval= 100,
                     onlyDynamicEdges = FALSE,
                     fit = TRUE
                   ),
                   
                   forceAtlas2Based = c(
                     theta=0.5,
                     gravitationalConstant= -50,
                     centralGravity= 0.01,
                     springConstant= 0.08,
                     springLength= 100,
                     damping= 0.4,
                     avoidOverlap=  0
                   )
    ) %>%
    visInteraction(navigationButtons = FALSE) %>%
    visIgraphLayout(layout = "layout.fruchterman.reingold") 
  
  return(p)
  
} 

# passing custom nodes and/or edges
lnodes <- data.frame(label = styles_col$label[1:6], 
                     shape =  "square",
                     size = 6,
                     color = styles_col$col[1:6],
                     font = list( color = styles_col$col[1:6],
                                  size = 24, 
                                  face = "bold",
                                  align = "left")) 

(p <- plot_hyp_trait_network () %>%
    visInteraction(tooltipStyle = 'position: fixed; text-align: left; visibility:hidden;padding: 5px;font-family: verdana;font-size:14px;font-color:#00000;
                   background-color: white;-moz-border-radius: 3px;*-webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
                   box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2); max-width:400px;
                   word-break: break-word;
                    line-height: 1.4') %>%
    visLegend(addNodes = lnodes,
              zoom = FALSE,  
              width = 0.15, stepY = 100,
              position = "left",
              useGroups = FALSE)
)

# # Try other network vis
# library(tidyverse)
# library(networkD3)
# library(igraph)
# 
# nodes <- nodes_proj_acteurs
# edges <- edges_proj_acteurs
# x= bipartite_mapping(graph2)$type
# 
# g_igraph = make_bipartite_graph(types = x,
#                          edges = as.vector(t(as_edgelist(graph2))))
# plot(g)
#           
# g = igraph_to_networkD3(g,
#                         group = bipartite_mapping(graph2)$type )          
#           
#           
# forceNetwork(
#   Links = g$links, Nodes = g$nodes, 
#   Source ='source', Target = 'target',
#   NodeID = "name", Group = "group",
#   fontSize = 16)
