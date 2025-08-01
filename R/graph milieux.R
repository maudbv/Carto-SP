# Create graph 1 - milieux
# Une représentation centrée sur les milieux d'étude. 
# Croiser la colonne type de programme (inventaire, suivi, signalement, recueil  / on avait parlé d'un code couleur) avec les milieux étudiés (colonne "thèmes"). Et éventuellement avoir une autre représentation type de programme / taxon.

 
# build the igraph object ####
graph1 <- graph_from_biadjacency_matrix(
  SP_milieux[, milieux],
  directed = FALSE, mode = "out")

#plot(graph1)


# with vizNetwork #####

# projet traits
nodes_proj_milieux <- data.frame(
  id = 1 : length(vertex_attr(graph1)$name),
  as.data.frame(vertex_attr(graph1))
)
nodes_proj_milieux$label <- ""
nodes_proj_milieux$label[ nodes_proj_milieux$type] <- nodes_proj_milieux$name[ nodes_proj_milieux$type] 

# type of node
nodes_proj_milieux$layer =  c(1,2)[nodes_proj_milieux$type + 1]

nodes_proj_milieux$style =  c("projet","milieu")[nodes_proj_milieux$type + 1]
nodes_proj_milieux$style[nodes_proj_milieux$type == FALSE] = SP_milieux$type_prog

# Assigner les niveaux de facteur pour le style des noeuds
types = as.character(na.omit(unique(SP_milieux$type_prog)))
nodes_proj_milieux$style = factor(nodes_proj_milieux$style,
                                  levels = c(types,"milieu"))

# Couleurs

styles_col = data.frame(style = levels(as.factor(nodes_proj_milieux$style)),
                        col = c(brewer.pal(n = length(levels(as.factor(nodes_proj_milieux$style))) -1,
                                           name = "Dark2"),
                                "lightgrey")
)



# add project definitions
nodes_proj_milieux$def <- NA
nodes_proj_milieux[match(SP_milieux$projet, nodes_proj_milieux$name),"def"] <-
  SP_milieux[na.omit(match(nodes_proj_milieux$name, SP_milieux$projet)),"def"]
nodes_proj_milieux$def[ nodes_proj_milieux$type] <- ""

# add partner info
nodes_proj_milieux$partenaires <- NA
nodes_proj_milieux[match(SP_milieux$projet, nodes_proj_milieux$name),"partenaires"] <-
  SP_milieux[na.omit(match(nodes_proj_milieux$name, SP_milieux$projet)),"partenaires"]
nodes_proj_milieux$partenaires[ nodes_proj_milieux$type] <- ""

# give full node names 

# format vis
nodes_proj_milieux <-  data.frame(
  nodes_proj_milieux,
  # control shape of nodes_proj_milieux
  shape = c("square","ellipse")[nodes_proj_milieux$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p style=\"font-color: white;\"> <i>",
                 nodes_proj_milieux$style,
                 "</i><br><b>",
                 nodes_proj_milieux$name,
                 "</b><br>" ,
                 nodes_proj_milieux$def,
                 "<br> <br> <i>" ,
                 nodes_proj_milieux$partenaires,
                 "</i> <br></p> "
  ),
  # color
  color = styles_col$col[as.numeric(as.factor(nodes_proj_milieux$style))],
  #  "Inventaire","Recueil (Savoirs-Pratiques)", "Signalement","Suivi","theme"  
  
  # texf format
  font.color = c("black", "black")[nodes_proj_milieux$layer], 
  # font.size = c(10,20)[nodes_proj_milieux$group+1],
  # shadow
  shadow = FALSE 
)

#Edges
edges_proj_milieux <- as.data.frame(as_edgelist(graph1, names = FALSE))
colnames(edges_proj_milieux) <- c("from", "to")

# plot
plot_hyp_trait_network <- function(n = nodes_proj_milieux, e = edges_proj_milieux) {
  
  p <- visNetwork::visNetwork(
    n ,
    e,
    height = "600px",
    width = "100%",
    main = list(
      text = "Milieux étudiés par les projets de sciences participatives",
      style = "font-family:Arial;color:#0085AF;font-size:18px;
      text-align:center;text-align:center; max-width:400px;
      margin: auto; word-break: break-word; line-height: 1.4'")) %>%
    visNodes(
      font = list(size = 70),
      
    ) %>%
    visEdges(
      shadow = FALSE,
      color = list(color = "#0085AF", highlight = "#C62F4B")
    ) %>%
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
               autoResize = FALSE,
               selectedBy = list(variable = "style",
                                 main = "Type de programme",
                              multiple = FALSE,
                              values = levels(as.factor(nodes_proj_milieux$style))[1:4],
                              highlight = TRUE
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
    visInteraction(navigationButtons = FALSE, zoomView = FALSE) %>%
  visIgraphLayout(layout = "layout.fruchterman.reingold",
                  type = "full" )
  
  return(p)
  
}

# passing custom nodes and/or edges
lnodes <- data.frame(label = styles_col$style[1:3], 
                     shape =  "square",
                     size = 5,
                     color = styles_col$col[1:3],
                     font = list( color = styles_col$col[1:3],
                                  size = 15, 
                                  face = "bold",
                                  align = "left")) 


(p <- plot_hyp_trait_network () %>%
  visInteraction(tooltipStyle = 'position: fixed; text-align: left; visibility:hidden;padding: 5px;font-family: verdana;font-size:14px;font-color:#00000;
                   background-color: white;-moz-border-radius: 3px;*-webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
                   box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);max-width:500px;max-height:800px
                   word-break: break-word;
                    line-height: 1.4') %>%
    visLegend(addNodes = lnodes,
              zoom = FALSE,  
              width = 0.2,
              stepY =50,
              position = "left",
              useGroups = FALSE)
)



