# Create graph 1 - Habitats
#  Une représentation centrée sur les sujets d'étude. Croiser la colonne type de programme (inventaire, suivi, signalement, recueil  / on avait parlé d'un code couleur) avec les milieux étudiés (colonne "thèmes"). Et éventuellement avoir une autre représentation type de programme / taxon.

SP_questions$`Non spécifiée` <- 0
SP_questions[which(rowSums(SP_questions[, questions])==0), "Non spécifiée"] = 1


# build the igraph object ####
graph1 <- graph_from_biadjacency_matrix(
  SP_questions[, c(questions,"Non spécifiée")],
  directed = FALSE, mode = "out")

#plot(graph1)

# with vizNetwork #####

# projet traits
nodes_proj_questions <- data.frame(
  id = 1 : length(vertex_attr(graph1)$name),
  as.data.frame(vertex_attr(graph1))
)
nodes_proj_questions$label <- ""
nodes_proj_questions$label[ nodes_proj_questions$type] <- nodes_proj_questions$name[ nodes_proj_questions$type] 

# type of node
nodes_proj_questions$layer =  c(1,2)[nodes_proj_questions$type + 1]

nodes_proj_questions$style =  c("projet","theme")[nodes_proj_questions$type + 1]
nodes_proj_questions$style[nodes_proj_questions$type == FALSE] = SP_questions$type_prog



# add info about nodes
nodes_proj_questions$def <- NA
nodes_proj_questions[match(SP_questions$projet, nodes_proj_questions$name),"def"] <-
  na.omit(SP_questions[match(nodes_proj_questions$name, SP_questions$projet),"def"])
nodes_proj_questions$def[ nodes_proj_questions$type] <- nodes_proj_questions$name[ nodes_proj_questions$type]

# give full node names 

# Couleurs
styles_col = data.frame(style = levels(as.factor(nodes_proj_questions$style)),
                        col = c(brewer.pal(n = 4, name = "Dark2"), "lightgrey")
)

# format vis
nodes_proj_questions <-  data.frame(
  nodes_proj_questions,
  # control shape of nodes_proj_questions
  shape = c("square","ellipse")[nodes_proj_questions$layer],
  # tooltip (html or character), when the mouse is above
  title = paste0("<p style=\"font-color: white;\"> <i>",
                 nodes_proj_questions$style,
                 "</i><br><b>",
                 nodes_proj_questions$name,
                 "</b><br>" ,
                 nodes_proj_questions$def,
                 "<br></p> "
  ),
  # color
  color = styles_col$col[as.numeric(as.factor(nodes_proj_questions$style))],

  # texf format
  font.color = c("black", "black")[nodes_proj_questions$layer], 
  # font.size = c(10,20)[nodes_proj_questions$group+1],
  # shadow
  shadow = FALSE 
)

#Edges
edges_proj_questions <- as.data.frame(as_edgelist(graph1, names = FALSE))
colnames(edges_proj_questions) <- c("from", "to")


# plot
plot_hyp_trait_network <- function(n = nodes_proj_questions, e = edges_proj_questions) {
  
  p <- visNetwork::visNetwork(
    n ,
    e,
    height = "600px",
    width = "100%",
    main = list(
      text = "Questions étudiées par les projets de sciences participatives",
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
                              values = levels(as.factor(nodes_proj_questions$style))[1:4],
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
lnodes <- data.frame(label = styles_col$style[1:4], 
                     shape =  "square",
                     size = 6,
                     color = styles_col$col[1:4],
                     font = list( color = styles_col$col[1:4],
                                  size = 28, 
                                  face = "bold",
                                  align = "left")) 


(p <- plot_hyp_trait_network () %>%
  visInteraction(tooltipStyle = 'position: fixed; text-align: left; visibility:hidden;padding: 5px;font-family: verdana;font-size:14px;font-color:#00000;
                   background-color: white;-moz-border-radius: 3px;*-webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
                   box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);max-width:400px;
                   word-break: break-word;
                    line-height: 1.4') %>%
    visLegend(addNodes = lnodes,
              zoom = FALSE,  
              width = 0.20, stepY = 100,
              position = "left",
              useGroups = FALSE)
)



