# Shiny app pour cartographier les projets de sciences participatives en France.
# author: Maud Bernard-Verdier


# Load packages ####
library(shiny)
require(shinyWidgets)
require(shinythemes)
library(plotly)
library(DT)
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

# Importer et formatter les données ####
source("R/Import data.R")

# User Interface ####
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("paper"),
             collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#27596B;" class="active" href="#">Cartographie des Sciences participatives en France</a>'),
             id="nav",
             windowTitle = "Cartographie des Sciences Participatives",
             
             # Panel 1: Partenaires
             tabPanel("Partenaires",
                      div(
                        visNetworkOutput('graph_partenaires', height = "600px"),
                        style = 'max-width: 1000px;max-height: 1000px;')
                      ),

             # Panel 2: Milieux
             tabPanel("Milieux",
                      div(
                        visNetworkOutput('graph_milieux', height = "600px"),
                        style = 'max-width: 1000px;')
                      ),

             # # Panel final : Data table
             # tabPanel("Data",
             #          div(
             #            DT::DTOutput("") 
             #            ,style = 'max-width: 3000px;'
             #            )
             # ),
             
             # footer: about the project
             footer = list(
               tags$br(),
               tags$br(),
               hr(), ' Données assemblées par Anne Dozière, Alexandra Villarroel Parada, Alice Couëtil. Web app construite par Maud Bernard-Verdier avec R shiny et VisNetwork. Novembre 2025',
               tags$br(),
               tags$br()
             )
             
  )
)

# Server:   ####

server <- function(input, output, session) {

  
  # Graph Partenaires
  output$graph_partenaires <- renderVisNetwork({
    carto_SP_graph(df = SP_acteurs,
                   groupes_col = types_acteurs,
                   groupes_titre = "Type de partenaire",
                   couleurs_col = "type_participants",
                   couleurs_label = types_part$court,
                   selection_col = "type_participants",
                   selection_titre = "Type de participants",
                   titre = "Partenaires des sciences participatives",
                   hauteur = "800px")
  })
 
  # Graph Milieux
  output$graph_milieux <- renderVisNetwork({
    carto_SP_graph(df = SP_milieux,
                   groupes_col = milieux,
                   groupes_titre = "Milieux",
                   couleurs_col = "type_participants",
                   couleurs_label = types_part$court,
                   titre = "Milieux explorés par les sciences participatives",
                   hauteur = "800px")
  })
  
 #  # Data table
 #  output$filtered_data = DT::renderDT({
 #    req(filtered_df())
 #    display_columns <- c("Title","support_for_hypothesis","Investigated_species","Habitat","Research_Method", "continents","Study_date", "hypothesis", "DOI") 
 #    df <-  as.data.frame(filtered_df())
 #    rownames(df) <- df$index
 #    df <-  df[, display_columns]
 # datatable(df,
 #           rownames = FALSE,
 #           extensions = 'Buttons',
 #           options = list(
 #             dom = 'Bfrtip',
 #             exportOptions = list(header = ""),
 #             buttons = list(
 #               list(
 #                 extend = "csv", 
 #                 filename = 'export',
 #                 text = "Download", 
 #                 title = NULL
 #                 )
 #               )
 #           )
 # )
 # 
 #  }, server = FALSE)
}
  
 
shinyApp(ui = ui, server = server)
