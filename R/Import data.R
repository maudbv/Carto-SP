# Import data 
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(readxl)
library(networkD3)

# read data ####
SPdata <- read_excel(path = "data/carto_analytique.xlsx",
                     sheet = "Tableau Maud")


# Taxons ####
SP_taxons = col2matrix(df = SPdata, 
                       column = "Taxons",
                       name_col = "Nom",
                       sep = ",", 
                       id_cols = c("Milieux","Taxons",
                                   "Partenaires", "Type participants",
                                   "Résumé de l'observatoire"),
                       id_cols_new = c("milieux","taxons",
                                       "partenaires", "type_participants",
                                       "def"))
taxons = SP_taxons$objects
SP_taxons = SP_taxons$data

# Nouveaux Taxons
data_taxons <- as.data.frame(read_excel("data/carto_analytique.xlsx",
                                        sheet = "Index taxons") )

SPdata$Taxons_corrected <- change_taxonomy(df = SPdata,
                                           col = "Taxons",
                                           index = data_taxons ,
                                           col_init = "TAXONS TABLEAU",
                                           col_finale = "TAXONS FINAUX") 



# Milieux ####
SP_milieux = col2matrix(df = SPdata, 
                       column = "Milieux",
                       name_col = "Nom",
                       sep = ",", 
                       id_cols = c( "Taxons",
                                   "Partenaires", "Type participants",
                                   "Résumé de l'observatoire"),
                       id_cols_new = c( "taxons",
                                       "partenaires", "type_participants",
                                       "def"))
milieux = SP_milieux$objects
SP_milieux = SP_milieux$data

# Taxons ####

# Acteurs ####

# importer le tableau classifiant les acteurs
data_acteurs <- as.data.frame(read_excel("data/carto_analytique.xlsx",
                                         sheet = "Index acteurs") )
types_acteurs <- names(data_acteurs)[-c(1,2)]

# Convertir en 0 et 1: 
data_acteurs[data_acteurs == "x"] <- 1
data_acteurs[is.na(data_acteurs)] <- 0

# Restreindre aux noms simplifiés: 
data_acteurs <- data_acteurs %>%
  select(-1) %>%
  unique()

# Extraire type d'acteur unique:
data_acteurs <- pivot_longer(data = data_acteurs,
                             cols = types_acteurs) %>%
  filter(value == 1) %>%
  select(-value)

# Table pour résumer les types d'acteurs pour chaque programme
SP_acteurs = as.data.frame(matrix(0,
                                  nrow(SPdata),
                                  length(types_acteurs),
                                  dimnames = list(SPdata$Nom, 
                                                  types_acteurs)))
SP_acteurs$partenaires = SPdata$Partenaires
SP_acteurs$projet = SPdata$Nom
SP_acteurs$def = SPdata$`Résumé de l'observatoire`

# Caractériser le type de programme: Effort
SP_acteurs$type_participants = SPdata$`Type participants`

# Fill the incidence matrix:
for (i in 1:nrow(data_acteurs)) {
  act = as.character(data_acteurs[i, "Nom_simple"])
  type = as.character(data_acteurs[i, "name"])
  lines = grep(act, SP_acteurs$partenaires)
  if (length(lines)>0) {
    previous_values = SP_acteurs[lines,type]
    SP_acteurs[lines,type] = previous_values + 1
    rm(previous_values)
  }
}

rownames(SP_acteurs) = SP_acteurs$projet

# Simplifier nom des participants:
types_part = data.frame(long = levels (as.factor(SP_acteurs$type_part)),
                        court  = c("Grand Public",
                                   "Naturalistes bénévoles",
                                   "Professionnels espaces naturels",
                                   "Professionnels espaces verts",
                                   "Professionnels agricole",
                                   "Scolaires")
)
