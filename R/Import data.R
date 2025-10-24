# Importation et traintement des données
require(dplyr)
require(tidyr)
require(readr)
require(stringr)
require(readxl)

# import main data table ####
SPdata <- read_excel(path = "data/carto analytique.xlsx",
                     sheet = "Tableau Maud")


# Taxons ####

# Remplacer les taxons par de une nouvelle taxonomie plus simple
data_taxons <- as.data.frame(read_excel("data/carto analytique.xlsx",
                                        sheet = "Index taxons",) ) %>%
  select(1:3)

# Nouvelle colonne avec les taxons 
SPdata$Taxons_corrected <- change_taxonomy(df = SPdata,
                                           col = "Taxons",
                                           index = data_taxons ,
                                           col_init = "TAXONS TABLEAU",
                                           col_finale = "TAXONS FINAUX") 

# Créer la matrice d'incidence des taxons par projets:
SP_taxons = col2presence(df = SPdata,
                         colonne = "Taxons_corrected",
                         colonnes_a_garder = c("Nom", "Milieux","Taxons_corrected",
                                               "Partenaires", "Type participants",
                                               "Résumé de l'observatoire"),
                         nouveaux_noms = c("Nom_projet", "milieux","taxons",
                                           "partenaires", "type_participants",
                                           "def"),
                         binaire = TRUE,
                         rename_rows = "Nom_projet")
  
# Garder la liste des taxons uniques:
taxons = SP_taxons$objects

# Extraire uniquement le tableau de données:
SP_taxons <- SP_taxons$data


# Milieux ####
SP_milieux = col2presence(df = SPdata,
                          colonne = "Milieux",
                          colonnes_a_garder = c("Nom", "Milieux","Taxons_corrected",
                                                "Partenaires", "Type participants",
                                                "Résumé de l'observatoire"),
                          nouveaux_noms = c("Nom_projet", "milieux","taxons",
                                            "partenaires", "type_participants",
                                            "def"),
                          binaire = TRUE,
                          rename_rows = "Nom_projet")
milieux = SP_milieux$objects
SP_milieux = SP_milieux$data
rownames(SP_milieux) = SP_milieux$Nom_projet

# Acteurs ####

# importer le tableau classifiant les acteurs
data_acteurs <- as.data.frame(read_excel("data/carto analytique.xlsx",
                                         sheet = "Index acteurs") )
types_acteurs <- names(data_acteurs)[-c(1,2)]

# Convertir en 0 et 1: 
data_acteurs[data_acteurs == "x"] <- 1
data_acteurs[is.na(data_acteurs)] <- 0

# Restreindre aux noms simplifiés: 
data_acteurs <- data_acteurs %>%
  select(-'Nom de la structure animatrice') %>%
  unique()

# Extraire type d'acteur unique:
data_acteurs <- pivot_longer(data = data_acteurs,
                             cols = types_acteurs,
                             names_to = "type",
                             values_to = "value") %>%
  filter(value == 1) %>%
  select(-value)

# Table pour résumer les types d'acteurs pour chaque programme
SP_acteurs = as.data.frame(matrix(0,
                                  nrow(SPdata),
                                  length(types_acteurs),
                                  dimnames = list(SPdata$Nom, 
                                                  types_acteurs)))
SP_acteurs$partenaires = SPdata$Partenaires
SP_acteurs$Nom_projet = SPdata$Nom
SP_acteurs$def = SPdata$`Résumé de l'observatoire`

# Caractériser le type de programme: Effort
SP_acteurs$type_participants = SPdata$`Type participants`

# Fill the incidence matrix:
for (i in 1:nrow(data_acteurs)) {
  act = as.character(data_acteurs[i, "Nom_simple"])
  type = as.character(data_acteurs[i, "type"])
  lines = grep(act, SP_acteurs$partenaires)
  if (length(lines)>0) {
    previous_values = SP_acteurs[lines,type]
    SP_acteurs[lines,type] = previous_values + 1
    rm(previous_values)
  }
}

rownames(SP_acteurs) = SP_acteurs$Nom_projet

# Simplifier nom des participants:
types_part = data.frame(long = levels (as.factor(SP_acteurs$type_part)),
                        court  = c("Grand Public",
                                   "Naturalistes bénévoles",
                                   "Professionnels espaces naturels",
                                   "Professionnels espaces verts",
                                   "Professionnels agricole",
                                   "Scolaires")
)


