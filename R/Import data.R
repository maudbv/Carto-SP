# Import data

library(readr)
library(ggplot2)
library(igraph)
library(networkD3)
library(visNetwork)
library(dplyr)
library(stringr)
library(readxl)
library(RColorBrewer)

# read data
SPdata <- read_csv("data/carto analytique.csv")


# Données pour les graph Habitats et themes ####
# 1/ Une représentation centrée sur les sujets d'étude. Croiser la colonne type de programme (inventaire, suivi, signalement, recueil  / on avait parlé d'un code couleur) avec les milieux étudiés (colonne "thèmes"). Et éventuellement avoir une autre représentation type de programme / taxon.


themes = unique(unlist(str_split(SPdata$Thèmes, pattern = " , ")))
habitats = c("Mer & littoral","Terres agricoles","En forêt","En campagne","En montagne","En ville","Eaux intérieures")
questions = c("Espèces envahissantes" ,"Changement climatique","Parcs & jardins", "Espaces protégés","Espèces en danger")

# ajouter les habitats pour 4 projets: 
SPdata[ SPdata$Nom %in% c(
  "Base de données de l'Observatoire Mutualisé de la BIodiversité et de la NAture BOMBINA",
     "Nos Jardins à La Loupe", 
     "AGIIR",
     "Enquête hirondelles et martinets - Cotentin et Bessin"), "Thèmes" ] = "En montagne , En forêt , Mer & littoral , Eaux intérieures , En ville , Parcs & jardins , Terres agricoles , En campagne"

# Table pour résumer les thèmes
SP_themes = as.data.frame(matrix(0,
                                 nrow(SPdata),
                                 length(themes),
                                 dimnames = list(SPdata$Nom, themes)))
SP_themes$themes = SPdata$Thèmes
SP_themes$projet = SPdata$Nom
SP_themes$def = SPdata$`Résumé de l'observatoire`
SP_themes$type_prog = SPdata$`Type de programme`

# Fill the indicence matrix:
for (i in themes) {
  SP_themes[grep(i, SP_themes$themes), i] = 1
}


# Matrice d'incidence pour les habitats: 
SP_habitats = SP_themes %>%
  select(all_of(habitats), projet, def, type_prog)

# Matrice d'incidence pour les habitats: 
SP_questions = SP_themes %>%
  select(all_of(questions), projet, def, type_prog)

# 2/ Une représentation centrée sur les acteurs mobilisés. ####
# Relier chaque programme avec les types d'acteurs impliqués (ceux mentionnés dans les colonnes "nom de la structure animatrice" et "responsable scientifique") que l'on classe en 5 grands types (cf index acteurs : académique, associatif, gestionnaire d'espaces, collectivité, entreprise). Et indiquer par un code couleur le type de participants impliqués dans chaque programme (colonne J)

## importer le tableau classifiant les acteurs ####
acteurs <- as.data.frame(read_excel("data/carto analytique.xlsx", sheet = "Index acteurs") )

##  extraire les types d'acteurs: ####
types_act <- names(acteurs)[2:6]

## rendre les données numériques: ####
acteurs[acteurs == "x"] <- 1
acteurs[is.na(acteurs)] <- 0
acteurs <- acteurs %>%
  mutate(across(all_of(types_act) , as.numeric))


## Compter les types d'acteurs pour chaque projet de SP: ####

# types de structure animatrice
x1 = acteurs[match(SPdata$`Nom de la structure animatrice`,
                   acteurs$`Nom de la structure animatrice`),
             types_act ] 
x1[is.na(x1)] <- 0

# type de structure scientifique
x2 = acteurs[match(SPdata$`Structure responsable scientifique`,
                   acteurs$`Nom de la structure animatrice`),
             types_act ]
x2[is.na(x2)] <- 0

## Additionner les deux: ####
SP_acteurs <- as.data.frame(SPdata %>%
                              select(Nom,`Type participants`, `Résumé de l'observatoire` ) %>%
                              cbind(., x1 + x2))

rownames(SP_acteurs) = SP_acteurs$Nom

# Simplifier nom des participants:
type_part = data.frame(long = levels (as.factor(SP_acteurs$`Type participants`)),
                       court  = c("Grand Public",
                                  "Naturalistes bénévoles",
                                  "Professionnels espaces naturels",
                                  "Professionnels espaces verts",
                                  "Professionnels agricole",
                                  "Scolaires")
)

# 3/ Une carte des régions de France avec un gradient de couleur en fonction du nombre de programme par région + sur chaque région un donut qui représente la part de programmes locaux, départementaux ou régionaux (inclure les outremer). Mais pour cette représentation on avait dit qu'on voyait plutôt avec Simon et/ou Charles.