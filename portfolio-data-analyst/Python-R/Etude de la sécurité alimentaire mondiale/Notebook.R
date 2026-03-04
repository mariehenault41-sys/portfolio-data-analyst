---
  title: "Projet 4 : Réalisation d’une étude sur la faim dans le monde"
author: "Marie HENAULT"
date: "`r Sys.Date()`"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

#### INTRODUCTION

# L’objectif principal de ce projet est d’analyser la prévalence de la sous-nutrition dans différents pays, en utilisant des bases de données qui nous permettent de comprendre comment l’accès à l’alimentation et la disponibilité des ressources influencent cette problématique mondiale.
# Nous disposons de quatre jeux de données :
  # - aide_alimentaire.csv, qui contient la quantité d’aide humanitaire en denrées alimentaires reçue par différents pays.
  # - dispo_alimentaire.csv, qui nous informe sur les ressources alimentaires disponibles par habitant, telles que les calories, protéines ou matières grasses.
  # - population.csv, un jeu de données simple, mais essentiel, qui nous donne le nombre d’habitants pour chaque pays et chaque année.
  # - Enfin, nous avons les données de sous_nutrition.csv, qui indiquent le nombre de personnes souffrant d’un accès insuffisant à la nourriture dans chaque pays.

#///IMPORTANT\\\#
# Il est important de souligner que le code créé ici, n’est pas fait pour être exécuté d’un seul bloc, puisqu’il n’a pas été rédigé en une seule fois. Chaque analyse ayant été faite sur une nouvelle session, je remettrais donc ici pour chaque analyse, l’entièreté du code nécessaire à son exécution (harmonisation comprise).

#### SECTION 1 : Harmonisation des données

# Chargement des fichiers csv
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

# Harmonisation des données : 
  # -	Changements des noms de colonnes lorsque nécessaire
  # -	Conversion de toutes les valeurs en tonnes en kg
  # -	Conversion de toutes les valeurs en milliers ou millions de population en nombre d’effectif total
  # -	Valeurs présentes dans le fichier sous_nutrition.csv converties en valeurs numériques
  # -	Remplacement de toutes les valeurs NA par des 0
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

# Vérification 
head(aide_alimentaire)
head(dispo_alimenatire)
head(population)
head(sous_nutrition)

# Chargement des librairies
library(tidyr)
library(dplyr)
library(ggplot2)

#### SECTION 2 : Proportion de personnes en état de sous-nutrition en 2017

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Récupération des données : filtrage des données de sous-nutrition et de population pour l'année 2017.
# Calcul du nombre total : agrégation des données pour obtenir le nombre total de personnes en sous-nutrition et la population mondiale.
# Calcul du pourcentage : (nombre de personnes en sous-nutrition / population totale) * 100, ce qui nous donne la proportion de sous-nutrition à l’échelle mondiale.
sous_nutrition_2016_2018 <- subset(sous_nutrition, Année == "2016-2018")
sous_nutrition_totale <- sum(sous_nutrition_2016_2018$sous_nutrition, na.rm = TRUE)
population_2017 <- subset(population, Année == 2017)
population_totale <- sum(population_2017$Population, na.rm = TRUE)
proportion_sous_nutrition <- (sous_nutrition_totale / population_totale) * 100
cat("Nombre total de personnes en état de sous-nutrition en 2017 :", sous_nutrition_totale, "personnes\n")
cat("Proportion de personnes en état de sous-nutrition en 2017 :", round(proportion_sous_nutrition, 2), "%\n")

#### SECTION 3 : Nombre théorique de personnes qui pourraient être nourries en 2017

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Récupération des données sur la disponibilité alimentaire et la population mondiale pour 2017.
# Choix de plusieurs hypothèses de besoins caloriques : 2000, 2250 et 2500 Kcal par personne et par jour.
# Multiplication de la disponibilité alimentaire en Kcal/personne/jour par la population de chaque zone, pour obtenir le total mondial des calories disponibles par jour.
# Division de ce total par les besoins énergétiques moyens, pour calculer le nombre théorique de personnes pouvant être nourries.
dispo_alimentaire <- dispo_alimentaire[, c("zone", "dispo_kcal_p_j")]
population_2017 <- subset(population, Année == 2017)
dispo_population <- merge(dispo_alimentaire, population_2017, by.x = "zone", by.y = "Zone")
dispo_population$calories_totales_zone <- dispo_population$dispo_kcal_p_j * dispo_population$Population
calories_totales_mondiales <- sum(dispo_population$calories_totales_zone, na.rm = TRUE)
besoins_caloriques <- c(2000, 2250, 2500)
personnes_nourries <- sapply(besoins_caloriques, function(besoin) {
  calories_totales_mondiales / besoin
})
population_totale <- sum(population_2017$Population, na.rm = TRUE)
pourcentage_couverture <- (personnes_nourries / population_totale) * 100
cat("Calories totales disponibles par jour dans le monde :", calories_totales_mondiales, "Kcal\n")
cat("Population mondiale en 2017 :", population_totale, "personnes\n")
cat("Nombre théorique de personnes pouvant être nourries :\n")
for (i in 1:length(besoins_caloriques)) {
  cat("-", besoins_caloriques[i], "Kcal/jour :", round(personnes_nourries[i], 0), "personnes (",
      round(pourcentage_couverture[i], 2), "% de la population mondiale)\n")
}

#### SECTION 4 : Nombre théorique de personnes qui pourraient être nourries uniquement avec les végétaux en 2017

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Filtrage des données pour n’avoir que les disponibilités alimentaires d’origine végétale.
# Même méthode de calcul que dans l’analyse précédente, avec 2000, 2250 et 2500 kcal par personne et par jour
dispo_alimentaire_vegetale <- subset(dispo_alimentaire, origine == "vegetale")
dispo_alimentaire_vegetale <- dispo_alimentaire_vegetale[, c("zone", "dispo_kcal_p_j")]
population_2017 <- subset(population, Année == 2017)
dispo_population_vegetale <- merge(dispo_alimentaire_vegetale, population_2017, by.x = "zone", by.y = "Zone")
dispo_population_vegetale$calories_vegetales_zone <- dispo_population_vegetale$dispo_kcal_p_j * dispo_population_vegetale$Population
calories_vegetales_totales_mondiales <- sum(dispo_population_vegetale$calories_vegetales_zone, na.rm = TRUE)
besoins_caloriques <- c(2000, 2250, 2500)
personnes_nourries_vegetales <- sapply(besoins_caloriques, function(besoin) {
  calories_vegetales_totales_mondiales / besoin
})
population_totale <- sum(population_2017$Population, na.rm = TRUE)
pourcentage_couverture_vegetale <- (personnes_nourries_vegetales / population_totale) * 100
cat("Calories végétales totales disponibles par jour dans le monde :", calories_vegetales_totales_mondiales, "Kcal\n")
cat("Population mondiale en 2017 :", population_totale, "personnes\n")
cat("Nombre théorique de personnes pouvant être nourries uniquement avec des végétaux :\n")
for (i in 1:length(besoins_caloriques)) {
  cat("-", besoins_caloriques[i], "Kcal/jour :", round(personnes_nourries_vegetales[i], 0), "personnes (",
      round(pourcentage_couverture_vegetale[i], 2), "% de la population mondiale)\n")
}

#### SECTION 5 : Répartition de la disponibilité alimentaire mondiale

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Calcul des proportions des composantes des sorties (semences, pertes, nourriture, alimentation animale, traitement, autres utilisations).
# Résultats globaux présentés en pourcentages. 
pop = population[population['Année'] == 2017, c("Zone", "Population")]
colnames(pop)[colnames(pop) == "Zone"] <- "zone"
dispo_alimentaire = merge(dispo_alimentaire, pop, by='zone')
total_semences <- sum(dispo_alimentaire$semences, na.rm = TRUE)
total_pertes <- sum(dispo_alimentaire$pertes, na.rm = TRUE)
total_nourriture <- sum(dispo_alimentaire$nourriture, na.rm = TRUE)
total_aliments_animaux <- sum(dispo_alimentaire$aliments_animaux, na.rm = TRUE)
total_traitement <- sum(dispo_alimentaire$traitement, na.rm = TRUE)
total_autres_utilisations <- sum(dispo_alimentaire$autres_utilisations, na.rm = TRUE)
total_sorties <- sum(dispo_alimentaire$ dispo_interieure, na.rm = TRUE)
proportions <- c(
  Semences = (total_semences / total_sorties) * 100,
  Pertes = (total_pertes / total_sorties) * 100,
  Nourriture = (total_nourriture / total_sorties) * 100,
  "Aliments pour animaux" = (total_aliments_animaux / total_sorties) * 100,
  Traitement = (total_traitement / total_sorties) * 100,
  "Autres utilisations" = (total_autres_utilisations / total_sorties) * 100
)
cat("Proportions des composantes des sorties (%):\n")
print(round(proportions, 2))

# Visualisation avec un camembert
pie(
  proportions,
  labels = paste(names(proportions), "\n", round(proportions, 2), "%"),
  col = rainbow(length(proportions)),
  main = "Répartition des composantes des sorties"
)

#### SECTION 6 : Répartition de l’utilisation des céréales entre alimentation humaine et animale

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Les céréales analysées incluent : blé, riz, maïs, orge, seigle, avoine, millet, sorgho, et autres céréales.
# Extraction des données sur l’utilisation des céréales pour la nourriture humaine et animale.
cereales <- c("Blé", "Riz (Eq Blanchi)", "Orge", "Maïs", "Seigle", "Avoine", "Millet", "Sorgho", "Céréales, Autres")
dispo_cereales <- dispo_alimentaire[dispo_alimentaire$produit %in% cereales, ]
totaux_cereales <- dispo_cereales %>%
  group_by(produit) %>%
  summarise(
    total_nourriture = sum(nourriture, na.rm = TRUE),
    total_aliments_animaux = sum(aliments_animaux, na.rm = TRUE)
  )
totaux_cereales <- totaux_cereales %>%
  mutate(
    total_utilisation = total_nourriture + total_aliments_animaux,
    part_humaine = (total_nourriture / total_utilisation) * 100,
    part_animale = (total_aliments_animaux / total_utilisation) * 100
  )
print(totaux_cereales)
total_dispo_interieure <- sum(dispo_cereales$dispo_interieure, na.rm = TRUE)
total_global_nourriture <- sum(totaux_cereales$total_nourriture)
total_global_animaux <- sum(totaux_cereales$total_aliments_animaux)

part_humaine_global <- (total_global_nourriture / total_dispo_interieure) * 100
part_animale_global <- (total_global_animaux / total_dispo_interieure) * 100

cat("Proportions globales selon la correction (%):\n")
cat("Humaine :", round(part_humaine_global, 2), "%\n")
cat("Animale :", round(part_animale_global, 2), "%\n")

# Visualisation avec un camembert pour les résultats globaux
df_camembert <- data.frame(
  catégorie = c("Humaine", "Animale"),
  proportion = c(part_humaine_global, part_animale_global)
)
ggplot(df_camembert, aes(x = "", y = proportion, fill = catégorie)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +  
  theme_void() +  
  labs(title = "Proportions Globales de l'Utilisation des Céréales") +
  scale_fill_manual(values = c("skyblue", "orange")) +  
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5)  

# Calcul des proportions pour chaque type de céréale
totaux_cereales_long <- totaux_cereales %>%
  pivot_longer(cols = c(part_humaine, part_animale), names_to = "categorie", values_to = "proportion")

# Visualisation avec un histogramme pour le détail par céréales
ggplot(totaux_cereales_long, aes(x = produit, y = proportion, fill = categorie)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Proportions d'Alimentation Humaine et Animale par Produit",
    x = "Produit",
    y = "Proportion (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_fill_manual(values = c("skyblue", "orange")) +  
  theme(legend.title = element_blank())  

#### SECTION 7 : Les 10 pays les plus touchés par la sous-nutrition en 2017

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Extraction des données de sous-nutrition (2016-2018) et de population (2017) par pays.
# Calcul des proportions : personnes en état de sous-nutrition par rapport à la population totale.
# Classement des pays en fonction de leur ratio, avec sélection des 10 premiers. 
population_2017 <- population[population$Année == 2017, ]
sous_nutrition_2016_2018 <- sous_nutrition[sous_nutrition$Année == "2016-2018", ]
population_2017 <- aggregate(Population ~ Zone, data = population_2017, sum)
sous_nutrition_2016_2018 <- aggregate(sous_nutrition ~ Zone, data = sous_nutrition_2016_2018, sum)
data_combined <- merge(population_2017, sous_nutrition_2016_2018, by = "Zone")
data_combined$ratio_sous_nutrition <- data_combined$sous_nutrition / data_combined$Population
top_10 <- data_combined[order(-data_combined$ratio_sous_nutrition), ][1:10, ]
print(top_10)
top_10$ratio_sous_nutrition <- top_10$ratio_sous_nutrition * 100

# Visualisation sous forme d’histogramme
ggplot(top_10, aes(x = reorder(Zone, -ratio_sous_nutrition), y = ratio_sous_nutrition)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(
    title = "Top 10 des zones avec la plus forte proportion de sous-nutrition (2017)",
    x = "Zone",
    y = "Proportion de sous-nutrition (%)"
  ) +
  theme_minimal()

#### SECTION 8 : Les pays ayant reçu le plus d'aide alimentaire entre 2013 et 2016

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Extraction des données sur l'aide alimentaire pour les années 2013-2016.
# Somme des quantités d'aide reçue (en kg) par pays.
# Classement des 10 principaux bénéficiaires en ordre décroissant.
# Conversion des données en milliers de tonnes pour une meilleure lisibilité. 
aide_alimentaire_filtered <- aide_alimentaire[aide_alimentaire$Année >= 2013 & aide_alimentaire$Année <= 2016, ]
aide_par_zone <- aggregate(Aide_kg ~ Pays.bénéficiaire, data = aide_alimentaire_filtered, sum)
top_10_aide <- aide_par_zone[order(-aide_par_zone$Aide_kg), ][1:10, ]
top_10_aide_table <- top_10_aide
top_10_aide_table$Aide_kg <- format(top_10_aide_table$Aide_kg, big.mark = ",") 
print(top_10_aide_table)

# Visualisation par un histogramme
ggplot(top_10_aide, aes(x = reorder(Pays.bénéficiaire, -Aide_kg), y = Aide_kg / 1000)) + 
  geom_bar(stat = "identity", fill = "seagreen") +
  coord_flip() + 
  labs(
    title = "Top 10 des pays ayant reçu le plus d’aide alimentaire (2013-2016)",
    x = "Pays bénéficiaire",
    y = "Aide alimentaire reçue (en milliers de tonnes)"
  ) +
  theme_minimal()

#### SECTION 9 : Analyse temporelle de l'aide alimentaire (2013-2016)

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Sélection des 5 pays ayant reçu le plus d’aide alimentaire entre 2013 et 2016.
# Calcul de la somme annuelle des aides reçues (en kg) pour chaque pays.
aide_alimentaire_filtered <- aide_alimentaire[aide_alimentaire$Année >= 2013 & aide_alimentaire$Année <= 2016, ]
top_5_pays <- c("République arabe syrienne", "Éthiopie", "Yémen", "Soudan du Sud", "Soudan")
aide_top_5 <- aide_alimentaire_filtered[aide_alimentaire_filtered$Pays.bénéficiaire %in% top_5_pays, ]
evolution_aide <- aggregate(Aide_kg ~ Pays.bénéficiaire + Année, data = aide_top_5, sum)
summary(evolution_aide)
table(evolution_aide$Pays.bénéficiaire)  
evolution_aide_clean <- na.omit(evolution_aide)
unique(evolution_aide_clean$Pays.bénéficiaire)
tableau_aide <- aggregate(Aide_kg ~ Pays.bénéficiaire + Année, data = aide_top_5, sum)
print(tableau_aide)

# Visualisation par un graphique en courbe
ggplot(evolution_aide_clean, aes(x = Année, y = Aide_kg / 1e6, color = Pays.bénéficiaire, group = Pays.bénéficiaire)) +
  geom_line(linewidth = 1.2) +  
  geom_point(size = 3) +
  labs(
    title = "Évolution de l’aide alimentaire pour les 5 pays principaux (2013-2016)",
    x = "Année",
    y = "Aide alimentaire (millions de kg)",
    color = "Pays bénéficiaire"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### SECTION 10 : Analyse des pays à faible disponibilité alimentaire par habitant (2017)

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Utilisation de la colonne dispo_kg_p_j pour évaluer la disponibilité alimentaire moyenne par habitant en kg/jour.
# Agrégation et tri des données par pays pour identifier ceux avec la plus faible disponibilité.
# Extraction des 10 premiers pays avec la plus faible disponibilité alimentaire par habitant
dispo_pays_total <- aggregate(dispo_alimentaire$dispo_kcal_p_j, by = list(dispo_alimentaire$zone), sum)
colnames(dispo_pays_total) <- c("zone", "dispo_kcal_total")
dispo_pays_total <- dispo_pays_total[order(dispo_pays_total$dispo_kcal_total, decreasing = FALSE), ]
top_10_dispo <- head(dispo_pays_total, 10)
print(top_10_dispo)

# Visualisation par un histogramme
ggplot(top_10_dispo, aes(x = reorder(zone, -dispo_kcal_total), y = dispo_kcal_total)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Top 10 des pays par disponibilité alimentaire (Kcal/personne/jour)",
    x = "Pays",
    y = "Disponibilité alimentaire totale (Kcal/personne/jour)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### SECTION 11 : Les 10 pays avec la plus forte disponibilité alimentaire par habitant en 2017

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Utilisation de la colonne dispo_kcal_p_j pour évaluer la disponibilité alimentaire moyenne par habitant en Kcal/jour.
# Agrégation et tri des données par pays pour identifier ceux avec la plus grande disponibilité.
# Extraction des 10 premiers pays avec la plus forte disponibilité alimentaire par habitant.
dispo_pays_total <- aggregate(dispo_alimentaire$dispo_kcal_p_j, by = list(dispo_alimentaire$zone), sum)
colnames(dispo_pays_total) <- c("zone", "dispo_kcal_total")
dispo_pays_total <- dispo_pays_total[order(dispo_pays_total$dispo_kcal_total, decreasing = TRUE), ]
top_10_dispo <- head(dispo_pays_total, 10)
print(top_10_dispo)

# Visualisation par un histogramme
ggplot(top_10_dispo, aes(x = reorder(zone, -dispo_kcal_total), y = dispo_kcal_total)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Top 10 des pays par disponibilité alimentaire (Kcal/personne/jour)",
    x = "Pays",
    y = "Disponibilité alimentaire totale (Kcal/personne/jour)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### SECTION 12 : Analyse de la sous-nutrition et des flux de manioc en Thaïlande

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Part de sous-nutrition (2013-2018) :
  # - Filtrage des données pour la Thaïlande dans les fichiers sous_nutrition et population.
  # - Calcul des proportions de sous-nutrition (population affectée / population totale).
  # - Moyenne sur 6 ans.
sous_nutrition_thailande <- sous_nutrition[sous_nutrition$Zone == "Thaïlande", ]
sous_nutrition_thailande$Annee <- c(2013, 2014, 2015, 2016, 2017, 2018)
population_thailande <- population[population$Zone == "Thaïlande", ]
population_thailande <- population_thailande[population_thailande$Année %in% c(2013:2018), ]
data_combinee <- merge(sous_nutrition_thailande, population_thailande, by.x = "Annee", by.y = "Année")
data_combinee$part_sous_nutrition <- (data_combinee$sous_nutrition / data_combinee$Population) * 100
print(data_combinee[, c("Annee", "part_sous_nutrition")])
moyenne_part_sous_nutrition <- mean(data_combinee$part_sous_nutrition)
print(paste("Moyenne de la part de sous-nutrition sur 6 ans : ", moyenne_part_sous_nutrition))

# Exportations et production de manioc (2017) :
  # - Filtrage des données pour la Thaïlande dans le fichier dispo_alimentaire.
  # - Extraction des colonnes Exportations_quantite et Production.
  # - Calcul de la proportion d’exportations par rapport à la production.
manioc_thailande <- dispo_alimentaire[dispo_alimentaire$zone == "Thaïlande" & dispo_alimentaire$produit == "Manioc", ]
exportations <- manioc_thailande$exportations_quantite
production <- manioc_thailande$production
data_export_production <- data.frame(Exportations_kg = exportations, Production_kg = production)
print(data_export_production)
data_export_production$Exportation_en_pourcentage <- (exportations / production) * 100
print(data_export_production[, c("Exportation_en_pourcentage")])

# Disponibilité alimentaire (kcal/personne/jour) :
  # Filtrage des données pour la Thaïlande dans dispo_alimentaire.
  # Extraction de la colonne dispo_kcal_p_j.
  # Agrégation pour obtenir la disponibilité moyenne totale par habitant.
dispo_thailande <- dispo_alimentaire[dispo_alimentaire$zone == "Thaïlande", ]
dispo_kcal_totale <- sum(dispo_thailande$dispo_kcal_p_j)
print(dispo_kcal_totale)

#### SECTION 13 : Analyse complémentaire 

# Chargement et harmonisation des données
population = read.csv("population.csv", fileEncoding = "UTF-8")
dispo_alimentaire = read.csv("dispo_alimentaire.csv", fileEncoding = "UTF-8")
aide_alimentaire = read.csv("aide_alimentaire.csv", fileEncoding = "UTF-8")
sous_nutrition = read.csv("sous_nutrition.csv", fileEncoding = "UTF-8")

colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Pays bénéficiaire"] <- "Zone" 
colnames(aide_alimentaire)[colnames(aide_alimentaire) == "Valeur"] <- "Aide_kg"
aide_alimentaire$Aide_kg <- aide_alimentaire$Aide_kg * 1000
colnames(dispo_alimentaire) <- c( "zone", "produit", "origine", "aliments_animaux", "autres_utilisations", "dispo_kcal_p_j", "dispo_kg_p_j", "dispo_mg_p_j", "dispo_prot_g_p_j", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" )
dispo_alimentaire[is.na(dispo_alimentaire)] <- 0
cols_tonnes <- c( "aliments_animaux", "autres_utilisations", "dispo_interieure", "exportations_quantite", "importations_quantite", "nourriture", "pertes", "production", "semences", "traitement", "variation_stock" ) 
dispo_alimentaire[cols_tonnes] <- dispo_alimentaire[cols_tonnes] * 1000
population$Valeur <- population$Valeur * 1000
colnames(population)[which(colnames(population) == "Valeur")] <- "Population"
sous_nutrition$Valeur <- as.numeric(sous_nutrition$Valeur)
sous_nutrition[is.na(sous_nutrition)] <- 0
colnames(sous_nutrition)[colnames(sous_nutrition) == "Valeur"] <- "sous_nutrition"
sous_nutrition$sous_nutrition <- sous_nutrition$sous_nutrition * 1000000

library(tidyr)
library(dplyr)
library(ggplot2)

# Calcul de la disponibilité alimentaire totale dans le pays, puis soustraction de la quantité d’aide alimentaire reçue pour obtenir une estimation de la disponibilité alimentaire sans aide.
# Calcul de la disponibilité alimentaire par habitant dans ce scénario hypothétique.
# En comparant cette disponibilité avec celle incluant l’aide, on détermine un facteur de réduction, permettant d’évaluer l’impact de l’absence d’aide sur la situation alimentaire locale.
# Enfin, on applique ce facteur pour estimer une nouvelle proportion de sous-nutrition, dans ce scénario sans aide.
aide_haïti <- aide_alimentaire %>%
  filter(Pays.bénéficiaire == "Haïti") %>%
  summarise(Aide_totale = sum(Aide_kg, na.rm = TRUE)) %>%
  pull(Aide_totale)
dispo_sans_aide_h <- dispo_alimentaire %>%
  filter(zone == "Haïti") %>%
  summarise(Dispo_totale = sum(dispo_interieure, na.rm = TRUE)) %>%
  pull(Dispo_totale) - aide_haïti
pop_haïti <- population %>%
  filter(Zone == "Haïti", Année >= 2013 & Année <= 2016) %>%
  summarise(Population_moyenne = mean(Population, na.rm = TRUE)) %>%
  pull(Population_moyenne)
dispo_par_habitant_sans_aide_h <- dispo_sans_aide_h / pop_haïti
facteur_reduction_h <- dispo_par_habitant_sans_aide_h / (dispo_sans_aide_h + aide_haïti / pop_haïti)
sous_nutrition_scenario_h <- sous_nutrition %>%
  filter(Zone == "Haïti") %>%
  mutate(
    sous_nutrition_sans_aide_h = sous_nutrition * (1 + (1 - facteur_reduction_h))
  )
sous_nutrition_scenario_h <- sous_nutrition_scenario_h %>%
  mutate(
    Année = case_when(
      Année == "2012-2014" ~ "2013",
      Année == "2013-2015" ~ "2014",
      Année == "2014-2016" ~ "2015",
      Année == "2015-2017" ~ "2016",
      Année == "2016-2018" ~ "2017",
      Année == "2017-2019" ~ "2018"
    )
  )
head (sous_nutrition_scenario_h)
sous_nutrition_scenario_h <- sous_nutrition_scenario_h %>%
  mutate(Année = as.integer(Année))

sous_nutrition_scenario_h <- sous_nutrition_scenario_h %>%
  inner_join(population %>% filter(Année >= 2013 & Année <= 2018), by = c("Zone" = "Zone", "Année" = "Année"))
sous_nutrition_scenario_h <- sous_nutrition_scenario_h %>%
  mutate(
    sous_nutrition_pct = (sous_nutrition / Population) * 100,
    sous_nutrition_sans_aide_pct = (sous_nutrition_sans_aide_h / Population) * 100
  )

# Visualisation sous forme de graphique en courbe
ggplot(sous_nutrition_scenario_h, aes(x = Année)) +
  geom_line(aes(y = sous_nutrition_pct, color = "Avec aide"), linewidth = 1.2) +
  geom_line(aes(y = sous_nutrition_sans_aide_pct, color = "Sans aide"), linewidth = 1.2, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2013, 2018, 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(
    title = "Impact de l'aide alimentaire sur la sous-nutrition à Haïti (en % de la population)",
    x = "Année",
    y = "Sous-nutrition (% de la population)",
    color = "Scénario"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

