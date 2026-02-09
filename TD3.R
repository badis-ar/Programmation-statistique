#TD3 
#Exercice 1 - Importer les données

#1 Importer le jeu de données pokemon.xlsx à l’aide du package readxl.
install.packages("readxl") #installe le packages
library(readxl) # import le packages

df = read_excel(path = "pokemon.xlsx",sheet = "pokemon") #crée le df

#2 Combien de lignes, colonnes sont présentes dans ce 
#dataset (utilisez les fonctions adaptées) ?
nrow(df) #nombre de lignes
ncol(df)#nombre de colone

#3 Affichez un résumé des données avec la fonction adaptée.
#On remarque peut-être que les variables qualitatives n'ont pas 
#de statistique. En R, il est important que les variables q
#ualitatives soit de type factor.
summary(df)

#4 On souhaite analyser les variables generation, 
#is_legendary, et type en tant que variables qualitatives. 
#Modifier le type de ces variables pour les transformer en type 
#factor (plus d'infos sur le type factor ici)

df$is_legendary <-as.factor(df$is_legendary)
df$generation <-as.factor(df$generation)
df$type <-as.factor(df$type)

#5 Affichez un nouveau résumé des données avec la fonction adaptée.
summary(df)


#Exercice 2 - Création de colonne

#La fonction ifelse().

#1 Créer une colonne attack_group avec la valeur attack+ 
#si la valeur d'attack est supérieure ou égale à la médiane, 
#sinon attack-. Convertir cette variable en factor puis effectuer 
#un résumé de cette colonne avec la fonction summary().
df$attaque_group = ifelse(df$attack < median(df$attack), yes = "attaque-", no ="attaque+")
df$attaque_group = as.factor(df$attaque_group)
summary(df)

#2 Créer une colonne water_fire avec la valeur yes si le type 
#est water ou fire, sinon renseigner la valeur no. Convertir 
#cette variable en factor puis effectuer un résumé de cette 
#colonne avec la fonction summary().

df$water_fire = ifelse(df$type %in% c("water","fire"), "yes","no")
df$water_fire = as.factor(df$water_fire)
summary(df)
#3 Créer une colonne best avec la valeur yes si la valeur d'attack 
#fait parti du troisième quartile et si la valeur de defense fait 
#parti du troisième quartile et si la valeur de speed fait parti 
#du troisième quartile, sinon renseigner la valeur no. Convertir 
#cette variable en factor puis effectuer un résumé de cette colonne 
#avec la fonction summary().
q3attak = quantile(df$attack, probs = 0.75)
q3def = quantile(df$defense, probs = 0.75)
q3speed = quantile(df$speed, probs = 0.75)
df$best = ifelse(df$attack > q3attak & df$defense > q3def & df$speed > q3speed, yes="yes", no="no")
df$best = as.factor(df$best)
summary(df)

#La fonction is.na().


#1 Filtrer les données dans un objet nommé requete avec les 
#pokemons ayant des valeurs manquantes sur la colonne weight_kg.
requete = subset(df, is.na(weight_kg)) # pour crée un tableau (subset)
View(requete)
#2 Créer des nouvelles variables nommées weight_kgNa et height_mNA 
#avec les mêmes valeurs pour les valeurs déjà renseignées mais en 
#remplaçant les valeurs manquantes NA par leur valeurs médianne.
df$weightna = ifelse(is.na(df$weight_kg), median(df$weight_kg), df$weight_kg)
df$heightna = ifelse(is.na(df$height_m), median(df$height_m), df$height_m)


#La fonctions cut().

#1 Créer une nouvelle variable nommée weight_group en regroupant 
#en 3 tranches avec les labels léger / moyen / lourd.

df$wg = cut(df$weight_kg,breaks = 3,labels = c("Léger","Moyen","Lourd"), na.rm = TRUE)
#2 Créer une nouvelle variable nommée height_m_group en 
#regroupant en 4 tranches telles que : ]0,1] / ]1,2] / ]2,3] / ]3,max]
df$hg= cut(df$height_m, breaks = c(0,1,2,3,max(df$height_m)),na.rm  =TRUE)

#3Créer une nouvelle variable nommée defense_group en regroupant 
#en 5 tranches avec les min, max et quartiles telle que : 
#[min,Q1] / (Q1,Q2] / (Q2,Q3] / (Q3,max]. 
#Calculer un résumé de la nouvelle colonne.

df$defgrp = cut(df$defense, breaks = c(min(df$defense),quantile(df$defense, probs = 0.25),quantile(df$defense, probs =0.5), quantile(df$defense, probs = 0.75), max(df$defense)), na.rm = TRUE)
summary(df$defgrp)

#correction
df$defense_group = cut(df$defense,
                            breaks = quantile(df$defense,
                                              na.rm = TRUE),
                            include.lowest = TRUE)
summary(df$defense_group)



#Exercice 3 - Agregation

#La fonction aggregate().
#1 Calculer la moyenne d'attack par type.
aggregate(x = attack ~ type, 
          data = df,
          FUN = function(x) mean(x))

#2 Calculer la mediane d'attack par generation et type.
aggregate(x = attack ~ generation + type, 
          data = df,
          FUN = function(x) median(x))
# 3Calculer l'effectif par type.
aggregate(x = pokedex_number~ type, 
          data = df,
          FUN = function(x) length(x))
#4 Calculer la moyenne et la mediane de la statistique 
#speed pour chaque generation et type. Afficher également les 
#effectifs de chaque paire.
