#ARIOUA Badis 
#TP 2
#06 02 2026 


#TP2 - Rappel et cas pratique

#Exercice 1 - Importer les données 

#Importer le jeu de données fao.csv avec la fonction read.csv() 
#uniquement dans un objet appelé df. Vérifier le type des colonnes 
#pour vérifier que les colonnes aient le bon type.

FAO =read.csv("fao.csv", sep=";", dec=",", header = TRUE)

#2 Combien de pays sont présents dans ce dataset ?
nrow(FAO)
#3 Affichez un résumé des données avec la fonction adaptée. 
#Il semble que ce jeu de données présente quelques valeurs manquantes.
summary(FAO)

#Exercice 2 - Statistiques descriptives

#1 Quelle est la disponibilité alimentaire moyenne mondiale 
#en Kcal/personne/jour ?

mean(FAO$Dispo_alim, na.rm=TRUE)
#2Quel est le nombre d’habitant dans le monde ?
sum(FAO$Population, na.rm =  TRUE)

#3 Quel est l’écart-type du volume des exportations de viande ? 
#Et des importations de viande ?
sd(FAO$Import_viande, na.rm = TRUE)
sd(FAO$Export_viande, na.rm = TRUE)

#4 Quelle est la médiane du volume de production de viande ?
median(FAO$Prod_viande, na.rm = TRUE)
#5 Calculez les quartiles du nombre de Kcal de disponibilité 
#alimentaire.
quantile(FAO$Dispo_alim)
#6 Calculez les centiles du volume d’importation de viande.
quantile(FAO$Import_viande, seq(0,1,0.01))


#Exercice 3 - Tris et filtres