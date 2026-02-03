#ARIOUA Badis
# T2
#03 02 2026 

#Exercice 1 - Importer des données
#Exercice sur les Fonctions en R
#Les fonctions getwd(),setwd() et read.csv().

#1 Télécharger les jeux de données (bodies_karts.csv, tires.csv, gliders.csv et drivers.csv) disponible ici 
#puis les mettre dans un même dossier appelé dataset.
#2 Afficher le répertoire courant par 
#défaut de votre session RStudio avec la fonction getwd().
getwd()
#3 Changer le répertoire courant par défaut de votre session RStudio avec 
#la fonction setwd() pour pointer sur le dossier avec vos datasets. 
#Puis vérifier le bon changement du répertoire avec la fonction getwd().
setwd("D:/R/TD2/Données") 
getwd()
#L'intérêt de changer de répertoire est de ne pas être obliger 
#de renseigner tout le chemin d'un fichier à importer. Uniquement 
#le nom du fichier est ainsi suffisant. Importer les datasets avec 
#la fonction read.csv().
bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")
#5 Pour chaque dataset, afficher la dimension (ligne/colonne) 
#du dataframe.
dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)

# Exercice 2 - Statistique
# Exercice sur les Fonctions en R
#Les fonctions cor(), corrplot(), install.packages() et plot().

#1 Pour chaque dataset, effectuer un résumé des données avec la fonction adaptée.
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)
#2 Représenter graphiquement dans un nuage de points 
#le lien entre les statistiques des drivers sur Weight et 
#Acceleration. Commenter le lien entre ces deux variables ? 
#Pourquoi il n'y a pas autant de points que de drivers ?
plot(x = drivers$Weight, y = drivers$Acceleration, main = "Drivers : Weight / Acceleration")
#Les variables sont corélé négativement

# 3 Calculer le coefficient de corrélation de 
#cette relation avec la fonction cor().
correlation_drivers <- cor(x = drivers$Weight, y = drivers$Acceleration)
#4 Vérifier ce résultat en calculant vous même 
#le coéfficient de corrélation
cov_WA= cov( x = drivers$Weight, y = drivers$Acceleration)
sd_wa = (sd(drivers$Weight)*sd(drivers$Acceleration))
r= cov_WA / sd_wa
#r = -0.9527271
#5 Calculer le coefficient de détermination de cette même relation. = cor^2 
cor_carre = correlation_drivers^2
#6 Construire la matrice des corrélations des variables 
#quantitatives de statistiques des drivers avec la fonction 
#cor(). Afficher cette matrice dans une vue et arrondisser 
#les valeurs avec deux décimales uniquements. Qu'observez vous ?
matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#Toutes les variables semblent fortement corrélées entre elles.
#7 Pour mieux visualiser ces corrélations, 
#nous allons utiliser un package qui ne fait pas parti des 
#packages par défaut. Installer le package corrplot avec la 
#fonction install.packages()
install.packages("corrplot")

#8 Construire une Corrélogramme avec la fonction corrplot()
library(corrplot)
corrplot(matriceCor, method="circle")

#9 Construire une Corrélogramme pour 
#les 3 autres datasets. Pour chaque dataset, 
#quelle est la relation avec le lien le plus fort ? 
#le lien le moins fort ?
matriceCor_glider = round(cor(gliders[ , - 1]),1)
matriceCor_title = round(cor(tires[ , - 1]),1)
matriceCor_bodie = round(cor(bodies_karts[ , - 1]),1)


corrplot(matriceCor_glider, method="circle")
corrplot(matriceCor_title, method="circle")
corrplot(matriceCor_bodie, method="circle")

#revoir chez sois 


#Exercice 3 - Manipulation de data frame 
#La fonction order(). 
#1Créer un object resultat avec uniquement 
#le nom du Driver et son Weight.
resultat = drivers[, c("Driver", "Weight")]
View(resultat)
#2 Créer un object resultat avec uniquement 
#le nom du Driver et son Acceleration sur les 10 premières lignes.
resultat2 = drivers[1:10, c("Driver", "Acceleration")]
View(resultat2)
#3 Créer un object resultatsans les colonnes 5, 7 et 9.
resultat3 = drivers[-c(5,7,9), ]
View(resultat3)
#4 Créer un object resultatsans les colonnes Weight et Acceleration.
#Impossible ?
#5 Créer un object resultat avec uniquement les colonnes 
#Driver, Acceleration et Weight dans cet ordre. 
#Que remarquez-vous ?
resultat5 = drivers[ , c("Driver","Acceleration","Weight") ]
View(resultat5)
#6 Créer un object resultat avec uniquement 
#les Driver 3 , 12 et 32 dans cet ordre.
resultat6 = drivers[c(3,12,32), ]
View(resultat6)
#7 Créer un object resultat avec uniquement 
#les Driver 32 , 3 , 12 dans cet ordre. Que remarquez-vous ?
resultat7 = drivers[c(32,3,12), ]
View(resultat7)
#Le trie ce fait bien 
#8 Créer un object resultat avec uniquement 
#les colonnes Driver et Weight en triant les conducteurs 
#du plus léger au plus lourd avec la fonction order().
resultat8 = drivers[order(drivers$Weight), c("Driver", "Weight")]
View(resultat8)

#9 Créer un object resultat avec uniquement les colonnes 
#Driver et Acceleration en triant les conducteurs du plus 
#rapide au moins rapide.
resultat9 = drivers[order(drivers$Acceleration), c("Driver", "Acceleration")]
View(resultat9)

#10 Créer un object resultat avec 
#les colonnes Driver, Weight et Acceleration en triant 
#les conducteurs du plus rapide au moins rapide puis du plus 
#léger au plus lourd.

resulat10 = drivers[order(drivers$Acceleration,drivers$Weight, decreasing = c(TRUE,FALSE)), c("Driver", "Acceleration", "Weight")]
View(resulat10)

