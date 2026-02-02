#ARIOUA Badis
#Lundi 2 février 2026


#Exercice 1 - Utilisation d'un dataframe existant un dataframe
#Exercice sur les Fonctions en R
#1 Afficher le jeu de données iris puis afficher la classe de l'objet iris. 
iris
class(iris)
#2 Afficher le jeu de données iris dans une vue avec la fonction View()
View(iris)
#3 Afficher le nombre de lignes avec la fonction nrow()
nrow(iris)
#4 Afficher le nombre de colonne avec la fonction ncol()
ncol(iris)
#5 Afficher le nom des colonnes avec la fonction colnames()
colnames(iris)
# 6 Afficher un résumé du dataframe avec la fonction summary()
summary(iris)
# 7 Afficher uniquement les colonnes Sepal.Length et Species.
iris[ , c("Sepal.Length","Species")]
#8 Afficher uniquement la ligne 100,103 et 105.
iris[ c(100,103,105),]
#9 Afficher uniquement la ligne 50 à 100
iris[ 50:100,]
#10 Calculer la moyenne de la variable Sepal.Length.
mean(iris$Sepal.Length) # mena est la formule avg
#11 Calculer la médiane de la variable Sepal.Width.
median(iris$Sepal.Width)
#12 Calculer l'écart-type de la variable Petal.Length.
sd(iris$Petal.Length)
#13 Calculer les déciles de la variable Petal.Width.
quantile(iris$Petal.Width, prob = seq( from=  0.1, to =0.9, by = 0.1))




#Exercice 2 - Import/Exporter un dataframe
# 1 Importer le jeu de données manga.csv dans un objet appelé dfManga et le jeu de données anime.csv 
#dans un objet appelé dfAnime. Puis afficher la classe des deux objets pour vérifier que la classe 
#est data.frame.
dfManga = read.csv("D:/R/Donnée/TP1/manga.csv", header = TRUE, sep = ",", dec=".")
dfAnime = read.csv("D:/R/Donnée/TP1/anime.csv", header = TRUE, sep = ",", dec=".")
class(dfAnime)
class(dfManga)
#3 Afficher le nombre de lignes et colonnes avec la fonction dim()
dim(dfAnime)
dim(dfManga)
#4 Calculer la moyenne de la variable Score pour les deux dataframe. Quelle est la moyenne la plus élevée ?
mean(dfManga$Score)
mean(dfAnime$Score)
#5 Calculer le nombre total de votes de la variable Vote pour les deux dataframe. Ou y a t-il le plus de votes ?
sum(dfManga$Vote)
sum(dfAnime$Vote
#6 Calculer l'écart-type des notes de la variable Score pour les deux dataframe. Ou est l'échantillon le plus homogènes au niveau des Score ?
sd(dfManga$Score)
sd(dfAnime$Score)
#7 Calculer les déciles des notes de la variable Score pour les deux dataframe. Quelle dataframe a le décile 1 le plus petit ?
quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))


#Les fonctions subset(), table() et prop.table()

#1 Combien de Manga ont une note strictement supérieure à 9/10 ?
manga_neuf_sur_dix = subset(dfManga,Score > 9 )
View(manga_neuf_sur_dix) # pour voir la table 
nrow(manga_neuf_sur_dix)# pour voir le nombre de manga ( le nombre de ligne )
#2 Combien de Manga ont 200000 votes ou plus ?
manga_20000 = subset(dfManga,Vote >= 20000 )
nrow(manga_20000)
#3 Combien de Manga ont strictement plus de 200000 votes et plus de 8/10 ?
extraction3 <- subset(dfManga, Vote >= 200000 & Score >= 8)
nrow(extraction3)
#4 Combien de Manga ont une note comprise entre 7/10 et 8/10 ? 
extraction4 = subset(dfManga, Score > 7 & Score < 8)
nrow(extraction4)


#1 Calculer les effectifs de la variable Rating(). Combien de modalité de la variable existe-t-il ? Calculer ensuite les effectifs en pourcentage.
effectifRating <- table(dfAnime$Rating)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)
#2 Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity) ?
extraction5 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(extraction5)
#3 Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity) et ont une note supérieur à 8/10 ?
extraction6 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score > 8 )
nrow(extraction6)



#Les fonctions rbind() et write.table()
#1 Modifier les deux dataframe en ne conservant que les variables : Title,Score,Vote,Ranked.
dfAnime <- dfAnime[ , c("Title","Score","Vote","Ranked")]
dfManga <- dfManga[ , c("Title","Score","Vote","Ranked")]

#2 Pour chaque dataframe créer une colonne Type avec pour valeur Anime ou Manga selon l'objet.
dfAnime$Type <- "Anime"
dfManga$Type <- "Manga

#3 Compiler les deux dataframe avec la fonction rbind() dans un objet appelé dfConcat.

