
#Exercice 1 - Importer les données
#1 Importer le jeu de données velov.csv à l’aide de la fonction read.csv().
df = read.csv(file = "velov.csv",
              header = TRUE,
              sep = ";", 
              dec = "," )
#2 Effectuer un résumé des données. On constate que des variables sont mal typées comme status et CodePostal.
summary(df)
class(df$status)
class(df$CodePostal)
#3Passer ces deux variables en type factor.
df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)
#4 On souhaite vérifier s'il y a des bornes indisponibles sur les stations. 
#Il suffit de vérifier si le nombre de vélos et places disponibles est égal à la capacity de la station. 
#Créer une colonne nommée bornes avec la valeur OKou KO s'il y a un ou plusieurs bornes indisponibles sur une station. 
#Combien y a-t-il de stations avec au moins une bornes HS ?
df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")
table(df$bornes)


#Exercice 2 - L'histogramme
#Exercice sur les Fonctions en R
#1 Construire un histogramme de la distribution des capacity à l'aide de la fonction hist(). 
#N'oublier pas de mettre un titre.
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations")
#2 Construire le même graphique mais avec 6 classes.
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,)
#3 Construire le même graphique mais en rouge.
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red",)
#4 Renommer l'axe des abscisses par Capacity.
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red",
     xlab = "capacity")
#La fonction abline()
#5 Sur le dernier graphique, ajouter une ligne horizontale bleue qui à pour ordonné la valeur 100, 
#à l'aide de la fonction abline(). La ligne s'ajoute au graphique en cours de lecture dans la fenêtre graphique. 
#Vous pouvez personnaliser le trait de la ligne avec l'argument lty.
abline(h = 100, col = "blue")
#Les fonctions hist(), lines() et density()
#6 Construire le même graphique mais avec la densité plutôt que les effectifs. 
#Supprimer l'argument break pour rétablir les classes par défaut.
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity")
#7 Ajouter la courbe densité de cette distribution à l'aide des fonctions lines() et density(). 
#On peut mettre cette courbe en bleu en changeant la taille de la courbe avec l'argument lwd.
lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 4)
#8 Pour voir la courbe density en entier, modifier les bornes de l'axe des ordonnées de l'histogramme avec l'argument 
#ylim. Relancer l'ensemble des commandes pour tracer à nouveau le graphique.
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

#Exercice 3 - Le boxplot

#La fonction boxplot()
#1 Construire une boîte à moustache de la distribution des capacity à l'aide de la fonction boxplot(). 
#N'oublier pas de mettre un titre.
boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations")
#2 Construire le même graphique mais pivoter horizontalement.
boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal  = TRUE)
#3 Construire le même graphique en le remettant à la verticale et en n'affichant pas les valeurs atypiques.
boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations",
        outline =FALSE)
#4 Ajouter un point supplémentaire qui correspond à la moyenne de la série avec la fonction points(). 
#On souhaite que ce point soit un gros carré rouge
points(x = mean(df$capacity, narm = TRUE),
       col = "red",
       pch = 16,)
#La fonction par()
#5 On souhaite comparer les vélos disponibles sur le 7ème et le 8ème arrondissement. 
#Diviser la fenêtre graphique en deux puis constuire un boxplot pour ces deux arrondissement. 
#Que peut-on dire ?
par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))
#6 Sur le même graphique, on souhaite analyser le nombre de vélos disponibles en fonction de la variable bonus. 
#Que peut-on dire ?
par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")
#Les fonctions points() et tapply().
#7 Ajouter les moyennes de chaque groupes sur le graphique à l'aide de la fonction tapply() et points(). 
# Calculer les moyennes de chaque groupe
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)

#Exercice 4 - Le diagramme


