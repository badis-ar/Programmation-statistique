#TD5
# Exercice 1 - Comprendre la loi normale
#La commande rnorm()

#1 Créer une graphique vide avec comme borne d'abscisse [-5,5] et d'ordonnées [0,1].
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")

# 2 Programmer une boucle for qui à chaque itération, ajoute une courbe densité issue d'une des 4 combinaisons de paramètres de loi normale suivant :
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "orange")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 1000, 
                mean = moyennes[i], 
                sd = sigmas[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

#3 Simuler une loi normale  N(μ=0,σ=1)de taille 10 000.
serie2 = rnorm(n = 10000, mean = 0, sd = 1)
#4 Contruire l'histograme de la distribution de la série avec sa courbe densité.
hist(x = serie2, main="ex 1.4" ,    probability = TRUE)
lines(density(serie))
#5 Calculer la médiane de la série.
median(serie2)
#6 Calculer les quartiles de la série.
quantile(serie2)
#7Calculer les centiles de la série. Quelle valeur de la série correspond au centile 0.95 ?
quantile(serie2, 
         probs = seq(from = 0, 
                     to = 1, by = 0.01))

pp = quantile(serie2, 
         probs = 0.95)
#8 Calculer la valeur théorique à l'aide de la fonction qnorm(). Vérifier cette valeur avec la fonction réciproque pnorm()
qnorm(p = 0.95,mean = 0, sd = 1)
pnorm(q =1.644854 ,mean = 0,sd = 1)
#9 Quelle est la valeur théorique pour P(X < x) = 0.975.
qnorm(p = 0.975, mean = 0, sd = 1)
#10Quelle est la probabilité théorique pour P(X >= 1.96) = p.
pnorm(q =1.96 ,mean = 0,sd = 1)

# Exercice 2 - Construire la table de loi normale

#1 A l'aide de boucle for. Construire une vecteur avec les probabilités de la première colonne de la table de loi normale. On souhaite une précision avec uniquement 4 décimales.
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#on crée un vecteur vide pour ajouter les probas au fur et à mesure
all_probas = c()
#On parcourt les indices lignes
for (i in indices_lignes){
  proba = pnorm(q = i, mean = 0, sd = 1)
  #on ajoute la nouvelle proba au vecteur existant
  all_probas = c(all_probas,proba)
  all_probas = round(all_probas,digits = 4)
}
#2 Modifier ce code pour construire la table de loi normale
indices_colones = seq(from = 0.00, to = 0.09, by = 0.01)
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#On crée un objet résultat vide.
resultat = NULL
#On parcourt les indices colonnes
for (j in indices_colones) {
  #on crée un vecteur vide pour ajouter les probas au fur et à mesure
  all_probas = c()
  #On parcourt les indices lignes
  for (i in indices_lignes){
    quantile = i + j
    proba = pnorm(q = quantile, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  #On ajoute une colonne au resultat
  resultat = cbind(resultat,all_probas)
}

# Exercice 3 - Simulation d'une population
#1 Produire les tailles d'une population simulée de 10.000.000 de français répartis suivant une loi normale 

population = rnorm(1e7, mean = 171, sd = 9)
#2 Calculer la moyenne et l'écart-type de la population. Retrouvez-vous les valeurs attendues ?
moyenne_pop = mean(ex3)
sd_pop = sd(ex3)
#3 Etablir l'histogramme de la taille. Retrouvez-vous la forme bien connues ?
hist(ex3)

#4 Combien de personnes ont une taille inférieur à 190cm ? Combien devriez-vous en trouver théoriquement ?
#observé
pop190 = population[population < 190]
length(pop190)
length(pop190) / length(population)

#en théorie
pnorm(q = 190, mean=moyenne_pop, sd=sd_pop)*1e7

#5 Combien de personnes ont une taille supérieur à 200 cm ? Combien devriez-vous en trouver théoriquement ?.
pop200 = population[population > 200]
length(pop200)/length(population)
length(pop200)

#théorie
pnorm(q = 200, mean=moyenne_pop, sd=sd_pop)
# Exercice 4 - Simulation d'échantillon

#La commande sample()
#1 Tirez un échantillon de taille 100 dans la population initiale, à l'aide de la fonction sample. Quelle est la taille moyenne dans l'échantillon ? Quelle est l'écart-type dans l'échantillon ? Ces deux valeurs sont-elles proches de celles de la population ?
