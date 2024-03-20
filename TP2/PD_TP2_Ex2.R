# TP2 - Variables continues
# Exercice 2 : Loi exponentielle

# On suppose que la durée d'attente en minutes de la première alerte après 18h dans une entreprise suit une loi exponentielle de paramètre λ = 0.1.
# On étudie la distribution de la variable aléatoire qui modélise le temps d'attente.


# On utilise une méthode qui consiste à simuler des variables uniformes U_n ~ U(0,1) et à appliquer la fonction inverse.

λ = 0.1 # Intensité de la loi exponentielle

n = 100000

U_n = runif(n) # Simulation de n variables uniformes

X = -log(1-U_n) / λ # fonction inverse de la loi exponentielle

# Histogramme de la loi exponentielle (empirique)
hist(X, prob = T, main = paste("Histogramme de la loi exponentielle de paramètre ", lambda), xlab = "Temps d'attente (en minutes)", ylab = "Densité", col = "lightgreen")

# Une courbe modélisant la densité théorique
curve(dexp(X, rate = λ), col ="red", add = TRUE, xname = "X", ylab = "Densité", lwd = 2)

# On voit que la densité empirique est proche de la densité théorique avec n suffisamment grand.


# Calcul de la probabilité qu'il n'y ait aucune alerte avant 18h30.

mean(X > 30) # Probabilité empirique

pexp(30, λ, lower.tail = FALSE) # Probabilité théorique

# On voit que la probabilité empirique est proche de la probabilité théorique pour n suffisamment grand.


# Calcul de la probabilité qu'il n'y ait aucune alerte avant 19h.

mean(X > 60) # Probabilité empirique.

pexp(60, λ, lower.tail = FALSE) # Probabilité théorique.

# On voit que la probabilité empirique est proche de la probabilité théorique pour n suffisamment grand.


# Estimation empirique de la probabilité qu'il n'y ait aucune alerte avant 19h sachant qu'il n'y a aucune alerte avant 18h30.

X_30 = X[which(X > 30)]
mean(X_30 > 60)

# On observe que la probabilité empirique qu'il n'y ait aucune alerte avant 19h sachant qu'il n'y a aucune alerte avant 18h30 est la même que la probabilité qu'il n'y ait aucune alerte avant 18h30.