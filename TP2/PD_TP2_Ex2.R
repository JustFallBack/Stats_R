# TP2 - Variables continues
# Exercice 2 : Loi exponentielle

lambda = 0.1 # Intensité de la loi exponentielle
n = 100000
U_n = runif(n) # Simulation de n variables uniformes
X_n = -log(1-U_n)/lambda # fonction inverse
hist(X_n, prob = T)
curve(dexp(x, rate = lambda), col ="red", add = TRUE)

# Calcul de la probabilité qu'il n'y ait aucune alerte avant 22h30.
mean(X_n>30) #Proba empirique
pexp(30, lambda, lower.tail = FALSE) # Probabilité théorique

# Estimation empirique de la probabilité qu'il n'y ait aucune alerte avant 23h.
mean(X_n>60)

# Estimation empirique de la probabilité qu'il n'y ait aucune alerte avant 23h sachant qu'il n'y a aucune alerte avant 22h30.
X_n_30 <- X_n[which(X_n>30)]
mean(X_n_30>60)
pexp(30, lambda, lower.tail = FALSE)