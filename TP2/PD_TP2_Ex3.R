# TP2 : variables continues
# Exercice 3 : Loi normale N(0,1)

# Petit rappel : X ~ N(0,1) signifie que X suit une loi normale de moyenne 0 et d'écart-type 1.


# Définition des fonctions de répartitions

p = function(x) pnorm(x, 0, 1, lower.tail = TRUE) # p(x) = P(X <= x)

g = function(x) pnorm(x, 0, 1, lower.tail = FALSE) # g(x) = P(X > x)

# Calcul des probabilités théoriques

p(0.41) # P(X <= 0.41)

p(-0.2) # P(X <= -0.2)

g(-1.54) # P(X > -1.54)

p(1.2)-p(-0.63) # P(-0.63 <= X <= 1.2) ou P(X <= 1.2) - P(X <= -0.63)

summary(qnorm(0.75, 0, 1)) # Q(p) = x tel que P(X <= x) = p


# Simulation de n observations de X, on obtient une répartition empirique.

n = 100

test = rnorm(n, 0, 1) # Génère n observations de X ~ N(0,1)

hist(test, main = paste("Histogramme de", n,"observations de X"), xlab = "X", ylab = "Fréquence en %")


# Calcul de l'approximation des probabilités théoriques.

mean(test <= 0.41) # Fréquence empirique de X <= 0.41

mean(test <= -0.2) # Fréquence empirique de X < -0.2

mean(test > -1.54) # Fréquence empirique de X > -1.54

mean(test <= 1.2) - mean(test <= -0.63) # Fréquence empirique de -0.63 <= X <= 1.2)

# On remarque que pour n suffisamment grand, les fréquences empiriques se rapprochent des probabilités théoriques.