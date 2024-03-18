# TP1 - Variables discrètes

# Exercice 1 - Simulation de variables aléatoires discrètes

n = 100

U = runif(n) # simulation de n variables uniformes de la loi U(0,1).

mean(U) # moyenne empirique des variables uniformes.

var(U) # variance empirique des variables uniformes.

# histogramme des n variables uniformes.
hist(U, col="magenta", main = paste("Simulation de", n, "variables uniformes U(0,1)"), ylab = "Densité", xlab = "U", probability = TRUE)

# On obtient une répartition empirique des variables.
# Plus n augmente, plus la répartition empirique se rapproche de la répartition théorique (0.5), la densité se stabilise.
# Théoriquement, la probabilité que U < 0.5 avec U ~ U(0,1) est de 0.5.


# Soit X = 1 si U < 0.5, 0 sinon.
# La probabilité que X = 1 est de 0.5.
# La variable X suit donc une loi de Bernoulli de paramètre 0.5
X = (U < 0.5)
as.numeric(X)

# Théoriquement, E[X] = p = 0.5.
sum(X) # proportion empirique de 1 dans X (i.e. le nombre de valeur inférieure à 0.5 dans U).

# Théoriquement, var[X] = p(1-p) = 0.5(1-0.5) = 0.25.
var(X) # variance empirique de X

# Comme auparavant, plus n augmente, plus la proportion de 1 se rapproche de l'espérance théorique, idem pour la variance.


# On considère une autre variable Y = 1 si U < p, 0 sinon, avec p ~ U(0,1).
p = runif(n) # Tirage uniforme de n variables

Y = (U < p) # On simule n variables de Bernoulli (loi de paramètre p)
as.numeric(Y)

mean(Y) # moyenne empirique de Y

var(Y) # variance empirique de Y

# On observe que la moyenne de Y est proche de 0.5 et que la variance de Y est proche de 0.5*(1-0.5) = 0.25
# On peut donc en déduire que Y suit une loi de Bernoulli de paramètre 0.5.