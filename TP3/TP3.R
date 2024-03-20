# TP3 - Loi des grands nombres. Théorème central limite.

# On considère le lancer de deux dés. On considère la somme des deux dés.
# On remarque que cette somme est comprise entre 2 et 12 et que contrairement au lancer d'un dé, les différentes issues ne sont pas équiprobables.
# Soit X et Y les deux dés. On a alors X ~ U(1,6) et Y ~ U(1,6).

# P(X+Y = 2) = 1/36
# P(X+Y = 3) = 2/36
# P(X+Y = 4) = 3/36
# P(X+Y = 5) = 4/36
# P(X+Y = 6) = 5/36
# P(X+Y = 7) = 6/36
# P(X+Y = 8) = 5/36
# P(X+Y = 9) = 4/36
# P(X+Y = 10) = 3/36
# P(X+Y = 11) = 2/36
# P(X+Y = 12) = 1/36

# Espérence de X + Y : E[X + Y] = 2 * 1/6 * (1 + 2 + 3 + 4 + 5 + 6) = 42/6 = 7.

# Espérance de la somme de 100 dés : E([S] = 100 * 7 = 700.

# Espérance de la moyenne de deux dés : (E[X] + E[Y])/2 = 7/2 = 3.5.

# Variance de X : Var[X] = E[X^2] - E[X]^2 = 1/6 * (1^2 + 2^2 + 3^2 + 4^2 + 5^2 + 6^2) - (3.5)^2 = 35/12.

# Variance de X + Y : Var[X + Y] = E[(X + Y)^2] - E[X + Y]^2 = 91/6 - 7^2 = 35/6.


# simulation d'un dé lancé n fois

n = 100

X = floor(6 * runif(n) + 1)

mean(X) # moyenne empirique de n lancé de dés.

var(X) # variance empirique de n lancé de dés.


# Simulation de k dés, chaque dé est en réalité la moyenne de n lancés.

k = 100 # nombre de simulations

S = numeric(k) # vecteur pour stocker les sommes des dés.

for (i in 1:k) {
  X = floor(6*runif(n) + 1) # vecteur contenant n lancer de dés.
  S[i] = mean(X) # S est un vecteur qui contient les moyennes des n lancés pour chaque k dés.
}

hist(S, prob = T, main = paste("Histogramme de la moyenne de",k ,"dés"), xlab = "Moyenne", ylab = "Densité", col = "lightblue", border = "black")

mean(S) # moyenne empirique de la moyenne de k dés.

var(S) # variance empirique de la moyenne de k dés.


# Maitenant, on transforme le vecteur S pour qu'il soit centré et réduit, afin de l'assimuler à une variable aléatoire suivant N(0,1).

S_centre = (S-3.5)/(sqrt(35/(12*n))) # On soustrait la moyenne (centrage) et on divise par l'écart type(réduction).

hist(S_centre, prob = T, main = paste("Histogramme de la moyenne de",k ,"dés centrée et réduite"), xlab = "Moyenne centrée et réduite", ylab = "Densité", col = "pink", border = "black")

curve(dnorm(x), col ="black", lwd = 2, add = T) # On ajoute la densité théorique de la loi normale centrée réduite.

# On observe que pour n suffisamment grand, la moyenne de k dés suit une loi normale centrée réduite. C'est le théorème central limite.