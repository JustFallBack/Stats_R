# TP1 - Variables discrètes

# Exercice 1
# On cherche 
n = 100
U_n = runif(n) # simule n variables uniformes de la loi U(0,1).
U_n
mean(U_n) # moyenne empirique des variables uniformes
var(U_n) # variance empirique des variables uniformes
hist(U_n, col="magenta", main = paste("Simulation de", n, "variables uniformes U(0,1)"), ylab = "Densité" ,probability = TRUE) # histogramme des variables uniformes

# On obtient une répartition empirique des variables.
# Plus n augmente, plus la répartition empirique se rapproche de la répartition théorique, la densité se stabilise.
# Théoriquement, la probabilité que U < 0.5 avec U ~ U(0,1) est de 0.5.

# X = 1 si U < 0.5, 0 sinon.
X_n = (U_n<0.5)
as.numeric(X_n)
sum(X_n)
var(X_n)
# La probabilité que X = 1 est de 0.5
# La loi de X est une loi de Bernoulli de paramètre 0.5
# La variance de X est de 0.5*0.5 = 0.25 et son espérance est de 0.5
# Comme auparavant, plus n augmente, plus la proportion de 1 se rapproche de l'espérance théorique et la variance se stabilise

# On considère une autre variable Y = 1 si U < p, 0 sinon
p = runif(n) # Tirage uniforme de n variables
Y_n = (U_n<p)
as.numeric(Y_n)
mean(Y_n)
var(Y_n)
# On observe que la moyenne de Y est proche de p et que la variance de Y est proche de p*(1-p)
# On peut donc en déduire que Y suit une loi de Bernoulli de paramètre p

# Exercice 2

n = 1000
vol_n = numeric(n) # vecteur de taille n
for (i in 1:n) {
  U_n <- runif(110) # on simule 110 variables uniformes
  X_n <- (U_n<0.93) # on simule 110 variables de Bernoulli (loi de paramètre 0.93)
  n_pass <- sum(X_n) # on compte le nombre de passagers qui sont venus
  vol_n[i] = as.numeric((104 - n_pass) <0) # on retourne 1 s'il y a plus de passagers que de places
}
mean(vol_n) # probabilité que l'avion soit surbooké
# On observe qu'en moyenne, 20% des vols sont surbookés

# La loi de probabilité d'un évenement'k parmi 110 passagers viennet à l'embarquement' est une loi binomiale de paramètre 110 et 0.93.
# Probabilité que le vol est surchargé (au moins 105 passagers sont venus) :

k = 105:110
sum(choose(110,k)*(0.93^k)*(0.07^(110-k))) # somme des probabilités que k passagers viennent à l'embarquement
sum(dbinom(k,110,0.93)) # fonction de densité de la loi binomiale
pbinom(104,110,0.93, lower.tail = FALSE) 


floor(6*runif(1)) + 1

