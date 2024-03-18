# TP3 - Loi uniforme discrète

# simulation d'un dé lancé n fois
n = 100
X = floor(6 * runif(n) + 1)

mean(X)
var(X)


# histogramme de 100 dés (chaque dé est en réalité la moyenne de n lancés)
k = 1000
S = numeric(k)

for (i in 1:k) {
  X = floor(6*runif(n) + 1)
  S[i] = mean(X)
}

#hist(S, prob = T)
#mean(S)

S_centre = (S-3.5)/(sqrt(35/(12*n)))
hist(S_centre, prob = T)
curve(dnorm(x), col ="blue", lwd = 2, add = T)