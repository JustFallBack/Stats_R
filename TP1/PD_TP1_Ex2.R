# TP1 - Variables discrètes

# Exercice 2 - Surbooking dans une compagnie de transport aérien

# On suppose que le nombre de passagers qui viennent à l'embarquement suit une loi de Bernoulli de paramètre 0.93. 
# On suppose également que l'avion a 104 places. 
# On cherche à simuler le nombre de passagers qui viennent à l'embarquement et à calculer la probabilité que l'avion soit surbooké.
# Comme 7% des passagers ne viennent pas à l'embarquement, on peut simuler le nombre de passagers qui viennent à l'embarquement en simulant 110 variables de Bernoulli de paramètre 0.93.

n = 1000

vol_n = numeric(n) # vecteur de taille n

for (i in 1:n) {
  
  U = runif(110) # on simule 110 variables uniformes sur [0,1]
  
  X = (U < 0.93) # on simule 110 variables de Bernoulli (loi de paramètre 0.93)
  
  n_pass = sum(X) # on compte le nombre de passagers qui sont venus
  
  vol_n[i] = as.numeric((104 - n_pass) < 0) # on retourne 1 s'il y a plus de passagers que de places
  
}
mean(vol_n) # probabilité que l'avion soit surbooké

sum(vol_n) # nombre de vols surbookés

# On observe qu'en moyenne, 20% des vols sont surbookés


# On peut également calculer la probabilité que l'avion soit surbooké en utilisant la loi binomiale.
# La loi de probabilité d'un évenement'k parmi 110 passagers viennet à l'embarquement' est une loi binomiale de paramètre 110 et 0.93.

# Probabilité que le vol soit surchargé (au moins 105 passagers sont venus) :
k = 105:110

sum(choose(110,k)*(0.93^k)*(0.07^(110-k))) # somme des probabilités que k passagers viennent à l'embarquement

sum(dbinom(k,110,0.93)) # fonction de densité de la loi binomiale - probabilité que k passagers viennent à l'embarquement

pbinom(104,110,0.93, lower.tail = FALSE) # probabilité que le vol soit surchargé

# On observe que la probabilité que le vol soit surchargé est de 0.21. 
# Ces trois méthodes de calcul nous donne le même résultat.