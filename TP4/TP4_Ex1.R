# TP4 - Intervalles de confiance. Tests statistiques


# Exercice 1

Pop17 = rep(0, 29700000) # 29.7 millions personnes actives en 2017 en France.
Pop17[1:2900000] = 1 # 2.9 millions de chômeurs dans les 29.7 millions personnes actives.


# Soit X le nombre d'individu au chomage. 
# X suit une loi binomiale de paramètres n = 29.7 millions et p = 0.1 (p = 29 700 000 / 2 900 000) ≈ 0.1.
# X ~ B(29700000, 0.1)


taux = mean(Pop17) # Moyenne du vecteur Pop17

print(mean(taux)) # Probabilité qu'un individu prit au hasard soit au chômage.


n = 100 # On considère n personnes prit au hasard.

Sondage17 = sample(Pop17, n, replace = FALSE) # simule le tirage de n éléments du vecteur Pop17.


tauxSondage = mean(Sondage17) # Moyenne du vecteur Sondage17

print(mean(tauxSondage)) # Probabilité qu'un individu prit au hasard soit au chômage.

# Avec n = 100, la moyenne du veteur Sondage17 est plutôt éloignée du taux de chômage réel. 
# Cela s'explique car n est trop petit par rapport à la taille de la population.


# Intervalle de confiance pour la proportion de chômeurs avec la loi normale.

alpha = 0.2 # Niveau de confiance

ICInf = tauxSondage - qnorm(1-alpha/2)*sqrt(tauxSondage*(1-tauxSondage)/n)  # Borne inférieure de l'intervalle de confiance.

ICSup = tauxSondage + qnorm(1-alpha/2)*sqrt(tauxSondage*(1-tauxSondage)/n) # Borne supérieure de l'intervalle de confiance.

IC = c(ICInf, ICSup) # vecteur intervalle de confiance.

print(IC) # Intervalle de confiance pour la proportion de chômeurs avec la loi normale.


# Intervalle de confiance pour la proportion de chômeurs avec la loi binomiale
prop.test(sum(Sondage17), n, conf.level = 1-alpha)$conf.int

# Pratiquement toutes les valeurs obtenues dans Sondage17 sont compris dans l'intervalle.

# Le réel taux de chomage (0.1) est presque toujours compris dans l'intervalle, que ce soit celui calculé avec la loi normale ou celui avec la loi binomiale.


# En augmentant n (n = 1000), l'intervalle de confiance se rétrécit.
# De plus, la proportion de chômeurs calculée plus tôt se rapproche du véritable taux de chômage.

# Après plusieurs tests, le taux de chômage est presque toujours compris dans l'intervalle de confiance.
# On peut donc dire que n = 1000 est suffisant pour obtenir un intervalle de confiance fiable.


# Construction de k intervalles de confiance pour la proportion de chômeurs avec la loi binomiale.

k = 20

n = 10000

ConfInts = matrix(ncol = k, nrow = 2) # Matrice de 2 lignes et k colonnes

for (i in 1:k) { # k intervalles de confiances
  
  Sondage17 = sample(Pop17, n, replace = FALSE) # simule le tirage de n éléments du vecteur Pop17.
  
  ConfInts[,i] = prop.test(sum(Sondage17), n, conf.level = 1-alpha)$conf.int[1:2] # Intervalle de confiance pour la proportion de chômeurs (loi binomiale).
}

matplot(ConfInts, rbind(1:k,1:k), type="l", lty=1, xlab="Intervalles de confiance", ylab="Indice de l'intervalle") # Affiche les intervalles de confiance.

abline(v = mean(Pop17), lwd=2, col="red") # Ajoute la vraie valeur du taux de chômage

# Il y a environ (k * alpha) intervalles qui ne contiennent pas la vraie valeur du taux de chômage.
# Avec k = 20 et alpha = 0.2, on observe bien qu'en moyenne, il y a 4 intervalle qui ne contiennent pas la vraie valeur du taux de chômage.


# En augmentant n (n = 10000), on observe que les intervalles de confiance se rétrécissent.
# Cependant, on observe toujours qu'en moyenne, il y a (k * alpha) intervalles qui ne contiennent pas la vraie valeur du taux de chômage.