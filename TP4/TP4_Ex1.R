# TP4 - Intervalles de confiance, tests statistiques - Exercice 1


Pop17 = rep(0, 29700000) # 29.7 millions de personne active en 2017 en France
Pop17[1:2900000] = 1 # 2.9 millions de chômeurs dans les 29.7 millions de personne active

# Question 1
# Quelle loi suit la variable 'nombre de personnes au chomage' ? Avec quel(s) paramètres ?
# Soit X - nombre de personnes au chomage. 
# X suit une loi binomiale de paramètres n = 29.7 millions et p = 0.1 (p = 29 700 000 / 2 900 000) ≈ 0.1.
# X ~ B(29700000, 0.1)

# Question 2 
taux = mean(Pop17)
print(mean(taux)) # Probabilité qu'un individu prit au hasard soit au chomage

# Question 3
n = 100 # 100 personnes
Sondage17 = sample(Pop17, n, replace = FALSE) # simule le tirage de n éléments du vecteur Pop17.

# Question 4
tauxSondage = mean(Sondage17) # Moyenne du vecteur Sondage17
print(mean(tauxSondage))
# Avec n = 100, la moyenne du veteur Sondage17est plutôt éloignée égale à taux. 
# Cela s'explique car n est trop petit par rapport à la taille de la population.

# Question 5
alpha = 0.2 # Niveau de confiance
ICInf = tauxSondage - qnorm(1-alpha/2)*sqrt(tauxSondage*(1-tauxSondage)/n)  # Borne inférieure de l'intervalle de confiance
ICSup = tauxSondage + qnorm(1-alpha/2)*sqrt(tauxSondage*(1-tauxSondage)/n) # Borne supérieure de l'intervalle de confiance
IC = c(ICInf, ICSup) # vecteur intervalle de confiance
print(IC)

# Question 6
# Intervalle de confiance pour la proportion de chômeurs avec la loi binomiale
prop.test(sum(Sondage17), n, conf.level = 1-alpha)$conf.int
# Pratiquement toutes les valeurs obtenues dans la question 3 sont compris dans l'intervalle.

# Question 7
# Le vrai taux de chomage (0.1) est presque toujours compris dans l'intervalle, que ce soit celui calculé à la question 5 ou celui calculé à la question 6.

# Question 8
# En augmentant n (n = 1000), l'intervalle de confiance se rétrécit.
# De plus, la proportion de chomeur calculée dans la question 2 et 4 se rapproche du vértiable taux de chomage.
# Après plusieurs tests, le taux de chomage est presque toujours compris dans l'intervalle de confiance.
# On peut donc dire que n = 1000 est suffisant pour obtenir un intervalle de confiance fiable.

# Question 9
k = 20 
n = 10000
ConfInts = matrix(ncol = k, nrow = 2) # Matrice de 2 lignes et k colonnes
for (i in 1:k) { # 20 intervalles de confiances
  Sondage17 = sample(Pop17, n, replace = FALSE) # simule le tirage de n éléments du vecteur Pop17.
  ConfInts[,i] = prop.test(sum(Sondage17), n, conf.level = 1-alpha)$conf.int[1:2] # Intervalle de confiance pour la proportion de chômeurs
}
matplot(ConfInts, rbind(1:k,1:k), type="l", lty=1, xlab="Intervalles de confiance", ylab="") # Affiche les intervalles de confiance
abline(v = mean(Pop17), lwd=2, col="red") # Ajoute la vraie valeur du taux de chomage
# Il y a environ k*alpha intervalle qui ne contiennent pas la vraie valeur du taux de chomage.
# Avec k = 20 et alpha = 0.2, on observe bien qu'en moyenne, il y a 4 intervalle qui ne contiennent pas la vraie valeur du taux de chomage.

# Question 10
# En augmentant n (n = 10000), on observe que les intervalles de confiance se rétrécissent.
# Cependant, on observe toujours qu'en moyenne, il y a k*alpha intervalle qui ne contiennent pas la vraie valeur du taux de chomage.
# Cela dit, les intervalles qui ne contiennent pas la vraie valeur du taux de chomage on tendance à être plus éloignés 
# de la vraie valeur du taux de chomage au fur et à mesure que n augmente.



