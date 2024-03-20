# TP4 - Intervalle de confiance. Tests statistiques
# Exercice 2


Pop21 = rep(0, 28900000) # 0 pour la population active (28.9 millions).
Pop21[1:5370000] = 1 # 1 pour les gens inscrits à Pôle Emploi (5.37 millions).


n = 1000 # Échantillon de n personnes

alpha = 0.1 # Intervalle de confiance à 90%

Sondage21 = sample(Pop21, n, replace = FALSE) # simule le tirage de n éléments du vecteur Pop21.

tauxSondage = mean(Sondage21) # Moyenne du vecteur Sondage21

ICInf = tauxSondage - qnorm(1-alpha/2)*sqrt(tauxSondage*(1-tauxSondage)/n)  # Borne inférieure de l'intervalle de confiance.

ICSup = tauxSondage + qnorm(1-alpha/2)*sqrt(tauxSondage*(1-tauxSondage)/n) # Borne supérieure de l'intervalle de confiance.

IC = c(ICInf, ICSup) # vecteur intervalle de confiance
print(IC)


# Avec le même seuil, i.e. alpha = 0.1, on obtient un intervalle de confiance plus petit que dans l'exercice 1, mais centré autour de 0.19.


# On formule les hypothèses mathématiques suivantes:
# H0 : p = 0.098
# H1 : p != 0.098

prop.test(sum(Sondage21), n, p = 0.098, conf.level = 1-alpha) # Test avec la loi binomiale.

# Il faut observer ici "lap-value". Si elle est inférieure à alpha, on rejette H0.
# Or, sur chaque test, la lap-value est très proche de zero. 
# Comme alpha = 0.1, il s'ensuit que alpha > 0. Ainsi, on peut rejeter H0.

# Finalement, on peut donc conclure que le taux de chomage en 2021 est différent de celui en 2017.