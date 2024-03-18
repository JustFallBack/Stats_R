# TP Noté - Elliot Pozucek

# L'objectif est de simuler des paiements de sinistres pour une compagnie d'assurance pour ajuster les tarifs.
# On suppose que les montants des sinistres suivent une loi normale de moyenne 1250 et d'écart-type 400.
# On suppose que la probabilité d'avoir un sinistre est de 8%.

n = 1500 # On suppose que l'entreprise a 1500 clients.

p = 0.08 # On suppose que la probabilité d'avoir un sinistre est de 8%.

X = rnorm(n, 1250, 400) # Vecteur des montants des sinistres (loi normale).

# Le montant total des sinistres (théorique) = 1500*1250*0.08 = 150 000.


# Soit Y la variable aléatoire qui vaut 1 si le client a un sinistre et 0 sinon.
Y = sample(c(1,0), n, replace = TRUE, prob = c(p, 1-p)) # Vecteur des remboursements (1 si le client a un sinistre, 0 sinon).


# Le nombre de sinistres est donné par la somme des valeurs de Y.
sum(Y) # Théoriquement : 0.08 * 1500 = 120.


# Le montant total des paiements est donné par la somme des montants des sinistres multipliés par les remboursements.
payments = X*abs(Y) # Vecteur des paiements.

sum(payments) # Somme des paiements empirique.

mean(payments) # Moyenne empirique des paiements.

max(payments) # Montant maximal (empirique) des paiements.

median(payments) # Médiane des paiements (ici toujours égale à 0).

sd(payments) # Ecart-type empirique des paiements.


# Histogramme du vecteur payments, qui exclue Y = 0 (i.e.lorsque qu'il n'y pas de remboursement).
hist(payments[payments != 0], xlab = "Montant du paiement", main = "Histogramme des paiements de sinistres")


# Calcule du profit selon la formule tarifaire (90, 100, 110).
# Modifier la valeur de la variable 'tarif' pour obtenir le profit pour un tarif donné.
tarif = 90
profit = n * tarif - sum(payments) # Calcul du profit de l'entreprise d'assurance.
profit


# Calcul du profit théorique.
profit_théorique = n * tarif - n * 1250 * p
profit_théorique


# On se rend compte que le profit théorique est négatif pour un tarif de 90.
# Pour un tarif de 100, le profit théorique est nul.
# On peut donc dire que l'entreprise commence à être rentable à partir de tarif = 100.