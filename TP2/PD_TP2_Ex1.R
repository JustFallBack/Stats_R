# install.packages("datasauRus")
library(datasauRus) # Chargement de la librairie datasauRus

set1 <- datasaurus_dozen[datasaurus_dozen$dataset=="dino",2:3] # Extraction de la première BDD
set2 <- datasaurus_dozen[datasaurus_dozen$dataset=="star",2:3] # Extraction de la deuxième BDD

# Analyse de la première BDD
summary(set1)
sd(set1[,1])
sd(set1[,2])
cor(set1[,1], set1[,2])

# Analyse de la deuxième BDD
summary(set2)
sd(set2[,1])
sd(set2[,2])
cor(set2[,1], set2[,2])

boxplot(set1)
boxplot(set2)

plot(set1[,1], set1[,2], lwd = "2", col = "darkgreen")
plot(set2[,1], set2[,2], lwd = "2", col = "yellow")