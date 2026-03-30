######## Mouhamed Ba
######## L3 AgroTic

rm(list=ls())
##
#################lecture de la base
data=read.table("C:/Users/user/OneDrive/Bureau/#RASSOUL/#rassoul221/MODELES LINEAIRES/Exercice_2024-2025/data_menage.csv",sep="",header = TRUE)
data
str(data)

################## Création des variables logarithmiques
data$logrevenu <- log(data$revenu)
data$logrevenu
data$logdepense <- log(data$depense)
data$logdepense

################### Estimation des modèles
########### le modele M1
M1 <- lm(depense ~ revenu, data = data)
M1
predict(M1, data.frame(revenu = c(100000)))
predict(M1, data.frame(revenu = c(600000)))

########### le modele M2
M2 <- lm(depense ~ logrevenu, data = data)
M2
predict(M2, newdata=data.frame(logrevenu=log(100000)))
predict(M2, newdata=data.frame(logrevenu=log(600000)))

######### le modele M3
M3 <- lm(logdepense ~ logrevenu, data = data)
M3
predict(M3, newdata=data.frame(logrevenu=log(100000)))
predict(M3, newdata=data.frame(logrevenu=log(600000)))

########## representation graphique
plot(data$revenu, data$depense)
abline(M1, col="red", lwd=2)

plot(data$logrevenu, data$logdepense)
abline(M3, col="blue", lwd=2)

########## ajout droite M2
plot(data$logrevenu, data$depense)
abline(M2, col="green", lwd=2)

############ Résultats
# pour le premier model
summary(M1)
summary(M1)$coefficients[,4]

# Interprétation M1 :
# - Les p-values sont très faibles (< 0.05)
# → le revenu est significatif
# - La relation est positive : plus le revenu augmente, plus la dépense augmente
# - Mais le modèle est trop simple (linéaire)

# pour le deuxieme model
summary(M2)
summary(M2)$coefficients[,4]

# Interprétation M2 :
# - Les p-values sont également faibles → modèle significatif
# - Relation non linéaire entre revenu et dépense
# - Meilleur que M1 sur le plan économique

# pour le troisieme model
summary(M3)
summary(M3)$coefficients[,4]

# Interprétation M3 :
# Les p-values sont très faibles donc le modèle très significatif
# Le coefficient de logrevenu représente une élasticité
# % variation de la dépense quand le revenu varie de 1%
# Relation réaliste en économie (loi d’Engel)

##CONCLUSION
# Les trois modèles sont globalement significatifs (p-values < 0.05).
# Cependant, le modèle M3 est le plus pertinent.
# Il permet une meilleure interprétation économique grâce à l’élasticité.
# On observe que la dépense augmente avec le revenu, mais moins rapidement.
# Donc, plus un ménage est riche, moins la part consacrée à l’alimentation est élevée.