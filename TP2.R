#Importation de la base 
data=read.table("eucalyptus.txt",sep=";",header = T)
data
#interpretation de la commande 
str(data)
#la commande donne le nombre de variables, leur type 
#Boite a moustache
boxplot(data$numero,main="histogramme des numeros")$out
boxplot(data$ht,main="histogramme de ht")$out
boxplot(data$circ,main="histogramme de circ")$out
boxplot(data$bloc,main="histogramme de bloc")$out

#Histogramme

hist(data$numero,main = "Histogramme des numéros",col = "blue")
hist(data$ht,main = "Histogramme des ht",col = "green")#on a une asymétrie à gauche avec la présence de  25 valeurs atypiques à gauche  
hist(data$circ,main = "Histogramme des circ",col = "red")#La distribution est plus ou moins symétrique avec 2 valeurs atypiques à gauche  
hist(data$bloc,main = "Histogramme des bloc",col = "yellow")

#Test de correlation entre circ et ht

cor.test(data$circ,data$ht)#on a une forte corrélation positive, plus la circ augmente plus ht augmente 

#Creation de la variable Reg

Reg=lm(ht~circ,data = data)#permet d'expliquer la hauteur en fonction de la surface 
Reg
#Nuange de points
plot(data$circ,data$ht,main = "Nuange de points",xlab="circonference",ylab="Hauteur",pch=16)

abline(Reg,col="red",lty=3,lwd=3)


anova(Reg)

summary(Reg)
#on obtient un résumé statistique sur les residus,le tableau des coef, le coef de determination
#la commande anova nous donne la table d'analyse de variance
#les residus varient entre -4.76 et 3.69 avec une valeur mediane de 0.05
#
#la pante est estimée à 0.257 qui est significativement différentes de 0 
#lorsque la circ augmente de d'une unité faut s'attendre à une augmentation en moyenne de la hauteur de 0.257
#l'ordonnée à l'origine  est estimée à 9.03 qui est aussi significativement différentes de 0 mais qui ne 
#peut pas être interprétée dans ce contexte

#Le coef de détermination indique que 76,83% de la variabilité des hautereurs est expliquée par le model
#Enfin, le test de fisher étant hautement significative, donc le modèle est bon.

confint(Reg)
x0=50
predict(Reg,newdata = data.frame(circ=x0),conf=.95,interval = "confidence")
#pour une circonférence de 50, la hauteur est estimée à 21.89
#A 95% de confiance que cette estimation soit comprise entre 21.81 et 21.96