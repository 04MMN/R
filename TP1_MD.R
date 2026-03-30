#Saisie des données 
Eng=c(100,200,300,400,500,600,700)
Rend=c(40,50,50,70,65,65,80)
donnes=data.frame(Eng,Rend) #création de la base de données 
donnes #affichage de la base 
plot(Eng,Rend,main = "Nuange de points")
#Test de corrélation 
cor.test(Eng,Rend)
#La P-value < 5%, alors il existe une corrélation significative entre le rendement 
#et la quantité d'engrais utilisée 
#l'intervalle de confiance de cette corrélation au niveau 95% est [0.54,0.98]

##Modelisation
model=lm(Rend~Eng,data = donnes)
model
plot(Eng,Rend,main = "Nuange de points")
abline(model,col="blue",lty=3,lwd=3) #droite de régression 

coef(model) #Affichage des coefficients du modèle 
#Si la quantité d'engrais augmente d'une unite, il faut s'attendre à une augmentation 
# du rendement en moyenne de 0.0589kg
#Si on ne mets pas d'engrais on peut s'attendre à un rendement  moyenne de 36.42

##Affichage des IC des coef
confint(model)
#IC(a)=[0.03,0.088] : 95 chances sur cent de contenir la vraie valeur de a 
#IC(b)=[23.47,49.37] 

##Table d'analyse de variance
anova(model)

summary(model)

##Prevision pour une quantité d'engrais de 325
x0=325
predict(model,newdata = data.frame(Eng=x0))
#La valeur ponctuelle predite est de 55.58kg
#Si on met325q d'engrais, on espere obtenir un rendement moyen de 55.58kg

predict(model,newdata = data.frame(Eng=x0),interval = "confidence")
#Nous avons 95%  de chances que le rendement moyen soit de 49.39 et 61.77 si on met
#une quantité de 325q.

predict(model,newdata = data.frame(Eng=x0),interval = "prediction")
#Nous avons 95%  de chances que le rendement moyen soit de 39 et 72 si on met
#une quantité de 325q, en tenant compte des erreus.

##Predire plusieurs donnees et le representation graphique 

new=data.frame(Eng=seq(90,705,10))
new
predict(model,new)
pred.IC=predict(model,new,interval = "confidence")
pred.IC
pred.IP=predict(model,new,interval = "prediction")
pred.IP
matplot(new$Eng,pred.IC[,-1],col = "red",lty = 3,type = "l",ylab = "",xlab = "",lwd = 3)
matplot(new$Eng,pred.IP[,-1],col = "blue",lty = 1,type = "l",add = T,lwd = 3)
abline(model,col="green",lty=3,lwd=3,add=T)
legend("topleft",legend = c("Int.conf","Int.Pari","Regression"),lty = c(3,1,3),col = c("red","blue","green",lwd=c(3,3,3)))
points(mean(Eng),mean(Rend),pch="o",lwd=4)
