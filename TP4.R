x=c(10,20,35,50,70,90,110,130)
y=c(5,3.75,2.75,2.25,1.75,1.25,0.8,0.5)
donnee=data.frame(x,y)
#representation graphique

cor.test(x,y)#il existe une forte corrélation negative significative : lorsque la quantité augmente le prix diminue
model=lm(y~x,data = donnee)
model
plot(x,y,main = "Nuange de points")
abline(model,col="blue",lty=3,lwd=3) #droite de régression 
lines(exp(u),yi,col="green")
coef(model)
anova(model)
summary(model)#les paramètres fixes du model sont estimés : 4.424 pour l'ordonnée à l'origine 
#et -0.034 pour la pente.
#Tous ces paramètres sont significativement différents de 0 au seuil de 5%.
#lorsque la quantité de tomate d'1kg, le prix baisse en moyenne de 0,034.
#le coefficient de détermination étant égale à 0.8997: 89,97% de la variabilité des prix des tomates
#est expliquée par le model.
#Le test de Fisher étant hautement significatif donc on a un bon model.
x0=140
yi=predict(model2)
predict(model,newdata = data.frame(x=x0))


predict(model,newdata = data.frame(x=x0),interval = "confidence")

predict(model,newdata = data.frame(x=x0),interval = "prediction")

u=log(x)
u
 cor.test(y,u)
plot(u,y,main = "Nuange de points")
 model2=lm(y~u)
summary(model2) 
predict(model2,newdata = data.frame(u=log(140)))
predict(model2)



