#creation de la base 
base=data.frame(Rdm=c(1.8,2.2,2.9,3.6,4,4.5,5.2),Dose=c(1,2,2,3,4,4,5),Sol=c(2,2,3,3,3,4,4))
#Modelisation
Model=lm(Rdm~Dose+Sol,data = base)
coef(Model)

 #tests sur les parametre fixe de model

summary(Model)
#Tests de Students :
#Le test sur la nullité de la constane beta_0 n'est pas significative. Alors, elle est nulle
#au seuil de 5%.Le test sur la nullité de beta_1 est hautement significative.Alors la dose a
#un impact significatf sur le rendement. De même que beta_, la quantité du sol a un impact 
#significative sur le rendement.

#Table d'analyse de variance 

anova(Model)

#prevision

#Dose=3.5 et sol=2.25

predict(Model,newdata = data.frame(Dose=3.5,Sol=2.25))
