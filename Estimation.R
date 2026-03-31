#creation de la base 
base=data.frame(Rdm=c(1.8,2.2,2.9,3.6,4,4.5,5.2),Dose=c(1,2,2,3,4,4,5),Sol=c(2,2,3,3,3,4,4))
#Modelisation
Model=lm(Rdm~Dose+Sol,data = base)
coef(Model)
