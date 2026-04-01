data("trees")
help(trees)
#model Diam=f(haut)

model1=lm(Girth~Height,data = trees)
summary(model1)

#Model diam=f(Haut) sans la constance 
sum(trees$Girth*trees$Height)/sum(trees$Height^2)

model1p=lm(Girth~Height-1,data = trees)
summary(model1p)
coef(model1p)

plot(trees$Height,trees$Girth,main = "Nuange de points")
abline(model1,col="blue",lty=3,lwd=3,)
abline(model1p,col="green",lty=3,lwd=3)
legend("topleft",legend = c("Model1","model1p"),lty = c(3,1),col = c("blue","green",lwd=c(3,3)))

summary(model1p)

# 3model2 : diam=f(vol)

model2=lm(Girth~Volume,data = trees)
summary(model2)

# 4 model3 : diam=f(vol,haut)
model3=lm(Girth~Height+Volume,data = trees)
summary(model3)

#Selection de la meilleure variable

step(model3)

#Chargement du package
library(DAAG)
#chargement de la base 
data("litters")
head(litters)

#Analyse 
#Modélisation 
model=lm(brainwt~.,data = litters)
best=step(model)
best$anova
base = data.frame(Rend=c(12,14,10,16,14,19,21,19,21,16,19,21,25,21),Dose=c(2,1,3,6,7,8,8,5,5,8,4,9,12,7),Temp=c(45,43,43,47,42,41,32,33,41,38,32,31,35,29),Eng=c(121,132,154,145,129,156,132,147,128,163,161,172,174,180))
base
model4=lm(Rend~.,data = base)
best=step(model4)

predict(model4,newdata = data.frame(Dose=0.75,Temp=-0.4,Eng=-0.03))

model5=lm(Rend~Dose+Temp,data = base)
best=step(model5)
