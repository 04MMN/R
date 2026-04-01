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
