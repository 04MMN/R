
##Etude descriptive des variable de la base
##etude univarie
#variable rank
table(Salaries$rank)
str(Salaries)
barplot(table(Salaries$rank),main="diagramme en barre du rang des professeur",col = "blue")
# commentaire:
# dans cette institution son largement majoritaire
# ils constitue 67% des enseignants

#variable discipline
tab=prop.table(table(Salaries$discipline))*100
tab
pie(tab,col = c("red","yellow"),main = "diagramme circulaire des disciblines")
# commentaire:
# les enseignant des departement theorique representent 
# 45,59% et ceux qui sont dans les departement appliques
# representent 54,41% des enseignants

#variable sexe
table(Salaries$sex)
barplot(prop.table(table(Salaries$sex))*100,main = "repartition des enseignant selon le sex",
        col = 1:2)
# commentaire:
# on constate que il y'a plus enseignant homme que femme

#variable annees de service
tab=table(Salaries$yrs.service)
tab
barplot(Salaries$yrs.service)
hist(Salaries$yrs.service,main = "hisogramme des annees de service")
boxplot(Salaries$yrs.service,main="boite a moustache des annees de serice")
summary(Salaries$yrs.service)
sd(Salaries$yrs.service) # ecart type
sd(Salaries$yrs.service)/17.61 #coefficient de variation
# commentaire:
# les annees de service de c enseignant varie de 0 a 60ans avec une moyenne de 17.61
# 50% des enseignant on fais au plus 16ans d'experience
# la repartion des annees de service est asymetrique et heterogene a droite avec la presence d'une 
# annees atipique qui est 60ans


#variable salary
tab=table(Salaries$salary)
tab
barplot(Salaries$salary)
histo=hist(Salaries$salary,main = "hisogramme des salaire")
histo
boxplot(Salaries$salary,main="boite a moustache des salaire")$out
summary(Salaries$salary)
# commentsire:
# les salaire des enseignant varie entre 57800 et 231545
# 50% des enseignant on un salaire superieur o egale a 107300 correspondant
# a la mediane
# la classe modale est entre 100mil et 120mil:la plus part des enseignant on un salaire compris 
# entre 100mil et 120mil dollard. 
#cette repartition est asymetrique a droite avec la presence de 3 valeur atipique(231545,20400,205500)




#etude Bivarié 
# Variables rank et sex
tab=table(Salaries$rank,Salaries$sex)
round(100*tab/sum(tab),2)
barplot(tab,bes=T,leg=T,main = "Diagramme en tuyaux d'orgue",col = 1:3)
barplot(tab,bes=T,main = "Diagramme en barre empilées",col = 1:3)
mosaicplot(t(tab),col=1:3)
#commentaire
#les enseignants hommes sont plus nombreux et la majorité sont des prof.
#on note une disproportionnalite entre les surfaces rectangles du diagramme en mosaïque 
#ce qui laisse penser qu'il existe une dependance entre le sexe de l'enseignant et son rang
chisq.test(tab)
#il existe une association significative entre le rang et le sexe de l;enseignant au seuil
# de 5%

#variable salaire et année de service 

plot(Salaries$yrs.service,Salaries$salary,main="Nuange de points Salaires vs Année de service",
     xlab ="année de service",ylab = "Salaire")

cor.test(Salaries$yrs.service,Salaries$salary)
#on a une faible corrélation positive mais hautement significative entre le salaire
#et le nombre d'annees de servicecar p-values<5% : le salaire augmente avec le nombre d'années
#de service






##Modélisation 
## salaire=f(année de sevice)
##premiere commande
model <- lm(salary ~ yrs.service, data=Salaries)
model
summary(model)
anova(model)
#L'ordonnee a l;origine de la pente sont significative 
#differentes de zero(test hautement significative)
#le premier est estime a 99974.7$
#le second parametre estimé a 779.6 :une augmenation d'une année de service se traduit par une 
#augmentataion du salaire moyenne de 779.6$
#11.21% de la variabilité des salaires est expliquée par le model
#le test de fisher étant très significative, donc le model est pertinent 
##extraction

yrs.service.H=Salaries$yrs.service[Salaries$sex=="Male"]
yrs.service.F=Salaries$yrs.service[Salaries$sex=="Female"]

t.test(yrs.service.H, yrs.service.F,paired=FALSE, alternative="greater")

#on a une différence significative entre le nombre d'année de service et ceux des femmes 

boxplot(yrs.service.H,horizontal = T)$out
boxplot(yrs.service.F,horizontal = T)

t.test(hommes$yrs.service, femmes$yrs.service,paired=FALSE, alternative="greater")