#NOM: HALILOUA Othmane, groupe B1
rm(list=ls())

#1
#chargement des données
data=read.table(file="Data.txt",header=TRUE)


#forcage dy type des variable DD et RR
data$DD=as.factor(data$DD)
data$RR=as.factor(data$RR)

names(data)
summary(data)
dim(data)

#2
lm.out1 = lm(O3~O3v,data=data)

#trace du nuage des points correspondant au modele ainsi que la droite de regression
plot(data$O3,data$O3v,xlab="O3 du jour J-1",ylab="O3 du jour J", main="PREVISIONS 24H",pch='+')
abline(lm.out1,col="red")

summary(lm.out1)

#un summary sur le modele lm.out1 met en evidence la p_value de O3v qui est bien inferieure a 0.05
#ce qui indique que l'impact de O3v est significatif sur la valeur maximale de concentration d'ozone
#d'autre part on constate que le RSE est de 20.64 c'est a dire que l'estimation de la variance du terme d'erreur est bien importante
#ce qui parle de la performance non pertinente du modèle, en plus de ca ce modle ne représente que 46.86% de la variabilite de la valeur max de la concentration
#de l'ozone, ce qui confirme que le modele n'est pas performant


#3

datasec = subset(data,RR=="Sec")
datapluie = subset(data,RR=="Pluie")
#boxplot(aix$O3o,aix$O3p,names=c("[O3]obs","[O3]prevu"))

#un summary sur les deux sous echantillons montre que l'echantillon de la modalite "Pluie" présente des valeurs de O3 puis faible
# en comparaison avec celle dans l'échnatillon de la modalite "Sec" à une difference significative, la meme remarque se fait les moyennes de O3 selon
# les deux modalite ce qui nous ramene a juger intuitivement que la variable qualitative RR est bien significative en terme de son impact sur la valeur
# de la concentration de l'ozone

lm.out2 = lm(O3~O3v+RR+DD+N+T+FF,data=data)
summary(lm.out2)
#un summary sur lm.out2 nous renseigne sur une p_value de la variable N qui est de 0.000285 qui est bien inferieure a 0.05
# donc l'ecart de cette variable de -2.20773 est juge a impact significatif sur la valeur de la concentration maximale de l'ozone

#R a pris comme reference pour la variable DD la modalite Est, la p_value des modalité Nord et Sud sont environ respecrtivement de 0.04 et 0.03 
#qui sont <0.05 ce qui prédit leur ecarts de -13.92 et 14.45 impact respec soient a impact  significatif sur
#la concentration de l'ozone, pourtant;, la modalite Ouest a une p-value bien superieure a 0.05 
#son ecart estime a -4.216 est juge non significatif, mais on ne peut la dissocier des autre modalites derivees de la variable DD
#en conclusion, les directions ouest et est sont de comportement similaires, alors que la direction sud et nord présentent par rapport à la
#référence prise un biais.

#la verification des hypotheses du cadre theorique:

#homoscedasticite:
plot(fitted(lm.out2),residuals(lm.out2),main="Hypoth?se d'homosc?dasticit?",xlab="Valeurs ajust?es (Y*)",ylab="R?sidus")
#=>  heteroscedasticite : la variabilite de l'erreur n'est pas stable, une structure de points eparpillés dans tous les sens apparait, 
#hypothese d'homoscedasticite non verifiee


#normalite:
qqnorm(residuals(lm.out2))
hist(residuals(lm.out2))
#=> residus centres par construction sur apprentissage,
# les queues de distribution s'eloignent de la normalite, mais globalement l'hypoth?se de normalite ne pose pas probleme sur une large plage


#independance:
acf(residuals(lm.out2))
#=> autocorrelation marquee pendant une bonne journee, moins nette ensuite, bien que le reste soit compris dans la bande, hypoth?se d'independance non respectee

#en constatant les p_value des variable du modele lm.out2 on conclue qu'on va conserver les predicteurs T, N, DD, et O3v, en se debarasse de RR et FF 

library(MASS)
#estimation en exploitant l'indice BIC
lm.outBIC=stepAIC(lm.out2,k=log(nrow(data)))
formula(lm.outBIC)
#le modèle obtenu en exploitant BIC est le meme que celui conclue avec la selection manuelle dans la question precedente


#4
lm.outint=lm(O3~.*.,data)
lm.outBICint=stepAIC(lm.outint,k=log(nrow(data)))
formula(lm.outBICint)
#la dimension du modele testant les interaction d'ordre 2 est 7 les predicteur pris sont T, N, FF, O3v, DD et T:FF
#un modele plus complique


#5
x11()
plot(data$O3,type ="l",lwd=2,main="Concentration d'ozone",xlab="Date",ylab="[O3]")
points(fitted(lm.out1),col="red",pch="+")
points(fitted(lm.outBICint),col="blue",pch="+")
legend(0,268,lty=1,col=c("black"),legend=c("observ?e"),bty="n")
legend(0,256,pch="+",col=c("red","blue"),legend=c("       lmout1","       lmoutBICint"),bty="n") 
# -> le traitement statistique avrc BIC tire donne des prévisions tres proches des valeur veritablement observe de la concentration de l'ozone
# contrairement au traitement statistique du modele lm.out1 qui est bien moins performant avec des prévisions lointaines des valeurs observés

#6
nappr=ceiling(0.8*nrow(data))
ii=sample(1:nrow(data),nappr)
jj=setdiff(1:nrow(data),ii)
datatest=data[jj,]
datapp=data[ii,]

lm.out1=lm(O3~O3v,datapp)
lm.out2 = lm(O3~O3v+RR+DD+N+T+FF,data=datapp)
lm.outBIC=stepAIC(lm.out2,k=log(nrow(datapp)))
lm.outint=lm(O3~.*.,datapp)
lm.outBICint=stepAIC(lm.outint,k=log(nrow(datapp)))

score=function(obs,prev) {
  rmse=sqrt(mean((prev-obs)**2))
  biais=mean(prev-obs)
  print("Biais  RMSE") 
  return(round(c(biais,rmse),3))
}
######################## sur les donnes d'apprentissage#########################
score(datapp$O3,fitted(lm.out1))
# biais=0 RMSE=19.824
# -> la regression simple debiaise la prevision brute et reduit la variabilite de l'erreur
score(datapp$O3,fitted(lm.outBIC))
# biais=0 RMSE=12.161
# -> la regression simple debiaise la prevision brute et reduit la variabilite de l'erreur
score(datapp$O3,fitted(lm.outBICint))
# biais=0 RMSE=12.161
# -> la regression BIC reduit encore la variabilite de l'erreur
# -> la regression BICint reduit encore la variabilite de l'erreur, le modele le plus complexe etant sans surprise le plus performant sur les donnees
#d'apprentissage


############################### sur les donnes de test ################################"
score(datatest$O3,predict(lm.out1,datatest))
# biais=-12.056 RMSE=22.590
score(datatest$O3,predict(lm.outBIC,datatest))
# biais=-2.989 RMSE=10.915
score(datatest$O3,predict(lm.outBICint,datatest))
# biais=55.318 RMSE=200.466


######################################"
source("CV.R")




