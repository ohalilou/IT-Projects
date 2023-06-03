#nom: HALILOUA Othmane, Groupe B1
library(MASS)

data=read.table(file="Data.txt",header=TRUE)

RMSE=function(obs,pr){
return(sqrt(mean((pr-obs)^2)))}

# Choix automatique de predicteurs a partir du modele complet sans puis avec interactions
regbic=stepAIC(lm(O3~.,data),k=log(nrow(data)),trace=0)
regbicint=stepAIC(lm(O3~.*.,data),k=log(nrow(data)),trace=0)


k=100

tab=matrix(nrow=k,ncol=7)

for (i in 1:k) {

nappr=ceiling(0.8*nrow(data))
ii=sample(1:nrow(data),nappr)
jj=setdiff(1:nrow(data),ii)
datatest=data[jj,]
datapp=data[ii,]

# Estimation des modeles
regsimple=lm(O3~O3v,datapp)
regbic=lm(formula(regbic),datapp)
regbicint=lm(formula(regbicint),datapp)

# Scores sur apprentissage
tab[i,1]=RMSE(datapp$O3o,predict(regsimple))
tab[i,2]=RMSE(datapp$O3o,predict(regbic))
tab[i,3]=RMSE(datapp$O3o,predict(regbicint))

# Scores sur test
tab[i,5]=RMSE(datatest$O3o,predict(regsimple,datatest))
tab[i,6]=RMSE(datatest$O3o,predict(regbic,datatest))
tab[i,7]=RMSE(datatest$O3o,predict(regbicint,datatest))

}

x11()
boxplot(tab,col=c(rep("blue",4),rep("red",4)),xlab="bleu=apprentissage - rouge=test",
        names=c("ASsimp","ASbic","ASbicint","ASsimp","ASbic","ASbicint"),main="Modele lineaire gaussien - Score RMSE")
