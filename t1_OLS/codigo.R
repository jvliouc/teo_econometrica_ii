#------------------------------------------------------------------------------#
# ESTE PASO SE LO PUEDE SALTAR, el archivo estará en el repositorio como csv
#------------------------------------------------------------------------------#
#En primer lugar es necesario simular los datos, en general estos datos vienen en
#algun formato .xls .dta
set.seed(1)
N=5000 #número de observaciones 
#W,u,v~N(0,1) y son independientes

#rnorm
W<-rnorm(N,mean=0,sd=1)
u<-rnorm(N,mean=0,sd=1)
v<-rnorm(N,mean=0,sd=1)
eta<-rnorm(N,mean=0,sd=1)
#Ahora vamos a simular los datos verdaderos
X<-0.8*W+v #existe correlación entre X y W, por ejemplo skills y años de estudio
Y<-1+2*X+3*W+u
X_medicion<-X+eta
# Si bien no es necesario voy a crear el dataframe para exportarlo y despues 
#hacer el mismo ejercicio con otras herramientas

datos<-data.frame(
  Y=c(Y),
  X=c(X),
  W=c(W),
  X_obs=c(X_medicion)
)
# voy a exportarlo, no es necesario que usted lo haga, subiré el archivo al repositorio
# si insiste en repetir todos los pasos, recuerde verificar que se esta trabajando en la carpeta adecuada
write.csv(datos,"ejercicio1.csv",row.names = FALSE)
#------------------------------------------------------------------------------#
#simulación terminada
#------------------------------------------------------------------------------#

#b)Trabajando desdela base de datos, voy a importar los datos:
library(readr)
ejercicio1 <- read_csv("ejercicio1.csv")
#notar que tienen el mismo nombre de variables

#Estime $Y$ sobre $X$ y luego $Y$ sobre $X, W$.
#Compare ambos coeficientes de $X$ con el valor verdadero.

#OLS en R no puede ser más sencillo, es simplemente usar la función lm()
#caso 1 Estime $Y$ sobre $X$: Busca dejar en evidencia que una variable omitida 
#en la regresión, produce un sesgo en el estimador de X, por lo tanto, no se puede
#inferir causalidad de X sobre Y, dado que esta varianza es capturada por el error y por el estimador de X
modelo1<-lm(Y~X, data=ejercicio1)
summary(modelo1)
# Beta del regresor X vale: 3.44611

#caso 2: $Y$ sobre $X, W$

modelo2<-lm(Y~X+W, data=ejercicio1)
summary(modelo2)





