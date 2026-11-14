#------------------------------------------------------------------------------#
#                   SIMULACION
#------------------------------------------------------------------------------#
#W,Z1,Z2,v,u son independientes entre sin ~N(0,1)
N<-5000
set.seed(1)
W<-rnorm(N,0,1)
Z1<-rnorm(N,0,1)
Z2<-rnorm(N,0,1)
v<-rnorm(N,0,1)
u<-rnorm(N,0,1)
#Modelo verdadero
X<-0.8*W+0.7*Z1+0.05*Z2+v
Y<-1+2*X+3*W+u
#W no se observa, no va en el dataframe
datos<-data.frame(
  Y=c(Y),
  X=c(X),
  Z1=c(Z1),
  Z2=c(Z2)
)
write.csv(datos,"IV.csv",row.names = FALSE)
#------------------------------------------------------------------------------#
library(readr)
IV <- read_csv("IV.csv")
#b) USAR OLS y sobre x

modelo1<-lm(Y~X,data = IV)
summary(modelo1)

# 3.17476 vs 2
# El estimador esta sesgado, porque hay una variable omitida, esto rompe el supuesto
# de exogeneidad luego no se puede hacer inferencia causal con X (la varianza de la
#variable omitida, la absorve beta y el error)

#c)

X_z1<-lm(X~Z1,data = IV)
X_z2<-lm(X~Z2,data = IV)
summary(X_z1)$fstatistic[1]
#pasa el test 1403>>10
summary(X_z2)$fstatistic[1]
# zona roja de IV debil 12>10 por apenas

#d)  
#caso 1 Z1 unico instrumento
#primer metodo 

modX<-lm(X~Z1,data = IV)
X_hat<-modX$fitted.values

mod_IV<-lm(Y~X_hat,data = IV)
summary(mod_IV)
#1.99815
#segundo metodo RECOMENDADO
library(AER)
mod_IV2<-ivreg(Y~X| Z1, data = IV)
summary(mod_IV2)
#1.99815  


#caso 2 Z1 y Z2 instrumento
#metodo directo
mod_IV12<-ivreg(Y~X|Z1+Z2, data = IV)
summary(mod_IV12)
# 2.01127 pequeño sesgo por el weak instrument