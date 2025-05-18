##,Parcial,1,Jhon,Ramirez
sin_aditivo,<-c(20,31,16,22,19,32,25,18,20,19)
con_aditivo,<-c(23,34,15,21,22,31,29,20,24,23)
#test,de,varianzas
var.test(sin_aditivo,con_aditivo)
?t.test
#test de igualdad de medias 
t.test(sin_aditivo,con_aditivo,var.equal,=,TRUE)
#Test de igualdad de medias no parametricas
wilcox.test(sin_aditivo,con_aditivo)

#Punto,2
con_tratamiento<-c(5.3,2.2,4.0,1.1,4.0,2.0,4.0,3.0,2.6,3.1,2.1,2.1,5.1,1.2,4.1,3.3,4.1,2.1,3.2,4.0,5.1,2.0,2.2,3.0,4.1)
sin_tratamiento<-c(8.0,8.7,13.2,11.3,7.2,4.5,8.2,6.6,9.1,9.2,6.7,10.2,12.2,10.6,16.3,13.3,9.2,5.2,6.4,6.2,7.2,8.0,17.2,4.8)
#test de varianzas
var.test(con_tratamiento,sin_tratamiento)

# test de igualdad de medias 
t.test(con_tratamiento,sin_tratamiento)

#test de igualdad de medias no parametrico 
wilcox.test(con_tratamiento,sin_tratamiento)


## Punto 3
T3=c(94.09,90.45,99.38,73.56,74.39,98.81,103.55,115.23,129.06,117.61,197.18,207.31,177.50,226.05,222.74,102.93,117.51,119.92,112.01,101.10,83.14,89.59,87.76,96.43,82.94)

mu=rep(1,25)
P=rep(1,25)
A=c(rep(0,5),rep(1,20))
g60=c(rep(0,10),rep(1,15))
g80=c(rep(0,15),rep(1,10))
Mm=c(rep(0,20),rep(1,5))

X=cbind(mu,P,A,g60,g80,Mm)
X

Xt=t(X)

XtX=Xt%*%X
solve(XtX)
W=XtX[-1, -1]
W_i=solve(W)
igXtX=rbind(c(0, rep(0, ncol(W_i))), cbind(0, W_i))
igXtX
theta=igXtX%*%Xt%*%T3
theta
T3_est=X%*%theta
T3_est

e=T3-T3_est
e

# Calcular la suma total de los cuadrados
SST <- sum((T3 - mean(T3))^2)

# Calcular la suma de los cuadrados del modelo
SSM <- sum((T3_est - mean(T3))^2)

# Calcular la suma de los cuadrados del error
SSE <- sum(e^2)

# Calcular los grados de libertad
df_modelo <- length(theta) - 1
df_error <- length(T3) - length(theta)
df_total <- length(T3) - 1

# Calcular las medias de los cuadrados
MS_modelo <- SSM / df_modelo
MS_error <- SSE / df_error

# Calcular el valor F
F_value <- MS_modelo / MS_error

# Calcular el coeficiente de determinación
R_cuadrado_ajustado <- 1 - (SSE / df_error) / (SST / df_total)

# Imprimir resultados
cat("Análisis de Varianza:\n")
cat("Causa de variación\tGrados de libertad\tSumas de cuadrados\tCuadrados medios\tValor F\n")
cat("Modelo\t", df_modelo, "\t", SSM, "\t", MS_modelo, "\t", F_value, "\n")
cat("Error\t", df_error, "\t", SSE, "\t", MS_error, "\n")
cat("Total\t", df_total, "\t", SST, "\n\n")

cat("Coeficiente de Determinación: ", R_cuadrado_ajustado, "\n")

qf(0.95,5,19)


# generadora 
Gen=igXtX%*%XtX
Gen=round(Gen)
Gen

lambda1=c(0,4,-1,-1,-1,-1)
lambda2=c(0,0,1,-1,0,0)
lambda3=c(0,1,1,-2,0,5)

lambda1%*%Gen
lambda2%*%Gen
lambda3%*%Gen


## Prueba 2 
-1*t(lambda2)%*%theta/(sqrt(t(lambda2)%*%igXtX%*%lambda2*MS_error))
qt(0.025,24)
