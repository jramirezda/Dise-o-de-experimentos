##################
### Capitulo 6 ###
##################

#################
## Ejemplo 6.1 ##
#################

#Introducci??n de los datos del ejemplo
ejemplo6.1 <- data.frame(dieta=factor(rep(seq(1,4),5)),
		replica=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4))),
		difpeso=c(-9.3,-10.1,1.5,-3.2,2.5,-5.0,-2.0,-5.6,-5.4,-7.2,
		-0.5,-8.4,-3.6,-9.2,3.4,-5.3,1.7,-8.1,-2.5,-1.4))

anova6.1 <- lm(difpeso ~ dieta,contrasts=list(dieta="contr.sum"),data=ejemplo6.1)
summary(anova6.1)
summary(aov(anova6.1))
boxplot(ejemplo6.1$difpeso ~ ejemplo6.1$dieta)

# desviaciC3n estC!ndar de los residuales
sd <- sd(anova6.1$residuals) 
# Residuales estudentizados
estresiduals <- anova6.1$residuals/sd 
# Concatena los dos vectores de residuales y residuales estudentizados
resi <- cbind(anova6.1$residuals,estresiduals) 
# Nombra las columnas correspondientes
colnames(resi) <- c("Resid", "Resid.Est") 

# pruebas de comparaciones mC:ltiples

library(agricolae)
# Prueba de Bonferroni
pairwise.t.test(ejemplo6.1$difpeso,ejemplo6.1$dieta, p.adj = "bonf")

# Prueba de Duncan
duncan.test(anova6.1,"dieta", group=TRUE,console=TRUE,main="Dieta")

# Prueba de Dunnett
library(multcomp)
dunnett <- glht(anova6.1, linfct = mcp(dieta = "Dunnett"))
confint(dunnett, level = 0.95)

# Prueba LSD de Fisher
LSD.test(anova6.1,"dieta", group=TRUE,console=TRUE,main="Dieta") 

# Prueba de Scheffe
scheffe.test(anova6.1,"dieta",group=TRUE,console=TRUE,main="Dieta")

# Prueba SNK   (Student-Newman-Keuls)
SNK.test(anova6.1,"dieta",group=TRUE,console=TRUE,main="Dieta")

# Prueba de Tukey
TukeyHSD(aov(anova6.1),"dieta",conf.level = 0.95)
plot(TukeyHSD(aov(anova6.1),"dieta",conf.level = 0.95))

# Comparaciones planeadas (tabla de ANOVA) ejemplo de las dieta

contrastes <- matrix(c(3,-1,-1,-1,0,-1,-1,2,0,1,-1,0),4,3)  
# 4 Dietas con 3 contrastes ortogonales
colnames(contrastes)<-c("3mu1-mu2-mu3-mu4=C1","-mu2-mu3+2mu4=C2",
"mu2-mu3=C3")  # nombres para los contrastes
contrasts(ejemplo6.1$dieta)<-contrastes
dca3=anova6.1
dca3$contrasts  #correr primero el modelo
summary.aov(dca3,split=list(dieta=list(C1=1,C2=2,C3=3)))


#################
## Ejemplo 6.2 ##
#################

# GrC!ficos exploratorios de los residuales

# Histograma de los residuales
hist(anova6.1$residuals, main = "Histograma de los Residuales", axes = TRUE, 
     breaks = "Sturges", col=4)
# Box plot por dietas
boxplot(anova6.1$residuals~dieta,data=ejemplo6.1, main="Diferencia de pesos por dietas", 
        xlab="dieta", ylab="diferencia de peso",col=4)
summary(aov(anova6.1$residuals~dieta,data=ejemplo6.1))

# Box plot de los residuales del modelo
boxplot(anova6.1$residuals,main="Residuales del modelo",col=4)  

# GrC!ficos de validaciC3n de supuestos sobre residuales
x11()
split.screen(c(2,2))
screen(1)
plot(anova6.1$fitted.values,anova6.1$residuals,main="Residuals vs. Fitted",
     pch=20)
# grC!fico de residuales vs valores ajustados
abline(h=0,lty=2)
screen(2)
plot(ejemplo6.1$dieta,anova6.1$residuals,main="Residuals vs. Levels")   
# grC!fico de residuales vs niveles de tratamientos
screen(3)
plot(1:20,anova6.1$residuals, main="Residuals vs. time order",pch=20)  
# gr?fico de residuales vs valores en el tiempo
abline(h=0,lty=2)
screen(4)
# gr?fico de residuales Q-Q
qqnorm(anova6.1$residuals,pch=20)  
# linea de probabilidad normal gr?fico de residuales Q-Q
qqline(anova6.1$residuals) 


# Gr?fico Q-Q con bandas de confianza
library(car)
qqPlot(anova6.1$residuals)
# Todos los gr?ficos cl?sicos en uno
par(mfrow=c(2,2))
plot(anova6.1)

#################
## Ejemplo 6.3 ##
#################

library(olsrr)
ols_plot_cooksd_bar(anova6.1)
ols_plot_cooksd_chart(anova6.1)
ols_plot_dfbetas(anova6.1)
ols_plot_dffits(anova6.1)
ols_plot_resid_stud(anova6.1)
ols_plot_resid_stand(anova6.1)
ols_plot_resid_lev(anova6.1)
ols_plot_resid_stud_fit(anova6.1)


# Test de homocedasticidad o Homogeneidad de varianzas Bartlett
bartlett.test(anova6.1$residuals~dieta,data=ejemplo6.1)

#################
## Ejemplo 6.4 ##
#################

# libreria para la prueba de Levene
library(lawstat) 
# libreria para poder hacer la prueba de LEVENE   
library(car)     
# libreria para la prueba de Levene requerida
library(VGAM)    

# Test de homocedasticidad o Homogeneidad de varianzas Levene
levene.test(anova6.1$residuals, ejemplo6.1$dieta)   
leveneTest(anova6.1$residuals~dieta,center=median,data=ejemplo6.1)
leveneTest(anova6.1$residuals~dieta,center=mean,data=ejemplo6.1)

ols_test_breusch_pagan(anova6.1)

oneway.test(rstudent(anova6.1)~dieta,data=ejemplo6.1)
fligner.test(rstudent(anova6.1)~dieta,data=ejemplo6.1)

#################
## Ejemplo 6.5 ##
#################

# Prueba de Normalidad de Kolmogorov-Smirnov
n.q <- (anova6.1$residuals-mean(anova6.1$residuals))/sd(anova6.1$residuals)
# Lista con las posiciones de los datos ordenados
n.o <- order(n.q)
# Vector de cuantiles estandarizados y ordenados.  
n.qo <- n.q[n.o] 
# Prueba K-S para saber si los datos provienen de una normal
ks.test(n.qo, pnorm)
# forma alternativa para el test de K-S
ks.test(anova6.1$residuals,"pnorm",mean(anova6.1$residuals),
        sd(anova6.1$residuals))

#################
## Ejemplo 6.6 ##
#################

library(car)
qqPlot(resid(anova6.1))

# Prueba de Normalidad de Shapiro-Wilk
shapiro.test(anova6.1$residuals)
shapiro.test(rstudent(anova6.1))

# Prueba de Normalidad de Jarque-Bera
library(tseries)
jarque.bera.test(anova6.1$residuals)

# Prueba de Normalidad de Anderson-Darling
library(nortest)
ad.test(anova6.1$residuals)
# Prueba de Normalidad de Cramer-von Mises
cvm.test(anova6.1$residuals)
# Prueba de Normalidad de Lilliefors
lillie.test(anova6.1$residuals) 

ols_test_normality(anova6.1)
ols_test_normality(rstudent(anova6.1))

## GrC!fico de probabilidad normal
plot(density(anova6.1$residuals),main="Gr?fico de Probabilidad Normal")

#################
## Ejemplo 6.9 ##
#################

ejemplo6.9<-c(39.3,3.5,6.0,2.7,7.4,3.5,19.4,19.7,1.0,8.7,14.8,8.3,17.1,26.2,6.6,8.3,
19.0,10.3,7.6,18.9,6.3,10,16.8,24.3,5.2,44.8,14.1,3.4,28.3,3.4,0.9,1.3,0.7,17.7,
8.3,8.3,1.9,16.7,26.2,10.0,6.5,7.1,7.9,3.2,5.9,13.4,12.0,4.3,31.7) 

# Verificaci?n de normalidad con los datos originales
ks.test(ejemplo6.9, "pnorm", mean(ejemplo6.9), sd(ejemplo6.9))
shapiro.test(ejemplo6.9)

# datos transformados con la potencia Z=Y^1/4
Z<-ejemplo6.9^(1/4)

# Verificaci?n de normalidad con los datos transformados

ks.test(Z, "pnorm", mean(Z), sd(Z))
shapiro.test(Z)

library(MASS)
boxcox(I(difpeso+11) ~ dieta,data=ejemplo6.1,lambda = seq(-1.25, 2.25, length = 20))

library(lmtest)
dwtest(anova6.1)

library(car)
durbinWatsonTest(anova6.1,max.lag = 11)

# FuncciC3n de AutocorrelacciC3n
cr <- acf(rstudent(anova6.1), lag = 18)
plot(cr, type = "o")

# FuncciC3n de AutocorrelacciC3n parcial
pacf(rstudent(anova6.1), lag = 44,pl = TRUE)

vif(lm(anova6.1))

