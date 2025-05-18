###################
### Capitulo 10 ###
###################

##################
## Ejemplo 10.1 ##
##################

# Introducción de los datos del ejemplo 10.1

ejemplo10.1 <- data.frame(rep=factor(rep(c(1,1,2,2,3,3),4)),
		bloque=factor(rep(seq(1,6,by=1),4)),
		a=factor(c(0,0,1,0,0,1,0,0,1,1,1,0,1,1,0,0,0,0,1,1,0,1,1,1)),
		b=factor(c(0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,1,0,0,1,1,0,1,1,1)),
		c=factor(c(1,0,1,1,1,0,0,1,0,0,1,0,0,1,1,0,0,1,1,0,0,1,0,1)),
		pureza=c(44.5,46.8,49.8,55.5,53.2,69.5,
		44.2,44.5,52.0,59.8,57.2,62.8,
		60.1,57.0,48.8,56.0,56.0,55.0,
		48.8,58.5,51.5,58.5,59.0,53.8))

anova10.0 <- aov(pureza ~ a*b*c,data=ejemplo10.1)
summary(anova10.0)
summary(lm(anova10.0))

## Obtención del análisis de varianza con sólo bloques
anova10.1 <- aov(pureza ~ bloque+a+b+a*b+c+a*c+b*c,data=ejemplo10.1)
summary(anova10.1)
summary(lm(anova10.1))

anova10.1.1 <- lm(pureza ~ bloque+a+b+a*b+c+a*c+b*c,contrasts=list(a="contr.sum",
                          b="contr.sum",c="contr.sum"),data=ejemplo10.1)
summary(anova10.1.1)
summary(aov(anova10.1.1))

## Obtención del análisis de varianza con réplicas y bloques dentro de réplica
anova10.1.2 <- aov(pureza ~ rep+bloque/rep+a+b+a*b+c+a*c+b*c,data=ejemplo10.1)
summary(anova10.1.2)

anova10.1.3 <- aov(pureza ~ a+b+a*b+c+a*c+b*c+rep+a:b:c+Error(rep(bloque)),
                   data=ejemplo10.1)
summary(anova10.1.3)

##################
## Ejemplo 10.2 ##
##################

# Introducción de los datos del ejemplo 10.2

ejemplo10.2 <- data.frame(temperatura=factor(rep(c(20,20,20,30,30,30,40,40,40),3)),
		sacarosa=factor(c(rep(20,9),rep(40,9),rep(60,9))),
		bloque=factor(c(1,1,1,2,2,2,3,3,3,3,3,3,1,1,1,2,2,2,2,2,2,3,3,3,1,1,1)),
		T1S1=factor(c(1,1,1,2,2,2,3,3,3,2,2,2,3,3,3,1,1,1,3,3,3,1,1,1,2,2,2)),
		replica=factor(c(rep(c(1,2,3),9))),
		energia=c(3.1,3.7,4.7,6.0,6.9,7.5,7.7,8.3,9.5,5.5,6.7,7.3,11.5,12.9,
		13.4,15.7,14.3,15.9,7.9,9.2,9.3,17.5,15.8,14.7,19.1,18.0,19.9))

anova10.2 <- aov(energia ~ bloque+temperatura+sacarosa+T1S1,data=ejemplo10.2)
summary(anova10.2)

##################
## Ejemplo 10.3 ##
##################

# Introducción de los datos del ejemplo 10.3

ejemplo10.3 <- data.frame(rep=factor(rep(c(1,1,2,2,3,3,4,4),4)),
		bloque=factor(rep(seq(1,8,by=1),4)),
		a=factor(c(0,0,1,0,1,1,1,0,1,1,0,1,0,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1)),
		b=factor(c(0,1,1,1,0,0,1,0,1,0,0,0,0,1,0,1,0,0,1,1,1,0,0,0,1,1,0,0,1,1,1,1)),
		c=factor(c(0,0,0,1,0,1,1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,0,1,1,1,0,1,0,1,1,0,0)),
		rendimiento=c(7,24,36,31,28,31,66,11,39,31,19,36,24,19,31,29,30,21,41,30,35,
    13,21,33,27,39,30,33,26,36,25,43))

anova10.3.1 <- aov(rendimiento ~ bloque+a*b*c,data=ejemplo10.3)
summary(anova10.3.1)  
anova10.3.2 <- aov(rendimiento ~ rep+bloque/rep+a*b*c,data=ejemplo10.3)
summary(anova10.3.2)  
 