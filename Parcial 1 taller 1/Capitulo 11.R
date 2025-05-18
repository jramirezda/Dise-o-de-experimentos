###################
### Capitulo 11 ###
###################

##################
## Ejemplo 11.1 ##
##################

ejemplo11.1 <- data.frame(A=factor(c(-1, 1, -1, 1, -1, 1, -1,1)),
		B=factor(c(-1, -1, 1, 1, -1, -1, 1,1)),
		C=factor(c(-1, -1, -1, -1, 1, 1, 1,1)),
		D=factor(c(-1,1,1,-1,1,-1,-1,1)),
		rendimiento=c(7,10,32,55,18,20,40,61))

modelo11.1 <-aov(rendimiento~A*B+C,data=ejemplo11.1)
summary(modelo11.1)
summary(lm(modelo11.1))

##################
## Ejemplo 11.2 ##
##################

ejemplo11.2 <- data.frame(A=factor(c(-1, 1, -1, 1, -1, 1, -1,1,1, -1, 1, -1, 1, -1, 1,-1)),
		B=factor(c(-1, -1, 1, 1, -1, -1, 1,1, 1, 1, -1, -1, 1, 1, -1,-1)),
		C=factor(c(-1, -1, -1, -1, 1, 1, 1,1,1, 1, 1, 1, -1, -1, -1,-1)),
		D=factor(c(-1,1,1,-1,1,-1,-1,1,1,-1,-1,1,-1,1,1,-1)),
		E=factor(c(1,-1,-1,1,1,-1,-1,1,-1,1,1,-1,-1,1,1,-1)),
		F=factor(c(1,-1,1,-1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1)),
		G=factor(c(1,1,-1,-1,-1,-1,1,1,-1,-1,1,1,1,1,-1,-1)),
		bloque=factor(rep(c(rep(1,8),rep(2,8)))),
		tiempofiltro=c(68.4,77.7,66.4,81,78.6,41.2,68.7,38.7,66.7,65,86.4,61.9,47.8,59,42.6,67.6))

modelo11.2 <-aov(tiempofiltro~A*F+bloque,data=ejemplo11.2)
summary(modelo11.2)
summary(lm(modelo11.2))

##################
## Ejemplo 11.3 ##
##################

ejemplo11.3 <- data.frame(replica=factor(rep(c(rep(1,6),rep(2,6)))),
		variedad=factor(c(rep(1,3),rep(2,3),rep(1,3),rep(2,3))),
		fecha=factor(c(1,2,3,3,1,2,1,3,2,2,3,1)),
		parcela=factor(c(1,1,1,2,2,2,3,3,3,4,4,4)),
		produccion=c(5.0,5.3,5.8,6.5,5.7,6.0,5.4,6.4,5.9,6.5,6.6,5.8))

modelo11.3 <-aov(produccion~replica+variedad*fecha+Error(parcela),
                 data=ejemplo11.3)
summary(modelo11.3)

interaction.plot(ejemplo11.3$fecha,ejemplo11.3$variedad,ejemplo11.3$produccion,
     col=1,xlab='Fecha',ylab='Producci?n promedio',leg.bty='o',cex=1.5)

# Grados de libertad del error parcela principal
gl.error.a<-1    
# Grados de libertad del error suparcela
gl.error.b <-modelo11.3$Within$df.residual 
# Residuos
x <-modelo11.3$parcela$residuals  
y <-modelo11.3$Within$residuals
# Suma de cuadrados del error parcela principal
sc.error.a <-sum(x^2)       
# Suma de cuadrados del error subparcela
sc.error.b <-sum(y^2)      
# Cuadrado medio del error parcela principal 
cm.a <-sc.error.a          
# Cuadrado medio del error de subparcela
cm.b <-sc.error.b/gl.error.b   

library(agricolae)
# Comparaci?n m?ltiple de Honestly Significant Difference 
HSD.test(ejemplo11.3$produccion,ejemplo11.3$fecha,gl.error.b,cm.b, 
         group=TRUE,console=TRUE)

