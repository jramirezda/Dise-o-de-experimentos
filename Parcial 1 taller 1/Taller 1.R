#Taller diseño
0.3360*sqrt(12)
qt(0.95,11)
qt(1-0.034,11)

type1=c(65,81,57,66,82,82,67,59,75,70)
type2=c(64,71,83,59,65,56,69,74,82,79)

var.test(type1,type2)

t.test(type1,type2,var.equal = TRUE)

library(pwr)
# Potencia de la prueba
potencia <- power.t.test(n = 20, delta = mean(type1) - mean(type2), sd = sd(type1), sig.level = 0.05, power = NULL)

# Tamaño de muestra necesario para una potencia de al menos 0.90
tamaño_muestra <- power.t.test(n = NULL, delta = 1, sd = sd(type1), sig.level = 0.05, power = 0.90)$n

potencia

tamaño_muestra

# Supongamos que la diferencia real en los tiempos medios de combustión es de 2 minutos
diferencia_real <- 2

# Estimación de la desviación estándar combinada de las dos muestras
sd_combinada <- sqrt((sd(type1)^2 + sd(type2)^2) / 2)

# Potencia de la prueba
potencia <- power.t.test(n = length(type1), delta = diferencia_real, sd = sd_combinada, sig.level = 0.05, power = NULL)

potencia

#10
library(MASS)

# Definir la matriz de diseño X
X <- matrix(c(1, 1, 1, 1, 1, 1,
              1, 1, 0, 0, 0, 0,
              0, 0, 1, 0, 0, 0,
              0, 0, 0, 1, 1, 1), nrow = 6, ncol = 4)

# Definir el vector respuesta Y
Y <- c(3, 2, 9, 10, 3, 1)

# Ajustar el modelo lineal
modelo <- lm(Y ~ X - 1)

# Estimación de la varianza residual
sigma_cuadrado_estimado <- summary(modelo)$sigma^2

# Matriz X transpuesta
Xt <- t(X)

# Matriz A
A <- Xt %*% X

# Pseudo-inversa de A
B <- ginv(A)

# Vectores de contraste para la hipótesis
v2 <- matrix(c(0, 0, 2, 0, -1, 1, -1, -1), nrow = 2, ncol = 4)

# Producto v2' * C
H_2 <- v2 %*% B %*% t(v2)

# Estimación de los coeficientes
beta <- B %*% t(X) %*% Y

# Estimación de Y
Y_est <- X %*% beta

# Suma de cuadrados del error
sce <- sum((Y - Y_est)^2)

# Cuadrado medio del error
cme <- sce / 3

# Estadístico t
t <- (v2 %*% beta) / sqrt(diag(H_2) * cme)

# Mostrar el resultado
print(t)

qt(1-0.025,3)



#16 
# Crear la matriz
matriz <- matrix(c(2, 4, 6,
                   3, 2, 7,
                   2, 5, 8), nrow = 3, byrow = TRUE)

valores=c(2, 4, 6,
          3, 2, 7,
          2, 5, 8)
mean(valores)
matriz <- matrix(c( 4.333,2, 4, 6,
                    4.333,3, 2, 7,
                    4.333,2, 5, 8), nrow = 3, byrow = TRUE)
# Asignar nombres a las columnas
colnames(matriz) <- c("mu","T_1", "T_2", "T_3")

# Mostrar la matriz
print(matriz)

A=t(matriz)%*%matriz;A
solve(A)
det(A)

# Obtener la submatriz excluyendo la primera fila y la primera columna
submatriz1 <- A[-1, -1]
print(submatriz1)
A1 <- t(submatriz1) %*% submatriz1
inv1 <- solve(A1)

# Agregar una nueva fila de ceros en la primera fila y columna de la submatriz1
inv1 <- rbind(c(0, rep(0, ncol(inv1))), cbind(0, inv1))
inv1
# Obtener la submatriz excluyendo la última fila y la última columna
submatriz2 <- A[-4, -4]
print(submatriz2)
A2 <- t(submatriz2) %*% submatriz2
inv2 <- solve(A2)

# Agregar una nueva fila de ceros en la última fila y columna de la submatriz2
inv2 <- cbind(inv2,rep(0, nrow(inv2)+1))
inv2 <- rbind(inv2,rep(0, nrow(inv2)+1))
inv2

