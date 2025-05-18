# Crear los factores
temperatura <- factor(rep(c(200, 225, 250), each = 3))
metodo <- factor(rep(1:3, times = 3))
replica <- factor(rep(c("I", "II"), each = 9))

# Datos de la tabla
resistencia <- c(30, 34, 29, 35, 41, 26, 37, 38, 33, # Replica I
                 28, 35, 32, 36, 37, 34, 32, 41, 49) # Replica II

# Crear un data frame
datos <- data.frame(Temperatura = temperatura,
                    Metodo = metodo,
                    Replica = replica,
                    Resistencia = resistencia)

# Modelo lineal con interacciones y confusión parcial
modelo <- lm(Resistencia ~ Temperatura + Metodo + Replica, data = datos)

# Resumen del modelo
summary(modelo)
summary(aov(modelo))

library(Rcmdr)
# Crear el gráfico con medias y barras de error (desviación estándar)
with(datos, plotMeans(Resistencia, Metodo, Temperatura, error.bars = "sd", 
                      connect = TRUE, xlab = 'Método', ylab = 'Resistencia promedio', main = 'Resistencia vs. Método y Temperatura'))

library(car)
qqPlot(modelo$residuals)
par(mfrow=c(2,2))
plot(modelo)

library(lmtest)
bptest(modelo)
library(nortest)
ad.test(modelo$residuals)





###3
aleacion <- factor(rep(c(1, 2, 3), each = 6))
resistencia <- c(37.5, 40.5, 49.0, 51.0, 61.5, 63.0,
                 57.5, 69.5, 87.0, 92.0, 107.0, 119.5,
                 38.0, 44.5, 53.0, 55.0, 58.0, 58.5)
diametro <- c(12.5, 14.0, 16.0, 15.0, 18.0, 19.5,
              16.5, 17.5, 19.0, 19.5, 24.0, 22.5,
              15.5, 16.0, 19.0, 18.0, 19.0, 20.5)

# Combinar en un dataframe
datos <- data.frame(Aleacion = aleacion, Resistencia = resistencia, Diametro = diametro)
lm2 <- lm(Resistencia ~ Aleacion+I(Diametro-mean(Diametro))%in%Aleacion,contrasts=list(Aleacion="contr.sum"),data=datos) 
summary(lm2)
anova(lm2)
# Modelos ajustado con pendiente constante
lm3 <- lm(Resistencia ~ I(Diametro-mean(Diametro))+Aleacion,contrasts=list(Aleacion="contr.sum"),data=datos) 
summary(lm3)
anova(lm3)
# Prueba de igualdad de betas a una constante
anova(lm3,lm2)



###4
# Datos del experimento factorial 2^3
A <- rep(c(-1, 1), each = 4, times = 3) # Niveles del factor A (velocidad de corte)
B <- rep(rep(c(-1, 1), each = 2), times = 3) # Niveles del factor B (geometría de la herramienta)
C <- rep(c(-1, 1), times = 12) # Niveles del factor C (ángulo de corte)
Replica <- rep(1:3, each = 8) # Tres réplicas

# Resultados de vida de la herramienta en horas para cada combinación
vida <- c(22, 31, 25, 32, 43, 29, 35, 34, 50, 55, 47, 46, 
          44, 45, 38, 40, 37, 36, 60, 50, 54, 39, 41, 47)

# Crear un dataframe con las variables
datos <- data.frame(A = factor(A), B = factor(B), C = factor(C), Replica = factor(Replica), Vida = vida)

# Ajustar el modelo factorial completo
modelo <- aov(Vida ~ A * B * C , data = datos)

# Resumen del modelo
summary(modelo)
summary(lm(modelo))

estimaciones <- coef(modelo)
estimaciones_A <- estimaciones["A1"]
estimaciones_B <- estimaciones["B1"]
estimaciones_C <- estimaciones["C1"]
estimacion_AB <- estimaciones["A1:B1"]
estimacion_AC <- estimaciones["A1:C1"]
estimacion_BC <- estimaciones["B1:C1"]
estimacion_ABC <- estimaciones["A1:B1:C1"]

library(car)
qqPlot(modelo$residuals,)
par(mfrow=c(2,2))
plot(modelo)

library(lmtest)
bptest(modelo)
library(nortest)
ad.test(modelo$residuals)



# Calcular la suma de cuadrados del modelo y del error
ss_totales <- sum((vida - mean(vida))^2)  # Suma total de cuadrados
ss_residual <- sum(residuals(modelo)^2)   # Suma de cuadrados de los residuos

# Calcular el tamaño del efecto f
varianza_entre_grupos <- (ss_totales - ss_residual) / 7  # Grupos (8 combinaciones - 1)
varianza_residual <- ss_residual / (length(vida) - 8)  # Error

# f es el cociente entre la desviación estándar de las medias grupales y la desviación estándar residual
f_value <- sqrt(varianza_entre_grupos / varianza_residual)

cat("El valor de f (tamaño del efecto) es:", f_value, "\n")
library(pwr)
pwr.anova.test(k = 8, f = f_value, sig.level = 0.05, power = 0.80) # f es el tamaño del efecto


##5


# Datos del experimento factorial 2^3
A <- rep(c(-1, 1), each = 4, times = 3) # Niveles del factor A (velocidad de corte)
B <- rep(rep(c(-1, 1), each = 2), times = 3) # Niveles del factor B (geometría de la herramienta)
C <- rep(c(-1, 1), times = 12) # Niveles del factor C (ángulo de corte)
Replica <- rep(1:3, each = 8) # Tres réplicas

# Resultados de vida de la herramienta en horas para cada combinación
vida <- c(22, 31, 25, 32, 43, 29, 35, 34, 50, 55, 47, 46, 
          44, 45, 38, 40, 37, 36, 60, 50, 54, 39, 41, 47)

# Crear un dataframe con las variables
datos <- data.frame(A = factor(A), B = factor(B), C = factor(C), Replica = factor(Replica), Vida = vida)

# Ajustar el modelo factorial completo
modelo <- aov(Vida ~ A * B * C , data = datos)
# Convertimos los niveles de A, B, y C de factores a numéricos (-1 y 1)
A_num <- as.numeric(as.character(datos$A))
B_num <- as.numeric(as.character(datos$B))
C_num <- as.numeric(as.character(datos$C))

# Crear el bloque basado en la suma (i + j + k) mod 2
bloque <- c(1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1)

# Añadir la nueva columna 'Bloque' al dataframe original
datos$Bloque <- factor(bloque)

# Ver el nuevo orden de los datos por bloque
datos_reordenados <- datos[order(datos$Bloque), ]

# Mostrar la tabla reordenada
print(datos_reordenados)

# Ajustar el modelo factorial completo con el bloque
modelo_bloque <- aov(Vida ~ A * B * C + Bloque, data = datos_reordenados)

# Resumen del modelo
summary(modelo_bloque)
summary(lm(modelo_bloque))

estimaciones <- coef(modelo_bloque)

# Ajustar el modelo factorial completo
modelo <- aov(Vida ~ A * B * C , data = datos)

# Calcular las medias de Vida para cada combinación de A, B y C
medias <- aggregate(Vida ~ A + B + C, data = datos, FUN = mean)

ggplot(medias, aes(x = A, y = Vida, color = B, linetype = C, group = interaction(B, C))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("-1" = "blue", "1" = "red")) + # Ajustar colores para B
  scale_linetype_manual(values = c("-1" = "dotted", "1" = "solid")) + # Ajustar tipos de línea para C
  labs(x = 'Nivel de A', y = 'Vida Promedio', 
       title = 'Interacciones Dobles entre A, B y C') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Factor B"), 
         linetype = guide_legend(title = "Factor C")) # Añadir títulos a la leyenda
