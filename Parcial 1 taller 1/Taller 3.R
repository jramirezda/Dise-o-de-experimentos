#taller 3 

####6.6
###a
# Datos del diseño factorial
data <- data.frame(
  A = c(-1, 1, -1, 1, -1, 1, -1, 1),
  B = c(-1, -1, 1, 1, -1, -1, 1, 1),
  C = c(-1, -1, -1, -1, 1, 1, 1, 1),
  Comb = c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc"),
  ReplicaI = c(22, 32, 36, 40, 43, 45, 60, 39)
)

# Calcular los efectos
model <- lm(ReplicaI ~ A * B * C, data = data)
effects <- coef(model)[-1] # excluimos el intercepto

# Convertir los efectos en un vector
effects <- as.numeric(effects)

# QQ plot
qqnorm(effects, main = "Normal Q-Q Plot of Factor Effects")
qqline(effects)

# Etiquetas para los puntos
text(qqnorm(effects)$x, qqnorm(effects)$y, labels = names(coef(model))[-1], pos = 4)


# Convertir niveles -1 y 1 a "Bajo" y "Alto"
data$A <- factor(data$A, levels = c(-1, 1), labels = c("Bajo", "Alto"))
data$B <- factor(data$B, levels = c(-1, 1), labels = c("Bajo", "Alto"))
data$C <- factor(data$C, levels = c(-1, 1), labels = c("Bajo", "Alto"))

# Crear un nuevo data.frame con la estructura deseada
nuevo_data <- data.frame(
  A = data$A,
  B = data$B,
  C = data$C,
  Comb = data$Comb,
  replica = factor(rep(1, nrow(data))), # Usar 1 como ejemplo de réplica
  tporc = data$ReplicaI
)

print(nuevo_data)
# Realizar ANOVA con el nuevo dataframe
anova_data <- aov(tporc ~ A * B * C, contrasts = list(A = "contr.sum", B = "contr.sum", C = "contr.sum"), data = nuevo_data)

# Mostrar el resumen del ANOVA
summary(anova_data)

# Mostrar el resumen del modelo lineal correspondiente
summary(lm(anova_data))

# Creación del nuevo data.frame estructurado
data_estructurado <- data.frame(
  factorA = factor(rep(c("Bajo", "Alto"), each = 4, times = 3)),
  factorB = factor(rep(c("Bajo", "Alto"), each = 2, times = 6)),
  factorC = factor(rep(c("Bajo", "Alto"), each = 1, times = 12)),
  replica = factor(rep(1:3, each = 8)),
  respuesta = c(
    22, 32, 36, 40, 43, 45, 60, 39,  # Replica I
    31, 43, 34, 47, 45, 37, 50, 41,  # Replica II
    25, 29, 50, 46, 38, 36, 54, 47   # Replica III
  )
)

# Verificar el nuevo data.frame
print(data_estructurado)

# ANOVA con el nuevo data.frame estructurado
anova_data_estructurado <- aov(respuesta ~ factorA * factorB * factorC, 
                               contrasts = list(factorA = "contr.sum", 
                                                factorB = "contr.sum", 
                                                factorC = "contr.sum"), 
                               data = data_estructurado)

# Resumen del ANOVA
summary(anova_data_estructurado)

# Resumen del modelo lineal
summary(lm(anova_data_estructurado))
residuales6.6=rstandard(anova_data_estructurado)
predchos6.6=fitted(anova_data_estructurado)

if (!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Realizar la prueba de Breusch-Pagan
# La prueba se realiza con la función 'bptest' del paquete 'lmtest'
prueba_breusch_pagan <- bptest(anova_data_estructurado)

# Mostrar los resultados de la prueba
print(prueba_breusch_pagan)

shapiro.test(residuales6.6)

##independencia 
indices <- seq_along(residuales6.6)

# Calcular el coeficiente de correlación de Spearman
cor_spearman <- cor(indices, residuales6.6, method = "spearman")

# Realizar la prueba de hipótesis para el coeficiente de correlación de Spearman
# Instalar y cargar el paquete 'Hmisc' si no está ya instalado
if (!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)

# Realizar la prueba de Spearman
prueba_spearman <- cor.test(indices, residuales6.6, method = "spearman")

# Mostrar los resultados
print(paste("Coeficiente de correlación de Spearman:", cor_spearman))
print(prueba_spearman)


##independencia 
plot( predchos6.6,residuales6.6,
     xlab = "Datos Predichos",
     ylab = "Residuos Estandarizados",
     main = "Residuos Estandarizados vs Datos Predichos",
     pch = 16)  # pch = 16 para puntos sólidos

# Añadir una línea horizontal en y = 0
abline(h = 0, col = "red", lty = 2)  # lty = 2 para línea punteada



#6.22
# Definir los factores y sus niveles
# Crear el marco de datos para los factores
factores <- data.frame(
  ALasPow =factor(c(1, 1, 1, -1, -1, -1, 1, 1, 1, -1, -1, 1, -1, -1, 1, -1)),
  BPuFre = factor(c(1, -1, 1, -1, 1, 1, -1, -1, 1, -1, -1, -1, -1, 1, 1, 1)),
  CCelSi = factor(c(1, -1, -1, -1, 1, 1, -1, 1, 1, 1, 1, 1, -1, -1, -1, -1)),
  DWriSp = factor(c(1, 1, 1, 1, -1, 1, -1, -1, 1, 1, -1, 1, -1, -1, -1, 1))
)

# Definir la respuesta del experimento (UEC)
UEC <- c(0.80, 0.81, 0.79, 0.60, 0.65, 0.55, 0.98, 0.67, 0.69, 0.56, 0.63, 0.65, 0.75, 0.72, 0.98, 0.63)

# Crear el modelo factorial completo
modelo <- aov(UEC ~ ALasPow * BPuFre * CCelSi * DWriSp,contrasts = list(ALasPow = "contr.sum", 
                                                                        BPuFre = "contr.sum", 
                                                                        CCelSi = "contr.sum",
                                                                        DWriSp = "contr.sum"), data = factores)
effects <- coef(modelo)[-1] # excluimos el intercepto

# Convertir los efectos en un vector
effects <- as.numeric(effects)

# Generar los nombres de los efectos
nombres_efectos <- names(coef(modelo))[-1]

# QQ plot
qqnorm(effects, main = "Normal Q-Q Plot of Factor Effects")
qqline(effects)

# Etiquetas para los puntos
text(qqnorm(effects)$x, qqnorm(effects)$y, labels = nombres_efectos, pos = 4)

# Resumen del modelo para obtener los efectos
summary(modelo)
summary(lm(modelo))
# Análisis de varianza (ANOVA)
anova(modelo)

residuales6.22=residuals.lm(modelo)
predchos6.22=fitted(modelo)
# Realizar la prueba de Breusch-Pagan
# La prueba se realiza con la función 'bptest' del paquete 'lmtest'
prueba_breusch_pagan <- bptest(modelo)

# Mostrar los resultados de la prueba
print(prueba_breusch_pagan)
##normalidad 

shapiro.test(residuales6.22)

qqnorm(residuales6.22, main = "Normal Q-Q Plot of Residuals")
qqline(residuales6.22)
##independencia 
indices <- seq_along(residuales6.22)

# Calcular el coeficiente de correlación de Spearman
cor_spearman <- cor(indices, residuales6.22, method = "spearman")

# Realizar la prueba de hipótesis para el coeficiente de correlación de Spearman
# Instalar y cargar el paquete 'Hmisc' si no está ya instalado
if (!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)

# Realizar la prueba de Spearman
prueba_spearman <- cor.test(indices, residuales6.22, method = "spearman")

# Mostrar los resultados
print(paste("Coeficiente de correlación de Spearman:", cor_spearman))
print(prueba_spearman)


##independencia 
plot( predchos6.22,residuales6.22,
      xlab = "Datos Predichos",
      ylab = "Residuos Estandarizados",
      main = "Residuos Estandarizados vs Datos Predichos",
      pch = 16)  # pch = 16 para puntos sólidos

# Añadir una línea horizontal en y = 0
abline(h = 0, col = "red", lty = 2)  # lty = 2 para línea punteada







###6.34
# Cargar datos en R
# Datos iniciales (respuestas y signos para cada combinación de tratamientos)
tratamientos <- data.frame(
  Response = c(45, 71, 48, NA, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96),
  A = c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1),
  B = c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1),
  C = c(-1, -1, -1, -1, 1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1),
  D = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Calcular contraste ABCD sin valor faltante
tratamientos$ABCD <- tratamientos$A * tratamientos$B * tratamientos$C * tratamientos$D
contraste_ABCD <- sum(tratamientos$Response * tratamientos$ABCD, na.rm = TRUE)

# Estimar el valor faltante
missing_value <- -1*contraste_ABCD
tratamientos$Response[4] <- missing_value

# Recalcular contrastes y efectos con el valor faltante estimado
contraste_ABCD_final <- sum(tratamientos$Response * tratamientos$ABCD)
efecto_A <- sum(tratamientos$Response * tratamientos$A) / 8
efecto_B <- sum(tratamientos$Response * tratamientos$B) / 8
efecto_C <- sum(tratamientos$Response * tratamientos$C) / 8
efecto_D <- sum(tratamientos$Response * tratamientos$D) / 8

# Resultados
cat("Contraste ABCD final:", contraste_ABCD_final, "\n")
cat("Valor faltante estimado para run 'ab':", missing_value, "\n")
cat("Efecto principal de A:", efecto_A, "\n")
cat("Efecto principal de B:", efecto_B, "\n")
cat("Efecto principal de C:", efecto_C, "\n")
cat("Efecto principal de D:", efecto_D, "\n")

modelo6.34 <- aov( Response~ A * B * C * D, data =tratamientos)

effects <- coef(modelo6.34)[-1] # excluimos el intercepto

# Convertir los efectos en un vector
effects <- as.numeric(effects)

# Generar los nombres de los efectos
nombres_efectos <- names(coef(modelo6.34))[-1]

# QQ plot
qqnorm(effects, main = "Normal Q-Q Plot of Factor Effects")
qqline(effects)

# Etiquetas para los puntos
text(qqnorm(effects)$x, qqnorm(effects)$y, labels = nombres_efectos, pos = 4)

# Resumen del modelo para obtener los efectos
summary(modelo)
summary(lm(modelo))
# Análisis de varianza (ANOVA)
anova(modelo)

residuales6.22=residuals.lm(modelo)
predchos6.22=fitted(modelo)










###7.2
data7.2 <- data.frame(
  Block = factor(rep(c("I", "II", "III", "IV"), each = 4)),
  Treatment = factor(rep(c("(1)", "a", "b", "ab"), times = 4)),
  Response = c(18.2, 27.2, 15.9, 41.0,
               18.9, 24.0, 14.5, 43.9,
               12.9, 22.4, 15.1, 36.3,
               14.4, 22.5, 14.2, 39.9)
)
# Realizar el análisis de varianza (ANOVA)
anova_result7.2 <- aov(Response ~ Treatment + Block, data = data7.2)

# Mostrar resultados del ANOVA
summary(anova_result7.2)
anova(anova_result7.2)

residuales7.2=residuals.lm(anova_result7.2)
predchos7.2=fitted(anova_result7.2)
# Realizar la prueba de Breusch-Pagan
# La prueba se realiza con la función 'bptest' del paquete 'lmtest'
prueba_breusch_pagan <- bptest(anova_result7.2)

# Mostrar los resultados de la prueba
print(prueba_breusch_pagan)
##normalidad 

shapiro.test(residuales7.2)

qqnorm(residuales7.2, main = "Normal Q-Q Plot of Residuals")
qqline(residuales7.2)
##independencia 
indices <- seq_along(residuales7.2)

# Calcular el coeficiente de correlación de Spearman
cor_spearman <- cor(indices, residuales7.2, method = "spearman")

# Realizar la prueba de hipótesis para el coeficiente de correlación de Spearman
# Instalar y cargar el paquete 'Hmisc' si no está ya instalado
if (!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)

# Realizar la prueba de Spearman
prueba_spearman <- cor.test(indices, residuales7.2, method = "spearman")

# Mostrar los resultados
print(paste("Coeficiente de correlación de Spearman:", cor_spearman))
print(prueba_spearman)


##independencia 
plot( predchos7.2,residuales7.2,
      xlab = "Datos Predichos",
      ylab = "Residuos Estandarizados",
      main = "Residuos Estandarizados vs Datos Predichos",
      pch = 16)  # pch = 16 para puntos sólidos

# Añadir una línea horizontal en y = 0
abline(h = 0, col = "red", lty = 2)  # lty = 2 para línea punteada


#7.14
# Crear los datos del experimento
data <- data.frame(
  Run = rep(1:8, each = 2),
  A = factor(rep(c(-1, 1, -1, 1, -1, 1, -1, 1), each = 2), levels = c(-1, 1), labels = c("Low", "High")),
  B = factor(rep(c(-1, -1, 1, 1, -1, -1, 1, 1), each = 2), levels = c(-1, 1), labels = c("BW", "Color")),
  C = factor(rep(c(-1, -1, -1, -1, 1, 1, 1, 1), each = 2), levels = c(-1, 1), labels = c("$19.95", "$24.95")),
  Block = factor(rep(1:2, times = 8)),
  Orders = c(50, 54, 44, 42, 46, 48, 42, 43, 49, 46, 48, 45, 47, 48, 56, 54)
)

# Realizar el análisis de varianza (ANOVA) considerando las réplicas como bloques
anova_result <- aov(Orders ~ A * B * C + Block, data = data)

# Resumen del ANOVA
summary(anova_result)
summary(lm(anova_result))

plot(anova_result)

###7.26
# Crear el marco de datos con los resultados del experimento
data <- data.frame(
  Tratamiento = rep(c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc", 
                      "d", "ad", "bd", "abd", "cd", "acd", "bcd", "abcd"), each = 2),
  Replica = rep(1:2, times = 16),
  Rendimiento = c(90, 93, 74, 78, 81, 85, 83, 80, 77, 78, 81, 80, 
                  88, 82, 73, 70, 98, 95, 72, 76, 87, 83, 85, 86, 
                  99, 90, 79, 75, 87, 84, 80, 80)
)

# Ajustar el modelo teniendo en cuenta la confusión
# Modelo lineal con interacciones confundidas con los bloques
data$Confusion <- factor(ifelse(data$Replica == 1 & data$Tratamiento %in% c("abcd", "(1)"), 
                                "ABCD_Replica1", 
                                ifelse(data$Replica == 2 & data$Tratamiento %in% c("abc", "(1)"),
                                       "ABC_Replica2", 
                                       "Ninguno")))

# Realizar el análisis de varianza (ANOVA) considerando la confusión
anova_result <- aov(Rendimiento ~ Tratamiento + Replica + Confusion, data = data)

# Resumen del ANOVA
summary(anova_result)
summary(lm(anova_result))


####8.10

# Datos del experimento
data <- data.frame(
  A = rep(c(0, 1), each = 16),
  B = rep(rep(c(0, 1), each = 8), 2),
  C = rep(rep(c(0, 1), each = 4), 4),
  D = rep(rep(c(0, 1), each = 2), 8),
  E = rep(c(0, 1), 16),
  Rep1 = c(7.78, 8.15, 7.50, 7.59, 7.54, 7.69, 7.56, 7.56, 7.50, 7.88, 7.50, 7.63, 7.32, 7.56, 7.18, 7.81, 
           7.78, 8.15, 7.50, 7.59, 7.54, 7.69, 7.56, 7.56, 7.50, 7.88, 7.50, 7.63, 7.32, 7.56, 7.18, 7.81),
  Rep2 = c(7.78, 8.18, 7.56, 7.56, 8.00, 8.09, 7.52, 7.81, 7.25, 7.88, 7.56, 7.75, 7.44, 7.69, 7.18, 7.50,
           7.78, 8.18, 7.56, 7.56, 8.00, 8.09, 7.52, 7.81, 7.25, 7.88, 7.56, 7.75, 7.44, 7.69, 7.18, 7.50),
  Rep3 = c(7.81, 7.88, 7.50, 7.75, 7.88, 8.06, 7.44, 7.69, 7.12, 7.44, 7.50, 7.56, 7.44, 7.62, 7.25, 7.59,
           7.81, 7.88, 7.50, 7.75, 7.88, 8.06, 7.44, 7.69, 7.12, 7.44, 7.50, 7.56, 7.44, 7.62, 7.25, 7.59)
)


library(reshape2)

# Convertir a formato largo
data_long <- melt(data, id.vars = c("A", "B", "C", "D", "E"), 
                  variable.name = "Replicate", value.name = "Free_Height")


# Crear el modelo
model <- aov(Free_Height ~ A*B*C*D*E, data = data_long)

# Mostrar resultados del ANOVA
summary(model)
summary(lm(model))


effects <- coef(model)[-1]
qqnorm(effects, main = "Normal Q-Q Plot of Factor Effects")
qqline(effects)

# Etiquetas para los puntos
text(qqnorm(effects)$x, qqnorm(effects)$y, labels = names(coef(model))[-1], pos = 4)


# Calcular rango y desviación estándar por combinación de factores
summary_stats <- aggregate(Free_Height ~ A + B + C + D + E, data_long, function(x) {
  c(Rango = diff(range(x)), DesvEst = sd(x))
})

# Ver los resultados
print(summary_stats)

# Verificar si hay indicación de que los factores afectan la variabilidad
library(dplyr)

# Separar los resultados para rango y desviación estándar
rango_data <- summary_stats %>% mutate(Rango = Free_Height[,1])
desvest_data <- summary_stats %>% mutate(DesvEst = Free_Height[,2])

# Ver los resultados de rango y desviación estándar por separado
print(rango_data)
print(desvest_data)

# Realizar un análisis gráfico para ver la variabilidad
boxplot(Free_Height ~ A*B*C*D*E, data = data_long, main = "Variabilidad en la Altura Libre por Corrida",
        xlab = "Combinación de Factores", ylab = "Altura Libre")

##residuales 
plot(model)

library(car)
qqPlot(model$residuals)


# libreria para la prueba de Levene
library(lawstat) 
# libreria para poder hacer la prueba de LEVENE   
library(car)     
# libreria para la prueba de Levene requerida
library(VGAM)    

leveneTest(model$residuals ~ interaction(data_long$A, data_long$B, data_long$C, data_long$D, data_long$E))
leveneTest(model)

library(tseries)
jarque.bera.test(model$residuals)


### 8.34
# Cargar librerías necesarias
library(dplyr)

# Crear el dataframe con los datos
datos <- data.frame(
  Apatite = factor(c(rep("+", 4), rep("-", 4), rep("+", 4), rep("-", 4))),
  pH = factor(c(rep("+", 2), rep("-", 2), rep("+", 2), rep("-", 2), 
                rep("+", 2), rep("-", 2), rep("+", 2), rep("-", 2))),
  Pb_Type = factor(c(rep("+", 2), rep("-", 2), rep("+", 2), rep("-", 2),
                     rep("+", 2), rep("-", 2), rep("+", 2), rep("-", 2))),
  Pb_mM = factor(c(rep(c("Hydroxyapatite", "Fishbone"), 4),
                   rep(c("Fishbone", "Hydroxyapatite"), 4))),
  pH_value = c(3.49, 3.46, 6.84, 6.61, 3.35, 3.34, 3.36, 3.24,
               5.29, 5.06, 5.53, 5.43, 2.82, 2.79, 4.50, 4.74)
)

# Ajustar el modelo de efectos principales y de interacción
modelo <- lm(pH_value ~ Apatite * pH * Pb_Type * Pb_mM, data = datos)

# Resumen del modelo para ver los efectos
summary(modelo)

# Análisis de varianza
anova(modelo)
summ














###6.10
# Crear un dataframe con los datos proporcionados
data <- data.frame(
  TipoBotella = factor(c("Vidrio", "Vidrio", "Vidrio", "Vidrio", "Plástico", "Plástico", "Plástico", "Plástico")),
  Trabajador = factor(c(1, 1, 2, 2, 1, 1, 2, 2)),
  Pulso = c(39, 45, 58, 35, 20, 13, 16, 11, 44, 35, 42, 21, 13, 10, 16, 15)
)
# Ajustar un modelo lineal
modelo <- aov(Pulso ~ TipoBotella + Trabajador, data = data)

# Resumen del análisis de varianza
summary(modelo)
# Análisis de residuos
par(mfrow = c(2, 2)) # Dividir la pantalla en 2x2 para gráficos
plot(modelo)
