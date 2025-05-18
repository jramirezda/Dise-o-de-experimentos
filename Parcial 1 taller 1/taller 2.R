# Datos de la tabla
ejercicio3.18 <- data.frame(
  recubrimiento = factor(rep(1:4, each = 4)),
  replica = factor(rep(1:4, times = 4)),
  conductividad = c(143, 141, 150, 146,
                    152, 149, 137, 143,
                    134, 136, 132, 127,
                    129, 127, 132, 129)
)

# Impresión del data frame
print(ejercicio3.18)

# Verifica la matriz de contrastes
contrasts(ejercicio3.18$recubrimiento) <- contr.sum(levels(ejercicio3.18$recubrimiento))
print(contrasts(ejercicio3.18$recubrimiento))

# Ajuste del modelo
anova3.18 <- lm(conductividad ~ recubrimiento, data = ejercicio3.18, contrasts = list(recubrimiento = "contr.sum"))

# Resumen del modelo
summary(anova3.18)

summary(aov(anova3.18))
# Intervalo de confianza del 95% para el recubrimiento 4
intercept <- coef(anova3.18)[1]
coef_1 <- coef(anova3.18)[2]
coef_2 <- coef(anova3.18)[3]
coef_3 <- coef(anova3.18)[4]

# Media del recubrimiento 4
media_4 <- intercept - (coef_1 + coef_2 + coef_3)

# Error estándar del recubrimiento 4
se_4 <- sqrt(vcov(anova3.18)[1, 1] + vcov(anova3.18)[2, 2] + vcov(anova3.18)[3, 3] + vcov(anova3.18)[4, 4] + 2 * (vcov(anova3.18)[1, 2] + vcov(anova3.18)[1, 3] + vcov(anova3.18)[1, 4] + vcov(anova3.18)[2, 3] + vcov(anova3.18)[2, 4] + vcov(anova3.18)[3, 4]))

# Intervalo de confianza del 95%
alpha_95 <- 0.05
t_value_95 <- qt(1 - alpha_95/2, df = anova3.18$df.residual)
ci_95 <- c(media_4 - t_value_95 * se_4, media_4 + t_value_95 * se_4)

# Intervalo de confianza del 99% para la diferencia entre recubrimiento 1 y recubrimiento 4
# Media del recubrimiento 1
media_1 <- intercept + coef_1

# Diferencia de medias
diff_1_4 <- media_1 - media_4

# Error estándar de la diferencia
se_diff <- sqrt(vcov(anova3.18)[2, 2] + se_4^2)

# Intervalo de confianza del 99%
alpha_99 <- 0.01
t_value_99 <- qt(1 - alpha_99/2, df = anova3.18$df.residual)
ci_99 <- c(diff_1_4 - t_value_99 * se_diff, diff_1_4 + t_value_99 * se_diff)

list(ci_95_recubrimiento4 = ci_95, ci_99_diferencia_1_4 = ci_99)


###D
# Cargar el paquete necesario
if (!require("agricolae")) {
  install.packages("agricolae")
  library(agricolae)
}

# Ajustar el modelo ANOVA
anova3.18 <- aov(conductividad ~ recubrimiento, data = ejercicio3.18)

# Realizar la prueba de Fisher LSD
lsd_result <- LSD.test(anova3.18, "recubrimiento", p.adj = "none", alpha = 0.05)

# Imprimir los resultados
summary(lsd_result)


# Cargar el paquete necesario
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Ajustar el modelo ANOVA
anova3.18 <- aov(conductividad ~ recubrimiento, data = ejercicio3.18)

# Obtener el error estándar
MSerror <- summary(anova3.18)[[1]]["Residuals", "Mean Sq"]
n <- length(levels(ejercicio3.18$recubrimiento))
r <- table(ejercicio3.18$recubrimiento)[1]  # Número de réplicas
SE <- sqrt(MSerror / r)  # Error estándar

# Grados de libertad
df <- df.residual(anova3.18)

# Valor t para el intervalo de confianza del 95%
alpha <- 0.05
t_value <- qt(1 - alpha / 2, df)

# Calcular los intervalos de confianza para cada nivel del factor
means <- aggregate(conductividad ~ recubrimiento, data = ejercicio3.18, mean)
means$SE <- SE
means$CI_lower <- means$conductividad - t_value * SE
means$CI_upper <- means$conductividad + t_value * SE

# Crear el gráfico con ggplot2
ggplot(means, aes(x = recubrimiento, y = conductividad)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  labs(title = "Comparación de Medias con Intervalos de Confianza (95%)",
       x = "Recubrimiento",
       y = "Conductividad") +
  theme_minimal()





###############################3.30
# Datos de calcio por lote
calcio <- data.frame(
  Lote1 = c(23.46, 23.48, 23.56, 23.39, 23.40),
  Lote2 = c(23.59, 23.46, 23.42, 23.49, 23.50),
  Lote3 = c(23.51, 23.64, 23.46, 23.52, 23.49),
  Lote4 = c(23.28, 23.40, 23.37, 23.46, 23.39),
  Lote5 = c(23.29, 23.46, 23.37, 23.32, 23.38)
)

# Convertir los datos a formato largo para el análisis
calcio_long <- stack(calcio)
colnames(calcio_long) <- c("Calcio", "Lote")
# Ajuste del modelo ANOVA
anova_result <- aov(Calcio ~ Lote, data = calcio_long)

#a
# Resumen del ANOVA
summary(anova_result)

#B
# Media cuadrática de los tratamientos (Lotes)
MS_treatment <- summary(anova_result)[[1]]["Lote", "Mean Sq"]

# Media cuadrática del error
MS_error <- summary(anova_result)[[1]]["Residuals", "Mean Sq"]


# Número de réplicas por lote
n <- length(calcio$Lote1)

# Número de lotes
k <- ncol(calcio)

# Varianza de los lotes (entre grupos)
sigma2_treatment <- (MS_treatment - MS_error) / n

# Varianza dentro de los lotes (dentro de grupos)
sigma2_error <- MS_error

list(sigma2_treatment = sigma2_treatment, sigma2_error = sigma2_error)

#c
# Grados de libertad
df <- anova_result$df.residual



# Intervalo de confianza para la varianza del error
alpha <- 0.05
lower_bound <- (df * MS_error) / qchisq(1 - alpha/2, df)
upper_bound <- (df * MS_error) / qchisq(alpha/2, df)

list(lower_bound = lower_bound, upper_bound = upper_bound)

#D
# Residuales del modelo ANOVA
residuals <- residuals(anova_result)

# Verificación de la normalidad de los residuales con un Q-Q plot
qqnorm(residuals)
qqline(residuals)

# Verificación de homogeneidad de varianzas
plot(fitted(anova_result), residuals)
abline(h = 0, col = "red")

# Prueba de Shapiro-Wilk para la normalidad
shapiro_test <- shapiro.test(residuals)

shapiro_test

# Instalar y cargar el paquete necesario
if (!require("lmtest")) {
  install.packages("lmtest")
  library(lmtest)
}

# Realizar el Test de Breusch-Pagan
bp_test <- bptest(anova_result)

# Imprimir los resultados del test
print(bp_test)


####################3.46
# Cargar las bibliotecas necesarias
library(ggplot2)

# Crear el marco de datos
data <- data.frame(
  Algorithm = rep(1:4, each = 6),
  Observation = rep(1:6, times = 4),
  Voltage = c(
    4.93, 4.86, 4.75, 4.95, 4.79, 4.88,
    4.85, 4.91, 4.79, 4.85, 4.75, 4.85,
    4.83, 4.88, 4.90, 4.75, 4.82, 4.90,
    4.89, 4.77, 4.94, 4.86, 4.79, 4.76
  ),
  PotNoise = c(
    0.05, 0.04, 0.05, 0.06, 0.03, 0.05,
    0.04, 0.02, 0.03, 0.05, 0.03, 0.02,
    0.09, 0.13, 0.11, 0.15, 0.08, 0.12,
    0.03, 0.04, 0.05, 0.05, 0.03, 0.02
  )
)

# Convertir Algorithm a factor
data$Algorithm <- as.factor(data$Algorithm)

# Calcular el logaritmo natural del "pot noise"
data$LogPotNoise <- log(data$PotNoise)

# ANOVA para el voltaje promedio
voltage_anova <- aov(Voltage ~ Algorithm, data = data)
print("ANOVA para el voltaje promedio:")
print(summary(voltage_anova))

residuos <- residuals(voltage_anova)
valores_ajustados <- fitted(voltage_anova)

# Crear una figura con dos gráficos
par(mfrow=c(1,2))

# Gráfico Q-Q Normal de los Residuos
qqnorm(residuos, main="Gráfico Q-Q Normal de los Residuos")
qqline(residuos, col="red")

# Gráfico de Residuos vs Valores Ajustados
plot(valores_ajustados, residuos, 
     xlab="Valores Ajustados", 
     ylab="Residuos",
     main="Residuos vs Valores Ajustados")
abline(h=0, col="red")  # Añadir una línea horizontal en y=0

# Restablecer el diseño de gráficos
par(mfrow=c(1,1))

# Imprimir un resumen del modelo ANOVA
print(summary(voltage_anova))



#############cap4
##########4.14

# Datos
velocidad <- rep(c(5, 10, 15, 20), times=4)
horno <- factor(rep(1:4, each=4))
tamaño_grano <- c(8, 14, 14, 17, 4, 5, 6, 9, 5, 6, 9, 3, 6, 9, 2, 6)

# Crear un data frame
datos <- data.frame(velocidad, horno, tamaño_grano)

# Análisis de Varianza (ANOVA)
modelo <- aov(tamaño_grano ~ velocidad + horno, data=datos)
summary(modelo)


# Gráfico de probabilidad normal de los residuos
residuos <- residuals(modelo)
qqnorm(residuos)
qqline(residuos, col = "red")

# Gráfico de residuos frente al horno
plot(datos$horno, residuos, main="Residuos vs Horno", xlab="Horno", ylab="Residuos")
abline(h=0, col="red")

# Gráfico de residuos frente a la velocidad de agitación
plot(datos$velocidad, residuos, main="Residuos vs Velocidad de Agitación", xlab="Velocidad de Agitación", ylab="Residuos")
abline(h=0, col="red")

#######4.26 
# Datos
oil <- factor(rep(1:3, each=5))
truck <- factor(rep(1:5, times=3))
consumo <- c(0.500, 0.634, 0.487, 0.329, 0.512, 0.535, 0.675, 0.520, 0.435, 0.540, 0.513, 0.595, 0.488, 0.400, 0.510)

# Crear un data frame
datos <- data.frame(oil, truck, consumo)

# Modelo mixto
library(lme4)
modelo <- lmer(consumo ~ oil + (1|truck), data=datos)

# Resumen del modelo
summary(modelo)

# Estimación de la componente de varianza del bloque
var_componente <- as.data.frame(VarCorr(modelo))$vcov[1]
var_componente



##### 4.54
# Datos
design <- factor(rep(1:3, each=4))
region <- factor(rep(c("NE", "NW", "SE", "SW"), times=3))
responses <- c(250, 350, 219, 375, 400, 525, 390, 580, 275, 340, 200, 310)

# Crear un data frame
datos <- data.frame(design, responses)

# Modelo completamente aleatorizado (ignorando bloques)
modelo_completo <- aov(responses ~ design, data=datos)

# Resumen del modelo
summary(modelo_completo)

# Comparar con el modelo con bloques (para referencia)
modelo_bloques <- aov(responses ~ design + region, data=datos)
summary(modelo_bloques)

###########cap 13
#13.6
# Datos
library(lme4)

# Crear el dataframe con los datos
data <- data.frame(
  Part = factor(rep(1:10, each = 6)),
  Operator = factor(rep(rep(1:2, each = 3), times = 10)),
  Measurement = c(50, 49, 50, 50, 48, 51,
                  52, 52, 51, 51, 51, 51,
                  53, 50, 50, 54, 52, 51,
                  49, 51, 50, 48, 50, 51,
                  48, 49, 48, 48, 49, 48,
                  52, 50, 50, 52, 50, 50,
                  51, 51, 51, 51, 50, 50,
                  52, 50, 49, 53, 48, 50,
                  50, 51, 50, 51, 48, 49,
                  47, 46, 49, 46, 47, 48)
)

# Ajustar el modelo mixto: piezas como efecto aleatorio y operadores como efecto fijo
model <- lmer(Measurement ~ Operator + (1 | Part), data = data)

# Resumen del modelo
summary(model)

# ANOVA para probar las hipótesis
anova(model)


# Instalar lmerTest si no está instalado
 install.packages("lmerTest")

# Cargar el paquete
library(lmerTest)
 # Ajustar el modelo mixto con lmerTest
 model1 <- lmer(Measurement ~ Operator + (1 | Part), data = data)
 
 # Resumen del modelo con lmerTest
 summary(model1)
 
 # Ajustar el modelo sin efecto aleatorio
 model_no_random <- lm(Measurement ~ Operator, data = data)
 
 # Comparar modelos
 # Ajustar el modelo completo con el efecto aleatorio
 model_full <- lmer(Measurement ~ Operator + (1 | Part), data = data)
 
 # Ajustar el modelo reducido sin el efecto aleatorio
 model_reduced <- lm(Measurement ~ Operator, data = data)
 
 # Comparar modelos usando la prueba de razón de verosimilitud
 anova(model_reduced, model_full)
 
 
 
 
 
 ###########13.30
 # Cargar las librerías necesarias
 library(lme4)  # Para ajustar el modelo lineal mixto
 
 # Datos de la tabla
 data <- data.frame(
   Time = factor(rep(c(40, 50, 60), each = 6*2*3)),
   Temperature = factor(rep(c(300, 350), each = 6*3, times = 3)),
   Operator = factor(rep(c("Operator1", "Operator2", "Operator3"), times = 6*2)),
   Score = c(
     23, 27, 31, 24, 38, 34,
     24, 28, 32, 23, 36, 36,
     25, 26, 29, 28, 35, 39,
     36, 34, 33, 37, 34, 34,
     35, 38, 34, 39, 38, 36,
     36, 39, 35, 35, 36, 31,
     28, 35, 26, 26, 36, 28,
     24, 35, 27, 29, 37, 26,
     27, 34, 25, 25, 34, 24
   )
 )
 
 # Ajustar el modelo lineal mixto con efecto aleatorio para el operador
 model <- lmer(Score ~ Time * Temperature + (1 | Operator), data = data)
 
 # Resumen del modelo para obtener la varianza del componente aleatorio
 summary(model)
 
 # Obtener la varianza del componente aleatorio (varianza del operador)
 var_comp <- as.data.frame(VarCorr(model))
 
 # Calcular intervalo de confianza para la varianza del operador
 sigma2 <- var_comp$vcov[1]
 df <- nrow(data) - length(fixef(model))  # Grados de libertad aproximados
 alpha <- 0.05
 chi2_lower <- qchisq(alpha / 2, df = df, lower.tail = FALSE)
 chi2_upper <- qchisq(1 - alpha / 2, df = df, lower.tail = FALSE)
 
 ci_lower <- sigma2 * (df / chi2_upper)
 ci_upper <- sigma2 * (df / chi2_lower)
 
 # Mostrar resultados
 cat("Varianza del operador:", sigma2, "\n")
 cat("Intervalo de confianza del 95% para la varianza del operador: [", ci_lower, ", ", ci_upper, "]\n")
 
 
 
 
 ############4.38
 # Crear el data.frame
 data <- data.frame(
   Order = rep(1:4, 2),
   Operator = rep(c("Operator 1", "Operator 2", "Operator 3", "Operator 4"), 2),
   Square = factor(rep(1:2, each=16)),
   Treatment = factor(c(
     "C", "D", "A", "B",
     "B", "C", "D", "A",
     "A", "B", "C", "D",
     "D", "A", "B", "C",
     "C", "B", "D", "A",
     "B", "C", "A", "D",
     "A", "D", "B", "C",
     "D", "A", "C", "B"
   )),
   Response = c(
     10, 14, 7, 8,
     7, 18, 11, 8,
     5, 10, 11, 9,
     10, 10, 12, 14,
     11, 10, 14, 8,
     8, 12, 10, 12,
     9, 11, 7, 15,
     9, 8, 18, 6
   )
 )
 
 # Ajustar el modelo
 model <- lm(Response ~ Treatment + Operator + Order + Square + Treatment:Square, data = data)
 
 # Ver el resumen del modelo
 summary(model)
 
 # Realizar el análisis de varianza
 anova_results <- anova(model)
 
 # Ver los resultados del ANOVA
 print(anova_results)
 
 
 
 ########## 4.42 
 # Cargar librerías necesarias
 # Cargar las librerías necesarias
 install.packages("lattice")
 library(lattice)
 
 # Datos de la tabla, con NA para los valores faltantes
 data <- data.frame(
   Concentracion = rep(c(2, 4, 6, 8, 10, 12, 14), each = 7),
   Dia = factor(rep(c(1:7), 7)),
   Resistencia = c(114, NA, NA, NA, NA, NA, NA, 
                   126, 120, NA, NA, NA, NA, NA,
                   NA, 137, 117, NA, NA, NA, NA,
                   141, NA, 129, 149, NA, NA, NA,
                   NA, 129, NA, 150, NA, NA, NA,
                   NA, NA, 120, NA, NA, NA, NA,
                   NA, NA, NA, 136, NA, NA, NA,
                   120, NA, 117, NA, NA, NA, NA,
                   NA, 119, NA, NA, NA, NA, NA,
                   NA, NA, 134, NA, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA, NA,
                   143, NA, NA, NA, NA, NA, NA,
                   118, 123, NA, NA, NA, NA, NA,
                   NA, 130, 127, NA, NA, NA, NA)
 )
 
 # Convertir las concentraciones a factor
 data$Concentracion <- factor(data$Concentracion)
 
 # Ajustar el modelo ANOVA considerando los bloques (días) y tratamientos (concentraciones)
 modelo <- aov(Resistencia ~ Concentracion + Dia, data = data)
 
 # Resumen del modelo
 summary(modelo)
 
 # Graficar para una mejor visualización (opcional)
 xyplot(Resistencia ~ Concentracion | Dia, data = data, type = c("p", "a"),
        main = "Resistencia del Papel por Concentración y Día",
        xlab = "Concentración de Madera Dura (%)", ylab = "Resistencia del Papel")
 