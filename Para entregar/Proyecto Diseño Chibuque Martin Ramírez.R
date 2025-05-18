## Análisis de los datos Estudio experimental sobre la remoción de manchas
## Gabriela Chibuque, Miguel Martin, Jhon Ramírez 
resultados <-  c(0.8271949893042507, 1.1238035718610602, 0.964515760418098, 1.013680994822095, 
                 1.0508379608629825, 0.914349005285425, 1.235426177088032, 0.9905014122218547, 
                 0.7653642913394095, 1.2514170483006912, 0.9904064585080199, 0.9349252440554778, 
                 1.167486148370997, 1.1053993573003553, 0.9498294394412983, 1.1612993005616676, 
                 1.0504086392815788, 0.8216337637178089, 0.9442664876451229, 0.9919124699606036, 
                 1.1586473567865212, 1.1225382949684573, 0.9601898960223937, 1.1626583411512172, 
                 1.0778769476876529, 0.774280710569622, 0.9587421186320213, 0.7409431313643372, 
                 1.0834487802793513, 0.9207146880870465, 1.0498954566328063, 1.0072898443777514, 
                 0.6776803927014329, 0.953048637533842, 1.03680627570232, 0.8974072841952373, 
                 0.7037845390282598, 0.9522838839293041, 0.7951500675845543, 0.8987596202575742, 
                 1.0254161126533337, 1.06466460552074, 0.8325997195980754, 1.014847784064339, 
                 1.1361962745347236, 0.606508326296588, 1.0566198420281745, 0.8477588121093643, 
                 0.988279283214424, 0.7615651833025758, 0.9990145609039675, 0.7234503435668153, 
                 0.8869507222718216, 1.10364487962027, 1.3207420716097964, 1.2101378699453798, 
                 1.1015361714457843, 1.3521481873577446, 1.26821895807844, 1.394669037578734, 
                 1.2252671713282903, 1.3048213764114713, 1.4096756501347796, 1.2169857243692748, 
                 0.9599317701026154, 1.1096556472746966, 1.2275393772506775, 1.0495713132232254, 
                 1.0425630991257404, 0.9987792523572305, 0.8993368952395263, 0.9784825896597334, 
                 1.0713005852017319, 0.9579892297260436, 1.108682635348195, 1.086504920498508, 
                 1.280050064026727, 1.096865612385824, 1.2418355455745875, 0.9954256073979227, 
                 1.045557301314669)

summary(resultados)
boxplot(resultados)

df <- data.frame(Índice = 1:81, Resultados = resultados)


# Vector con el nuevo orden de índices
nuevo_orden <- c(2, 9, 5, 7, 24, 21, 20, 18, 16, 17, 22, 4, 10, 23, 13, 8, 3, 12, 11, 1, 
                 25, 27, 15, 19, 6, 26, 14, 42, 52, 34, 38, 37, 50, 53, 43, 36, 30, 49, 29, 
                 32, 35, 41, 28, 33, 46, 54, 31, 39, 44, 45, 48, 40, 51, 47, 71, 75, 62, 81, 
                 68, 69, 79, 55, 70, 80, 74, 65, 73, 63, 77, 76, 57, 67, 56, 61, 66, 64, 60, 
                 59, 78, 58, 72)

# Reordenar el dataframe según el nuevo orden
df_reordenado <- df[match(nuevo_orden, df$Índice), ]


respuestaOrd <- df_reordenado$Resultados

tiempo <- factor(rep(c(rep(0, 9), rep(1, 9), rep(2, 9)), 3))
TipoM <-factor(rep(0:2, length.out = 81))
blanqueador <- factor(rep(c(rep(0, 3), rep(1, 3), rep(2, 3)), length.out = 81))
tela <- factor(c(rep(0, 27), rep(1, 27), rep(2, 27)))

Datos <-data.frame(Tiempo=tiempo,TipoM=TipoM,Blanqueador=blanqueador,Tela=tela,Respuesta=respuestaOrd) 
write.csv(Datos, file = "Datos.csv", row.names = FALSE)


Modelo <-lm(Respuesta ~Blanqueador+Tiempo+TipoM +Tela,contrasts=list(Blanqueador="contr.sum",Tiempo="contr.sum",TipoM="contr.sum",Tela="contr.sum"), data=Datos)

summary(Modelo)
summary(aov(Modelo))


# Validación de supuetos 

plot(Modelo)

#test de homocedasticidad
bartlett.test(Modelo$residuals~blanqueador,data=Datos)
library(car)     
leveneTest(Modelo$residuals, Datos$Blanqueador)   

#Test normalidad 
ks.test(Modelo$residuals,"pnorm",mean(Modelo$residuals),
        sd(Modelo$residuals))

qqPlot(Modelo$residuals)

library(tseries)
jarque.bera.test(Modelo$residuals)


#comparaciones multiples 
pruebas<-TukeyHSD(aov(Modelo))
