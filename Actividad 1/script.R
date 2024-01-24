Datos_Actividad_1 <- readxl::read_xlsx("Actividad 1/Datos Actividad 1.xlsx")
# mostramos los headers de los datos importados
ls(Datos_Actividad_1)
# mostramos algunas entradas de los datos
Datos_Actividad_1
# creamos una matriz de dimension 10
matriz10 <- data.frame(matrix(ncol = 10, nrow = 10))
# creamos matriz de demsion 5
matriz5 <- data.frame(matrix(ncol=5, nrow = 5))
# borramos dataframe de dimension 10
rm(matriz10)
#
#   TAREA 6 - Histograma
#
# 
hist(Datos_Actividad_1$`HDI 2021`,main = "Histograma columna HDI 2021")
# Segundo histograma
hist(Datos_Actividad_1$`Average annual growth(2010–2021)​`, main = "Segundo Histograma")
#
#
# Graficos de densisdad
#
# creamos una variable temporal
hdi <- Datos_Actividad_1$`HDI 2021`
densidad_hdi <- density(hdi, na.rm = TRUE) # calculamos la densidad
plot(densidad_hdi) # dibujamos la densidad

# repetimos para la media
avg <- Datos_Actividad_1$`Average annual growth(2010–2021)​`
den_avg <- density(avg, na.rm = TRUE)
plot(den_avg)

#
#
# MAGNITUDES COMUNES
# media, mediana, ...
#
#
mediana <- (median(Datos_Actividad_1$`HDI 2021`, na.rm = TRUE))
media <- mean(Datos_Actividad_1$`HDI 2021`, na.rm = TRUE)
cuartiles <- quantile(Datos_Actividad_1$`HDI 2021`, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
desviacion <- sd(Datos_Actividad_1$`HDI 2021`, na.rm = TRUE)
#coefieciente de variacion
cv <- (desviacion / media) * 100
iqr <- IQR(Datos_Actividad_1$`HDI 2021`, na.rm = TRUE)
maximo <- max(Datos_Actividad_1$`HDI 2021`, na.rm = TRUE)
minimo <- min(Datos_Actividad_1$`HDI 2021`, na.rm= TRUE)

resumen_estadistico_HDI <- data.frame(
  Media = media,
  Mediana = mediana,
  DesviacionEstandar = desviacion,
  CoeficienteVariacion = cv,
  IQR = iqr,
  Cuartil_10 = cuartiles[1],
  Cuartil_25 = cuartiles[2],
  Cuartil_50 = cuartiles[3],
  Cuartil_75 = cuartiles[4],
  Cuartil_90 = cuartiles[5],
  Maximo = maximo,
  Minimo = minimo
)

#
# -------------------
#
mediana <- (median(Datos_Actividad_1$`Average annual growth(2010–2021)​`, na.rm = TRUE))
media <- mean(Datos_Actividad_1$`Average annual growth(2010–2021)​`, na.rm = TRUE)

cuartiles <- quantile(Datos_Actividad_1$`Average annual growth(2010–2021)​`, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)

desviacion <- sd(Datos_Actividad_1$`Average annual growth(2010–2021)​`, na.rm = TRUE)
desviacion
#coefieciente de variacion
cv <- (desviacion / media) * 100
cv
iqr <- IQR(Datos_Actividad_1$`Average annual growth(2010–2021)​`, na.rm = TRUE)
iqr
maximo <- max(Datos_Actividad_1$`Average annual growth(2010–2021)​`, na.rm = TRUE)
minimo <- min(Datos_Actividad_1$`Average annual growth(2010–2021)​`, na.rm= TRUE)


resumen_estadistico <- data.frame(
  Media = media,
  Mediana = mediana,
  DesviacionEstandar = desviacion,
  CoeficienteVariacion = cv,
  IQR = iqr,
  Cuartil_10 = cuartiles[1],
  Cuartil_25 = cuartiles[2],
  Cuartil_50 = cuartiles[3],
  Cuartil_75 = cuartiles[4],
  Cuartil_90 = cuartiles[5],
  Maximo = maximo,
  Minimo = minimo
)

print(resumen_estadistico_HDI)
print(resumen_estadistico)

