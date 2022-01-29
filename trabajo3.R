rm(list=ls(all=TRUE))

## Carga de paquetes
library(dataseries)
library(npst)
library(forecast)
library(ggplot2)
library(dplyr)
library(knitr)
library(reshape2)
library(DescTools)
library(TSA)
library(lmtest)

## Lectura de datos
D=dataseries::ds("ch_comb_jobs.596.tot.1.0")
y = D$ch_comb_jobs.596.tot.1.0
fechas = D$time

## Datos trimestrales
y=ts(y,frequency=4, start=c(1991,3))

## Validacion cruzada
entreno = ts(y[1:(114-8)], frequency = 4, start = c(1991, 3))
validacion = ts(y[(114-8+1):114], frequency = 4, start = c(2018, 1))

#-------------------- grafica con fechas en el eje x

np = length(entreno)

fecha = seq(as.Date("1991/01/01"), length.out=(114-8), by="quarters")

ejex.trimestre = seq(fecha[1],fecha[np], "quarters")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,entreno, xaxt="n", panel.first = grid(),type='l',
ylab='Empleos por trimestre', lwd = 1, main='Tasa de empleo en hombres en Suiza')
axis.Date(1, at=ejex.trimestre, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

## Preparacion de los Modelo cubico + indicadoras
t = seq(1,length(entreno))
t2 = t*t
t3 = t2*t
It = seasonaldummy(entreno)
datos = data.frame(entreno,t, t2, t3, It)

## Ajuste modelo cubico + indicadoras
mod1 = lm(entreno ~ t + t2 + t3 +It,data=datos)

##Pruebas de raíz unitaria y estacional

### Canova-Hansen
acf(entreno, lag.max = 50)
pacf(entreno, lag.max = 50)
"La fac en la serie converge a 0 con mucha lentitud, dando
indicios de que la serie tiene una raiz unitaria"

#-----Serie diferenciada
diferenciada <- diff(entreno, 4,1)
ts.plot(diferenciada)

#-----eliminar la tendencia con stl()
m1 = stl(entreno, s.window = 'per')
s1 = m1$time.series[,1]
e1 = m1$time.series[,3]
y1 = s1+e1
ts.plot(y1)

#-----implementacion de la prueba Canova-Hansen
require(uroot) 
res = ch.test(y1, type = "trigonometric", pvalue='raw')
res
#       statistic pvalue
# pi/2     1.4355   0.01 **
# pi       0.4367 0.0616 .
# joint    1.6055   0.01 **
"Conclusion: raiz unitaria
De los resultados obtenidos en la prueba de Canova Hansen 
se concluye que de manera similar a una prueba de Dickey-Fuller se detecta una raíz estacionaria al nivel de 5% en una de las dos frecuencias, por lo que se rechaza la hipótesis nula y se concluye que hay evidencia de inestabilidad estructural en la componente estacional. Por lo que un modelo SARIMA integrado puede ser adecuado para modelar esta serie"

### HEGY
"Se plantea una prueba de HEGY incluyendo la tendencia, el ruido y el intercepto:"
"Cuyo juego de hipotesis consiste en :"
hegy.out = hegy.test(x=entreno, deterministic = c(1,1,1), lag.method = "fixed", maxlag = 1)
hegy.out
#       statistic p-value
# t_1     -2.9411  0.1223
# t_2     -3.5106   0.006 **
# F_3:4   17.2287       0 ***
# F_2:4   18.3556       0 ***
# F_1:4   17.8141       0 ***
"Se observa que no se rechaza una de las hipotesis nulas equivalentes a las de Dickey-Fuller, concluyendo asi que se tiene
evidencia de la existencia de una raiz unitaria y dando indicios de que la serie sea de tipo SARIMA integrado, sin embargo se ha de tener en cuenta la baja potencia de la prueba"
## Prueba HEGY sin tendencia
"Se realiza una prueba de HEGY para la serie sin tendencia con el fin de encontrar raices unitarias estacionales"
hegy.out1 = hegy.test(x=y1, deterministic = c(0,0,1), lag.method = "fixed", maxlag = 1)
hegy.out1
#       statistic p-value
# t_1     -5.4637       0 ***
# t_2     -4.1149   8e-04 ***
# F_3:4   16.7303       0 ***
# F_2:4   18.7506       0 ***
# F_1:4   25.2967       0 ***
"Al realizar sobre la serie sin tendencia se encuentra que se rechazan todas las hipotesis planteadas por la prueba, por lo que se concluye que no hay evidencia de que la serie contenga raices unitarias estacionales correspondiendo con lo observado en las anteriores graficas, donde al diferenciar la serie por primera vez se obtiene una serie estacional y al eliminar la tendencia y repetir la prueba no se encuentran raices unitarias estacionales.
"

### OCSB
"Se realiza una prueba OCSB sobre la serie con tendencia y estacionalidad:"
require(forecast)
# ocsb.test(entreno)
#         OCSB test

# data:  entreno

# Test statistic: -3.4879, 5% critical value: -1.8927
# alternative hypothesis: stationary
"Al realizar una prueba OCSB se obtuvo que el estadistico de prueba tiene un valor de -3.4879 y que el valor critico tiene un valor de -1.8927, por lo tanto se rechaza la hipotesis nula y se toma una de las hipotesis alternativas, se repite la prueba sobre la serie sin tendencia y se concluye acerca de la existencia o no de raices unitarias:"
#--------------eliminar tendencia
# ocsb.test(y1)
#         OCSB test

# data:  y1        

# Test statistic: -4.0032, 5% critical value: -1.8927
# alternative hypothesis: stationar
"Concluir lo mismo que coloco el profesor en las notas del tema 10 cuando realizo la prueba"

### SARIMA
"Finalmente, ya que ligeramente se tiene evidencia de la existencia de que la serie tiene raices unitarias ordinales y que puede ser modelada mediante un SARIMA integrado se propone obtener mediante la funcion autoarima un modelo SARIMA que modelo la serie., obteniendo asi un (S)ARIMA(1,1,0)(0,1,2)[4]:"
mod3 <- auto.arima(entreno)
mod3
# Series: entreno
# ARIMA(1,1,0)(0,1,2)[4]
# Coefficients:
#          ar1     sma1     sma2
#       0.1860  -0.4814  -0.1779
# s.e.  0.1013   0.1130   0.1179

## Punto 2
residuales <- mod3$residuals
### Bandas de Bartlett
acf(residuales,60,ci.type="ma",drop.lag.0=TRUE,main="")
pacf(residuales,60,main="")

"Al imprimir las bandas de Barlett de los residuales obtenidos mediante el modelo SARIMA aplicado a los residuales del modelo de componentes, se observa que ninguna linea sobrepasa las bandas de Bartlett, por lo que se tienen indicios de que estos residuales son ruido blanco"

### Pruebas Ljung-Box
Box.test(residuales, lag = 25, type = "Ljung-Box")
#     Box-Ljung test

# data:  residuales
# X-squared = 20.015, df = 25, p-value = 0.7461
"Al realizar la prueba de Ljung-Box, se obtiene un valor-p mayor a 0.05, por lo que no se tiene evidencia para rechazar que los nuevos residuales obtenidos mediante el modelo SARIMA son ruido blanco"

"Finalmente, luego de estimar un modelo SARIMA y evaluar si sus resultados son o no ruido blanco, se concluye que al modelar la serie con un modelo SARIMA sus residuales obtenidos son ruido blanco."

# Ajuste modelos
n= 114
m = 8

## Datos de validacion
tf = seq((n-m+1),n,1)
tf2 = tf * tf
tf3 = tf2 * tf
Itf = It = seasonaldummy(validacion)
nuevos = data.frame(t = tf,t2 = tf2, t3 = tf3, It = Itf)

## Ajuste modelo cubico + indicadoras
pron1 = predict(mod1, newdata=nuevos)
residuales1 <- mod1$residuals

### C + SARMA
mod_auto2 <- arima(x = residuales1, order = c(1,0,0), seasonal = list(order= c(0,0,1), period = 4))
pred_e  <- predict(mod_auto2,n.ahead=m)$pred
pron1 <- as.vector(pron1 + pred_e)
pron1
# 2671813 2690677 2705497 2682808 2669808 2688252 2697968 2665380
### ETS-AAA
mod_ets <- ets(entreno,model="AAA",damped=FALSE)
pron2 <- as.vector(forecast(mod_ets, m)$mean)
pron2
# 2682489 2711904 2731005 2710191 2708795 2738210 2757311 2736497
### SARIMA
pron3 <- as.vector(predict(mod3, n.ahead=m)$pred)
pron3
# 2674538 2699278 2720203 2712548 2701910 2727078 2746680 2736954
### Graficar pronosticos
fechaf = seq(as.Date("1991/01/01"), length.out=(114), by="quarters")[(114-8+1):114]
pronosticos = data.frame(Fecha = fechaf, Reales = validacion, CSARMA = pron1, ETSAAA = pron2, SARIMA = pron3)


## Graficar resultados:
matriz3 = pronosticos %>% melt(id.vars = c('Fecha'),
                          value.name = 'Tasa')

p3 = ggplot(data = matriz3, aes(x=Fecha, y=Tasa, group = variable, colour = variable)) +
    geom_line() + ggtitle('Estimación de la tasa de empleo en hombres en Suiza') +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, vjust = 0.5))
p3 

R = rbind(accuracy(validacion,pron1), accuracy(validacion,pron2), accuracy(validacion, pron3))
rownames(R) = c("C+ARMA", 'EE',"SARIMA")
# compara calidad de pronosticos con calidad de ajuste
Utheil=c(TheilU(validacion,pron1, type=2),
TheilU(validacion,pron2, type=2), TheilU(validacion,pron3, type=2))
R = cbind(R,Utheil)
R = R[,-c(1,3,4)]
R
#            RMSE      MAPE      Utheil
# C+ARMA 53004.75 1.8040858 0.019397969
# EE     11913.17 0.3810203 0.004359824
# SARIMA 18082.76 0.6452743 0.006617688
"Finalmente, en base a la tabla de pronosticos, su grafica y a la tabla de medidas de error se observa que a excepcion del tercer pronóstico claramente el modelo que mejor pronostica los datos es el modelo ETS-AAA."