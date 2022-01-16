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
datos = data.frame(t, t2, t3, It)

## Ajuste modelo cubico + indicadoras
mod1 = lm(entreno ~ t + t2 + t3 +It)

## Obtencion de valores ajustados
ajustados1 = mod1$fitted.values

## DataFrame de valores reales y ajustados
matriz1 = data.frame(Fecha = fecha,
                     Reales = entreno,
                     Ajustados = ajustados1) %>%
    melt(id.vars = c('Fecha'), value.name = 'Tasa')

## Graficar resultados:

p = ggplot(data = matriz1, aes(x=Fecha, y=Tasa, group = variable, colour = variable)) +
    geom_line() + ggtitle('Estimación de la tasa de empleo en hombres en Suiza') +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, vjust = 0.5))
p

#ETS AAA
mod_ets = ets(entreno,model="AAA",damped=FALSE)
ajustados2 = mod_ets$fitted
summary(mod_ets)

## Graficar resultados:
matriz2 = data.frame(Fecha = fecha,
                     Reales = entreno,
                     Ajustados = ajustados2) %>%
                     melt(id.vars = c('Fecha'),
                          value.name = 'Tasa')

p2 = ggplot(data = matriz2, aes(x=Fecha, y=Tasa, group = variable, colour = variable)) +
    geom_line() + ggtitle('Estimación de la tasa de empleo en hombres en Suiza') +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, vjust = 0.5))
p2

# Trabajo 2
## Pruebas incorrelacion

### Grafica de las fac con las bandas de Bartlett
residuales1 = mod1$residuals
residuales1 = ts(residuales1, frequency=4, start=c(1991,3))

### Bandas de Bartlett
acf(residuales1,60,ci.type="ma",drop.lag.0=TRUE,main="")
# pacf(residuales1,60,main="")

### Si estan todas las lineas hay evidencia, sino, hay evidencia de autocorrelacion


### Pruebas Ljung-Box
Box.test(residuales1, lag = 25, type = "Ljung-Box")

"Como el p-valor es menor a 0.05 se rechaza la hipotesis nula y se concluye los residuos del modelo de componentes estan correlacionados"

### Pruebas de Durbin-Watson
dwtest(mod1) 
"Con la prueba de Durbin-Watson también se obtiene el mismo resultado"
"Se concluye asi que los residuos del modelo de componentes estan correlacionados y que no son ruido blanco, por lo tanto, se pueden modelar"

## Identificacion de modelo ARMA-SARMA

"Se identifica el modelo ARMA-SARMA mediante la funcion autoarima, obteniendo asi el siguiente modelo candidato:"
### Identificacion de modelo con auto.arima()
mod_auto <- auto.arima(residuales1, stationary = TRUE, seasonal = TRUE, ic = 'aicc')
"El modelo que se obtiene mediante la funcion autoarima es un SARMA(2,0,0)(0,0,1)[4]"
coeftest(mod_auto)
"Al observar la significancia de sus coeficientes se observa que el segundo rezago autoregresivo NO es significativo y que el primero sí, por lo que se propone disminuir en una unidad al valor de p asociado al rezago autoregresivo del modelo, obteniendo asi el siguiente resultado:"
mod_auto2 <- arima(x = residuales1, order = c(1,0,0), seasonal = list(order= c(0,0,1), period = 4))
coeftest(mod_auto2)
"Se observa que al disminuir en un grado el rezago autoregresivo se obtiene que todos los rezagos asociados al modelo son signficativos, por lo que por este criterio se decanta por este modelo obtenido de depurar ligeramente al modelo obtenido mediante la funcion auto.arima"
### Identificacion del modelo mediante la funcion armasubsets()
plot(armasubsets(residuales1, 8, 8))
"Desde la funcion armasubsets se observa que si se guia por el principio de parsimonia el unico rezago significativo seria el primer rezago de la parte autoregresiva, ya que el grado proximo mas alto significativo asociado al rezago autoregresivo seria el quinto rezago, ademas se tiene en cuenta que esta serie tiene estacionalidad s=4, por lo que el modelo elegido seria un SARMA(1,0,0)(0,0,0)[4]"
## Estimacion de los modelos con la funcion arima()
mod2_0 <- arima(x = residuales1, order = c(1,0,0), seasonal= list(order = c(0,0,0), period = 4))
### Significancia coeficientes modelos
coeftest(mod2_0)
"Al observar sus coeficientes asociados se observa que el unico rezago autoregresivo que se incluye en el modelo es significativo, por lo que se decanta por este modelo."

### AIC modelos
AIC(mod_auto2)
AIC(mod2_0)

"A partir de los AIC se observa que el modelo con el menor AIC es aquel obtenido al depurar ligeramente el modelo resultado de la funcion auto.arima, por lo que se decanta por este modelo"

### Se elige el modelo obtenido mediante la funcion auto.arima


### Grafica de las fac con las bandas de Bartlett
residuales = mod_auto2$residuals
residuales = ts(residuales, frequency=4, start=c(1991,3))

### Bandas de Bartlett
acf(residuales,60,ci.type="ma",drop.lag.0=TRUE,main="")
# pacf(residuales,60,main="")

"Al imprimer las bandas de Barlett de los residuales obtenidos mediante el modelo SARMA aplicado a los residuales del modelo de componentes, se observa que ninguna linea sobrepasa las bandas de Bartlett, por lo que se tienen indicios de que estos residuales son ruido blanco"

### Pruebas Ljung-Box
Box.test(residuales, lag = 25, type = "Ljung-Box")

"Al realizar la prueba de Ljung-Box, se obtiene un valor-p mayor a 0.05, por lo que no se tiene evidencia para rechazar que los nuevos residuales obtenidos mediante el modelo SARMA son ruido blanco"

## Calculo de pronosticos:
n= 114
m = 8

tf = seq((n-m+1),n,1)
tf2 = tf * tf
tf3 = tf2 * tf
Itf = It = seasonaldummy(validacion)
nuevos = data.frame(t = tf,t2 = tf2, t3 = tf3, It = Itf)

### C
pron1 = predict(mod1, nuevos )

### C + ARMA
pred_e  = predict(mod_auto2,n.ahead=m)$pred
pron2 = as.vector(pron1 + pred_e)

### EE
pron3 = as.vector(forecast(mod_ets, m)$mean)

## Calculo de medidas de error:

R = rbind(accuracy(validacion,pron1), accuracy(validacion,pron2), accuracy(validacion, pron3))

rownames(R) = c("C", 'C+ARMA',"EE")

# compara calidad de pronosticos con calidad de ajuste


Utheil=c(TheilU(validacion,pron1, type=2),
TheilU(validacion,pron2, type=2), TheilU(validacion,pron3, type=2))

R = cbind(R,Utheil)

R = R[,-c(1,3,4)]

"Se realizan los pronosticos con los tres modelos y se obtienen los siguientes resultados:"


"Se imprimen graficamente los resultados:"
fechaf = seq(as.Date("1991/01/01"), length.out=(114), by="quarters")[(114-8+1):114]
pronosticos = data.frame(Fecha = fechaf, Reales = validacion, Componentes = pron1, SARMA = pron2, EE = pron3)

## Graficar resultados:
matriz3 = pronosticos %>% melt(id.vars = c('Fecha'),
                          value.name = 'Tasa')

p3 = ggplot(data = matriz3, aes(x=Fecha, y=Tasa, group = variable, colour = variable)) +
    geom_line() + ggtitle('Estimación de la tasa de empleo en hombres en Suiza') +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, vjust = 0.5))
p3 
"A simple vista se pueda observar que entre los modelos el modelo que se ajusta mejor a los datos reales sigue siendo el modelo de espacio de estados. Con la inclusion del modelo SARMA, se obtiene un modelo intermedio que aparentemente arroja valores mas consistentes que los obtenidos mediante el modelo de componentes, pero no lo suficiente como para competir con el modelo de espacio de estados."

"Finalmente, se tabulan el MAPE, RMSE y U-Theil de los 3 modelos:"
#            RMSE      MAPE      Utheil
# C      74143.86 2.6899376 0.027134178
# C+ARMA 53004.75 1.8040858 0.019397969
# EE     11913.17 0.3810203 0.004359824
"De la tabla anterior se respalda la idea planteada al concluir acerca de las graficas, ya que se observa que el modelo de espacio de estados sigue siendo el mejor candidato segun las medidas tabuladas y que el modelo de componentes+arma ocupa un valor intermedio en todas las medidas."