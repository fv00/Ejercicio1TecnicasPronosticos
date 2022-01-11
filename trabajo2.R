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

#Medidas de error para modelos lineales:
medidas = function(m,y,k){
# m = objeto producido con lm()
# y = variable dependiente
# k = número de coeficientes beta
T = length(y)
yest = fitted(m)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
mse = sse/(T-k)
R2 = 1 - sse/ssr
Ra2 = 1 - (T-1)*(1-R2)/(T-k)
aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
M = c(Ra2, mse, aic, bic)
names(M) = c("R2-ajus","MSE","logAIC","logBIC")
return(M)
}


## Pruebas incorrelacion

### Grafica de las fac con las bandas de Bartlett
residuales1 = mod1$residuals
residuales1 = ts(residuales1, frequency=4, start=c(1991,3))

### Bandas de Bartlett
acf(residuales1,60,ci.type="ma",drop.lag.0=TRUE,main="")
pacf(residuales1,60,main="")

### Si estan todas las lineas hay evidencia, sino, hay evidencia de autocorrelacion


### Pruebas Ljung-Box
Box.test(residuales1, lag = 25, type = "Ljung-Box")


### Pruebas de Durbin-Watson
dwtest(mod1) 

## Identificacion de modelo ARMA-SARMA


### Identificacion de modelo con auto.arima()
mod_auto <- auto.arima(entreno, stationary = TRUE, seasonal = TRUE, ic = 'aicc')
#SARMA(3,1)(2,0)[4]

### Identificacion del modelo mediante la funcion armasubsets()
plot(armasubsets(entreno, 8, 8))
## Estimacion de los modelos con la funcion arima()
mod2_1 <- arima(c(1,1,0), c(0,1,2), 4)

### Estimacion de modelos con funcion arima()
(5,7)(2,1)[4]
### AIC modelos

### Validacion de residuos con la fac

### Prueba de Ljung-Box

## Calculo de pronosticos:


### C


### C + ARMA


### EE

