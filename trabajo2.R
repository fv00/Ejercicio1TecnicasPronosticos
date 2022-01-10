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

## Pruebas incorrelacion

### Grafica de las fac con las bandas de Bartlett

### Pruebas Ljung-Box

### Pruebas de Durbin-Watson

## Identificacion de modelo ARMA-SARMA

### Identificacion de modelo con auto.arima()

### Identificacion del modelo mediante la funcion armasubsets()

## Estimacion de los modelos con la funcion arima()



## Calculo de pronosticos:

### C

### C + ARMA

### EE

