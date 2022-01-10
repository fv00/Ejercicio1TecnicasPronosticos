rm(list=ls(all=TRUE))

## Carga de paquetes
require(dataseries)
require(npst)
require(forecast)
library(ggplot2)
library(dplyr)
library(knitr)
library(reshape2)
require(DescTools)

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

kable(t(medidas(mod1, entreno, 7)), format = 'latex')

#Medidas de error para modelos basados en suavizadores:
medidas.struc = function(y,yest,k){
# y = serie
# yest = valores estimados
# k = numero parametros
T = length(y)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2) 
mse = sse/(T-k)
R2 = 1 - sse/ssr
Ra2 = 1 - (T-1)*(1-R2)/(T-k)
aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
M = c(mse,Ra2,  aic, bic)
names(M) = c("rmse","R2-ad","log.aic","log.bic")
return(M)
}


kable(t(medidas.struc(entreno, ajustados2, 4)))

### Pronosticos:
n= 114
m = 8

tf = seq((n-m+1),n,1)
tf2 = tf * tf
tf3 = tf2 * tf
Itf = It = seasonaldummy(validacion)
nuevos = data.frame(t = tf,t2 = tf2, t3 = tf3, It = Itf)

pron1 = predict(mod1, nuevos )
pron2 = as.vector(forecast(mod_ets, m)$mean)

fechaf = seq(as.Date("1991/01/01"), length.out=(114), by="quarters")[(114-8+1):114]
pronosticos = data.frame(Fecha = fechaf, Reales = validacion, CubicoInd = pron1, ETSAAA = pron2)

## Graficar resultados:
matriz3 = pronosticos %>% melt(id.vars = c('Fecha'),
                          value.name = 'Tasa')

p3 = ggplot(data = matriz3, aes(x=Fecha, y=Tasa, group = variable, colour = variable)) +
    geom_line() + ggtitle('Estimación de la tasa de empleo en hombres en Suiza') +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, vjust = 0.5))
p3                 


## Computar MAPE, RMSE y THAILU


R = rbind(accuracy(validacion,pron1),
accuracy(validacion,pron2))

rownames(R) = c("Cubico + Ind","ETSAAA")

# compara calidad de pronosticos con calidad de ajuste


Utheil=c(TheilU(validacion,pron1, type=2),
TheilU(validacion,pron2, type=2))

R = cbind(R,Utheil)

R = R[,-c(1,3,4)]

(R)

print(xtable(R,digits=3))

pronosticos1 = data.frame(Fecha = fecha, Reales = entreno, CubicoInd = ajustados1, ETSAAA = ajustados2)

## Graficar resultados:
matriz4 = pronosticos1 %>% melt(id.vars = c('Fecha'),
                          value.name = 'Tasa')

p4 = ggplot(data = matriz4, aes(x=Fecha, y=Tasa, group = variable, colour = variable)) +
    geom_line() + ggtitle('Estimación de la tasa de empleo en hombres en Suiza') +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45, vjust = 0.5))
p4                 