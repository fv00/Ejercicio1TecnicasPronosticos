
#-------------------Cargar los datos
datos = read.table("llamadas.banco.prn",
 header = TRUE, stringsAsFactors = FALSE)
attach(datos)

np = length(y)

fechas = as.Date(fecha,format="%d/%m/%y")

par(mfrow=c(2,1))

plot(fechas,y, xaxt="n",panel.first = grid(),type='l',ylab='')
axis.Date(1, at=seq(fechas[1],fechas[np], "months"), format="%m/%y")
axis.Date(1, at=seq(fechas[1],fechas[np], "years"), labels = FALSE, tcl = -0.2)

plot(fechas[1:70],y[1:70], xaxt="n",panel.first = grid(),type='b',ylab='')
axis.Date(1, at=seq(fechas[1],fechas[np], "months"), format="%m/%y")
axis.Date(1, at=seq(fechas[1],fechas[np], "years"), labels = FALSE, tcl = -0.2)


layout(1:2)
ts.plot(y)
ts.plot(log(y))

par(mfrow=c(3,2))

ts.plot(y)
plot(density(y),xlab='x',main= '')
acf(y,60,main="")
pacf(y,60,main="")
qqnorm(y)
qqline(y,col=2)



#---------modelo de componentes serie llamadas call center
require(forecast)
y = ts(y,frequency=7)
It = fourier(y,2)
t = seq(1,length(y))
m.l = lm(y ~ t + It )
summary(m.l)
#---------residuos estructurales
yr = resid(m.l)
yr = ts(yr,frequency=7)

par(mfrow=c(3,2))
require(TSA)
plot(t,yr,type='o',ylab='residuo')
abline(h=0,lty=2)
TSA::acf(yr,60,ci.type="ma",drop.lag.0 = TRUE,main="")
pacf(yr,60,main="")
qqnorm(yr)
qqline(yr,col=2)
plot(density(yr),xlab='x',main= '')

cpgram(yr) # periodograma acumulado
#------------identificador armasubsets
require(TSA)
res=armasubsets(y=yr,
nar=14,nma=14,y.name='y',ar.method='ols')
par(mfrow=c(1,1))
plot(res)
#------------identificador auto.arima
auto.arima(yr)
ARIMA(5,0,3)(2,0,0)[7] with zero mean

m.1 = arima(yr,order=c(3,0,3),seasonal=
list(order=c(2,0,1),period=7))
require(lmtest)
coeftest(m.1)

m.2 = arima(yr,order=c(5,0,3),seasonal=
list(order=c(2,0,0),period=7))

coeftest(m.2)

(c(AIC(m.1),AIC(m.2)))

at = resid(m.2)

par(mfrow=c(3,2))
require(TSA)

plot(t,at,type='o',ylab='residuo')
abline(h=0,lty=2)

acf(at,60,drop.lag.0 = TRUE,main="")
pacf(at,60,main="")
qqnorm(at)
qqline(at,col=2)
plot(density(at),xlab='x',main= '')
cpgram(at)


Box.test(x = at, lag = 7, type="Ljung-Box")

Box.test(x = at, lag = 14, type="Ljung-Box")

Box.test(x = at, lag = 21, type="Ljung-Box")


Box.test(x = at, lag = 28, type="Ljung-Box")


#------------calcular pronosticos a 21 dias
T = length(y)
tt = seq((T-21+1),T,1); Itt = fourier(yr,2,21);
py.l = predict(m.l,data.frame(t=tt,It=I(Itt)))
p.arma = predict(m.2,n.ahead=21)$pred
py.tot = py.l+p.arma


se = predict(m.2,n.ahead=21)$se

par(mfrow=c(1,1))
T = length(y)

plot(seq(T-90,T+21), c(y[(T-90):T],py.tot), 
type='l', col=2, ylim=c(-500,20000))

points(seq(T+1,T+21),py.tot, pch=20,lwd=2, col='blue')

points(seq(T+1,T+21), py.tot+1.64*se, pch=20, col='blue')
points(seq(T+1,T+21), py.tot-1.64*se, pch=20, col='blue')



