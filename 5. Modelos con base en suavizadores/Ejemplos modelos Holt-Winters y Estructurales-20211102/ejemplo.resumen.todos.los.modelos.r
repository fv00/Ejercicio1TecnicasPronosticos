# Analisis de Numero de Licencias
# Modelos  Holt-Winters, BSM, NNAR
# leer
G = read.table("series_histo_88_mpio_jun17.prn", 
header = TRUE, stringsAsFactors = FALSE)
attach(G)

# definir como serie de tiempo
y = ts(G$Totalviv,frequency=12,start=c(2009,1),end=c(2016,12))

ts.plot(y)

m = 12
n = length(y)
yi = ts(y[1:(n-m)],frequency=12,start=c(2009,01))
yf = ts(y[(n-m+1):n],frequency=12,start=c(2016,01))

lyi = log(yi)
t = seq(1,length(yi))

#-------------------- grafica con fechas en el eje x

np = length(yi)

fecha = seq(as.Date("2009/01/01"), length.out=(n-m), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#--------------analisis iniciales
#--------------detectar estacionalidad
# Package 'npst' generalizes Hewitt's (1971) test 
for seasonality and Rogerson's (1996) extension 
based on Monte-Carlo simulation.

require(npst)

npst(indata=as.numeric(y), peak=12, repts=100000,
  whole.distribution=FALSE, 
siglevels=c(0.05),
  PARALLEL=FALSE)

#--------------cuadratico + indicadoras

require(forecast)
source("medidas.r")
source("medidas.struc.r")


It = seasonaldummy(yi)
t2 = t*t
mod1 = lm(yi ~ t + t2 + It)
summary(mod1)

yhat1 = fitted(mod1)

k = 3 + 11
(M.cuad.est = medidas(mod1,yi,k))

#-----------------pronosticos
tf = seq((n-m+1),n,1)
Itf = seasonaldummy(yi,m)
tf2 = tf*tf

pron1 = predict(mod1,
data.frame(t = tf,t2 = tf2, It=I(Itf)))

pron1 = ts(pron1,frequency=12,start=c(2016,01))
(H.cuad.est = accuracy(pron1,yf))

#--------------cúbico + indicadoras
t3 = t*t2
mod2 = lm(yi ~ t + t2 + t3 + It)
summary(mod2)

yhat2 = fitted(mod2)

k = 4+11
(M.cub.est = medidas(mod2,yi,k))

#-----------------pronosticos
tf3 = tf2*tf
pron2 = predict(mod2,
data.frame(t = tf,t2 = tf2, t3 = tf3, It=I(Itf)))

pron2 = ts(pron2,frequency=12,start=c(2016,01))
(H.cub.est = accuracy(pron2,yf))

#--------------exponencial(cuadratico + indicadoras)
mod3 = lm(lyi ~ t + t2 + It)

beta0 = mod3$coefficient
Xt = cbind(rep(1,length(yi)),t,t2,It)
Ds = data.frame(yi,Xt)

mod4 = nls(yi~exp(Xt%*%beta),
data=Ds,
start=list(beta=beta0))

summary(mod4)

yhat4 = fitted(mod4) 

length(yhat4)
length(yi)
k = 3+11
(M.exp.cuad.est = medidas(mod4,yi,k))


# --------------pronosticos

Xtf = cbind(rep(1,m),tf,tf2,Itf)
pron4 = predict(mod4,
data.frame(Xt = I(Xtf)))

pron4 = ts(pron4,frequency=12,start=c(2016,01))
(H.exp.cuad.est = accuracy(pron4,yf))


#--------------exponencial(cúbico + indicadoras)
mod5 = lm(lyi ~ t + t2 + t3 + It)

beta0 = mod5$coefficient
Xt = cbind(rep(1,length(yi)),t,t2,t3,It)
Ds = data.frame(yi,Xt)

mod6 = nls(yi~exp(Xt%*%beta),
data=Ds,
start=list(beta=beta0))

summary(mod6)

yhat6 = fitted(mod6) 

k = 4+11
(M.exp.cub.est = medidas(mod6,yi,k))

# pronosticos con el exp cub 

Xtf = cbind(rep(1,m),tf,tf2,tf3,Itf)
pron6 = predict(mod6,
data.frame(Xt = I(Xtf)))


pron6 = ts(pron6,frequency=12,start=c(2016,01))
(H.exp.cub.est = accuracy(pron6,yf))

#--------------híbrido Loess+indicadoras

mod7 = loess(yi ~ t, span=0.5,
control = loess.control(surface = "direct"))

#----------genera matriz con todas las indicadoras 
dummy=function(s,n,i){ 
# s: estaciones, n: numero de datos, i: estación de inicio
A=diag(s)
for(j in 1:(floor(n/s))){ 
A=rbind(A,diag(s))}
A=A[i:(n+i-1),]
return(A)}

It12 = dummy(12,length(yi),1)

y.st = yi - yhat7

mod8= lm(y.st ~ -1+ It12)
summary(mod8)

yhat7 = mod7$fitted+mod8$fitted

(M.hib.loess.ind = medidas.struc(yi,yhat7,3+12))


#----------pronosticos

pr.yw = predict(mod7, data.frame(t = tf))
Itf12 = dummy(12,m,7)
pr.st = predict(mod8,data.frame(It12=I(Itf12)))
pron7 = pr.yw+pr.st

pron7 = ts(pron7,frequency=12,start=c(2016,01))
(H.hib.loess.ind = accuracy(pron7,yf))

#-----------------------Holt-Winters

mod9 = HoltWinters(yi)
(c(mod9$alpha,mod9$beta,mod9$gamma))
# la tendencia, componente estacional y y estimada

yhat9 = fitted(mod9)[,1]

(M.HW = medidas.struc(yi,yhat9,3))

Tt = fitted(mod9)[,2] + fitted(mod9)[,3] 
St = fitted(mod9)[,4]

# y 12 pronosticos se calculan como
pron9= predict(mod9,12) # prediction.interval = TRUE)

pron9 = ts(pron9,frequency=12,start=c(2016,01))
(H.HW = accuracy(pron9,yf))

#---------------------------aplicar TS-Struct

mod10 = StructTS(yi)
print(mod10)

Variances:
    level      slope       seas    epsilon  
7.085e-01  5.056e-04  4.524e-01  1.006e+01  

str(y.bsm)
fitted(y.bsm)
# "level" "slope" "sea"

yhat10 = apply(fitted(mod10),1,sum)

(M.ST = medidas.struc(yi,yhat10,3))

require(forecast)
B=forecast(mod10,h=m)
pron10 = B$mean
pron10 = ts(pron10,frequency=12, start=c(2016,01))

(H.ST = accuracy(pron10,yf))

#----------------- red neuronal

mod11 = nnetar(yi,lambda=0)

# y.nnar = nnetar(yi,p=2,P=1,size=3,lambda=0)

yhat11= fitted(mod11)

(M.NNAR = medidas.struc(yi[!is.na(yhat11)],na.omit(yhat11),10))

pron11 = forecast(mod11,h=m)$mean

pron11 = ts(pron11,frequency=12, start=c(2016,01))

(H.NNAR = accuracy(pron11,yf))

#---------------resumenes

M = rbind(M.cuad.est,
M.cub.est,
M.exp.cuad.est,
M.exp.cub.est,
M.hib.loess.ind,
M.HW,
M.ST,
M.NNAR)

(M)

H = rbind(H.cuad.est,
H.cub.est,
H.exp.cuad.est,
H.exp.cub.est,
H.hib.loess.ind,
H.HW,
H.ST,
H.NNAR)

(H)

#-----------modelo automatico
ets(yi)

m1 = ets(yi,model="MNA",damped=FALSE)
summary(m1)
yhat1 = m1$fitted

plot(t,yi,type='b')
lines(t,yhat1,col='red')

#---- h pronosticos
yf.ets = forecast(m1,m)$mean

yf.ets = ts(yf.ets,frequency=12, start=c(2016,01))

accuracy(yf.ets,yf)

(medidas.struc(yi,yhat1,3))

