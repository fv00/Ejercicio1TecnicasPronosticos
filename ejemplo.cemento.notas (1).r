#-----Ejemplo serie cemento
E = read.table("cementq.dat", header = TRUE)
attach(E)
y = ts(y,frequency=4,start=c(1956,1),end=c(1994,3))

#----- con stl()
m1 = stl(y, s.window = 'per')
plot(m1)

dyos = diff(diff(y,4,1),1,1)
dys = diff(y,4,1)
dyo = diff(y,1,1)

par(mfrow = c(2,2))
ts.plot(y,main="(A)")
ts.plot(dyo,main="(B)")
ts.plot(dys,main="(C)")
ts.plot(dyos,main="(D)")

require(TSA)
par(mfrow = c(1,2))
acf(dyos,24,drop.lag.0=TRUE,ci.type="ma")
pacf(dyos,24)

dyos = ts(dyos,frequency=4)
require(forecast)
auto.arima(dyos)
ARIMA(3,0,2)(0,0,1)[4] with zero mean