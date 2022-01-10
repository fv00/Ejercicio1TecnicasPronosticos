
# Ejemplo de comparacion de ajuste Holt-Winters versus STL

E = read.table("demanda_energia_18.dat", header = TRUE, stringsAsFactors = FALSE)

dem = ts(E$x, frequency = 7, start = c(1995,01))

ts = seq(1,length(dem))

plot(ts,dem,type='l', main="demanda energia diaria 8 años")

#-subserie


n = 2*364
plot(t[1:n],dem[1:n],type='b')
abline(v=seq(7,364,7))

y = ts(dem[1:n],frequency=7)
t = seq(1,n)

#-----------verifica periodo
par(mfrow=c(2,1))
spectrum(y, spans=c(5,7), log="dB", ci=0.8)
spec.pgram(y,kernel("modified.daniell", c(3,3)), log="no")  


#-------- metodo stl basado en  loess

par(mfrow=c(1,1))
plot( stl(y, s.window = 'per', t.window = 50, t.jump = 1))
m1 = stl(y, s.window = 'per', t.window = 50, t.jump = 1)

s1 = m1$time.series[,1]
t1 = m1$time.series[,2]
e1 = m1$time.series[,3]


#------------usar HW
(m <- HoltWinters(y))
 $ fitted : Time-Series [1:721, 1:4] from 2 to 105: 965 777 894 1164 1121 ...
 $ : chr [1:4] "xhat" "level" "trend" "season"

par(mfrow=c(1,1))

yhat = c(y[1:7],fitted(m)[,1])

#---------compara HW vs STL
plot(t[1:364],y[1:364],type='l',col='darkgray')
lines(t[1:364],yhat[1:364], col = 'red')

lines(t[1:364],(s1+t1)[1:364],col='blue')


#---------compara
require(forecast)
yhat=ts(yhat,frequency=364)
yhat.stl = ts(s1+t1,frequency=364)
(M = rbind(accuracy(yhat,y),accuracy(yhat.stl,y)))

#----------resultados diferentes, por qué?
source("medidas.hw.r")

(N=rbind(medidas.hw(as.numeric(y),as.numeric(yhat),3),
medidas.hw(as.numeric(y),as.numeric(yhat.stl),2)))


