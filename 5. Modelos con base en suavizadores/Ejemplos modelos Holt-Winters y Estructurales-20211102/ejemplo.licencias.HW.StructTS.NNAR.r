# Analisis de Numero de Licencias
# Modelos  Holt-Winters, BSM, NNAR
# leer
G = read.table("series_histo_88_mpio_jun17.prn", 
header = TRUE, stringsAsFactors = FALSE)
attach(G)

# definir como serie de tiempo
y = ts(G$Totalviv,frequency=12,start=c(2009,1))
ts.plot(y)

m = 12
n = length(y)
yi = ts(y[1:(n-m)],frequency=12)
yf = ts(y[(n-m+1):n],frequency=12)

#-------------------- grafica con fechas en el eje x

np = length(yi)

fecha = seq(as.Date("2009/01/01"), length.out=(n-m), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)



#-----------------------aplicar Holt-Winters

y.hw = HoltWinters(yi)
(c(y.hw$alpha,y.hw$beta,y.hw$gamma))
# la tendencia, componente estacional y y estimada

yest.hw = fitted(y.hw)[,1]

Tt = fitted(y.hw)[,2] + fitted(y.hw)[,3] 
St = fitted(y.hw)[,4]

# y 12 pronosticos se calculan como
pr.hw = predict(y.hw,12) # prediction.interval = TRUE)

source("medidas.r")

source("medidas.struc.r")

H = medidas.struc(yi,yest.hw,3)

#---------------------------aplicar TS-Struct

y.bsm = StructTS(yi)
print(y.bsm)

Variances:
    level      slope       seas    epsilon  
7.085e-01  5.056e-04  4.524e-01  1.006e+01  

str(y.bsm)
fitted(y.bsm)
# "level" "slope" "sea"

yest.bsm = apply(fitted(y.bsm),1,sum)

require(forecast)
B=forecast(y.bsm,h=m)
str(B)
pr.bsm = ts(B$mean,frequency=12, start=c(2016,06))


H = rbind(H,medidas.struc(yi,yest.bsm,3))


#-----------------aplicar red neuronal

y.nnar = nnetar(yi,lambda=0)

# y.nnar = nnetar(yi,p=2,P=1,size=3,lambda=0)

print(y.nnar)

str(y.nnar)

yest.nnar= fitted(y.nnar)

H = rbind(H,medidas.struc(yi[!is.na(yest.nnar)],na.omit(yest.nnar),10))

rownames(H)=c("H-W","BSM","NNAR")
(H)

pr.nnar = forecast(y.nnar,h=m)$mean

#-------------grafica
plot(fecha,yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 2,col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

lines(fecha[-seq(1,12)],yest.hw, xaxt="n", panel.first = grid(),
type='l',col='red')

lines(fecha,yest.bsm, xaxt="n", panel.first = grid(),
type='l',col='blue',lty=3,lwd=2)

lines(fecha[-seq(1,12)],na.omit(yest.nnar), xaxt="n", panel.first = grid(),
type='l',col='orange',lty=1,lwd=2)

#--------------compara calidad pronosticos
pr.y = ts(pr.bsm,frequency=12,start=c(2016,06))
pr.hw = ts(pr.hw,frequency=12,start=c(2016,06))
pr.nnar = ts(pr.nnar,frequency=12,start=c(2016,06))

yf = ts(yf,frequency=12,start=c(2016,06))

M = rbind(accuracy(pr.hw,yf),accuracy(pr.bsm,yf),
accuracy(pr.nnar,yf))
rownames(M) = c("Holt-Winters","BSM","NNAR")
(M)
require(xtable)
?
print(xtable(M),digits=3)


#---------------grafica pronosticos
tt = seq(1,m)
plot(tt,yf,type='b',lwd=1,ylim=c(8,21))
lines(tt,pr.hw,col='red',lwd=1)
lines(tt,pr.bsm,col='blue',lwd=1)
lines(tt,pr.nnar,col='orange',lwd=1)
legend("topleft", 
c("Obs","Holt-Winters","BSM","NNAR"), 
pch = c(1,2, 3, 5),
col = c("black","red","blue","orange"))


