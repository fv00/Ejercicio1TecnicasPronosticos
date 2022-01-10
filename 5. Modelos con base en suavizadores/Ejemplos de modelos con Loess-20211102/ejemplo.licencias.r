# Analisis de Numero de Licencias

# leer
G = read.table("series_histo_88_mpio_jun17.prn", 
header = TRUE, stringsAsFactors = FALSE)
attach(G)

# definir como serie de tiempo
y = ts(G$Totalviv,frequency=12,start=c(2009,1))
ts.plot(y)

library(Kendall)
# detectar tendencia monotona
# Ho: no hay tendencia, Ha: tendencia monotona
MannKendall(y)

#---------------utilizar loess para estimar tendencia


m = 12
n = length(y)
yi = ts(y[1:(n-m)],frequency=12)
yf = ts(y[(n-m+1):n],frequency=12)

t = seq(1,(n-m))
yw = loess(yi ~ t, span=0.5,
control = loess.control(surface = "direct"))
yest.yw = yw$fitted


# grafica con fechas en el eje x

np = length(yi)

fecha = seq(as.Date("2009/01/01"), length.out=(n-m), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,yi, xaxt="n", panel.first = grid(),type='l',
ylab='licencias.mes', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

lines(fecha,yest.yw, xaxt="n", panel.first = grid(),
type='l',col='red',lty=2,lwd=2)

lines(fecha,Tt.stl, xaxt="n", panel.first = grid(),
type='l',col='blue',lty=7,lwd=2)

#---------------utilizar loess para estimar tendencia

library(forecast)

dummy=function(s,n,i){ 
# s: estaciones, n: numero de datos, i: estación de inicio
A=diag(s)
for(j in 1:(floor(n/s))){ 
A=rbind(A,diag(s))}
A=A[i:(n+i-1),]
return(A)}

It = dummy(12,length(yi),1)

y.st = yi - yest.yw

mod1= lm(y.st ~ -1+ It)
summary(mod1)
plot(coef(mod1),type='b')

#-------------pronostico modelo híbrido

tt = seq((n-m+1),n,1)
pr.yw = predict(yw, data.frame(t = tt))


Itt = dummy(12,m,7)

pr.st = predict(mod1,data.frame(It=I(Itt)))

pr.y = pr.yw+pr.st


#-------------pronostico con STL
# descomposicion stl
y.stl = stl(yi,"per")
plot(y.stl)
St.stl = y.stl$time.series[,1]
Tt.stl = y.stl$time.series[,2]

pr.stl = forecast(y.stl,,method="ets",h=m)$mean

#---------------grafica pronosticos
plot(tt,yf,type='b',lwd=1,ylim=c(8,21))
lines(tt,pr.y,col='red',lwd=1)
lines(tt,pr.stl,col='blue',lwd=1)
legend("topleft", 
c("Obs","hibrido:Loess+Ind","STL"), 
pch = c(1, 3, 5),
col = c("black","red","blue"))

#--------------compara calidad pronosticos
pr.y = ts(pr.y,frequency=12,start=c(2016,06))
pr.stl = ts(pr.stl,frequency=12,start=c(2016,06))
yf = ts(yf,frequency=12,start=c(2016,06))
M = rbind(accuracy(pr.y,yf),accuracy(pr.stl,yf))
rownames(M) = c("hibrido:Loess+Ind","STL")
(M)
require(xtable)
print(xtable(M),digits=3)



