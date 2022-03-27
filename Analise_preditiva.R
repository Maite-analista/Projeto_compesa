#########Análise Preditiva##############
library (forecast)
library(lubridate)
library(stringr)

dados = read.csv2("C:/Users/COPEL3/Documents/Informa/Compessa/pressao_vazao_tratamento_rod.csv" , sep="," , dec=".")
attach(dados)

#PARTICIONAR A SERIE EM horas ou dias
particionar <- function(x, p){
  dias <- split(x, f = p)
  qdias <- length(dias)
  amostra <- list()
  for (j in 1:qdias) 
  {
    #nam <- paste("dia",j, sep=".")
    #temp1 <- assign(nam, coredata(dias[[j]]))
    #amostra [[j]] <- data.frame(Dia = sample(nam, 1), temp1)
    amostra[[j]] <- coredata(dias[[j]])
  }
  return(amostra)
}

#agrupando por unidade de tempo
boxplot(PressÃ.o...PC.52..mca.~Dia.da.Semana, xlab="Dia da semana", ylab = "Pressão PC (mca)")
boxplot(Pressao...PM.D52..mca.~Dia.da.Semana, xlab="Dia da semana", ylab = "Pressão PM (mca)")
boxplot(Vazao..L.s.~Dia.da.Semana, xlab="Dia da semana", ylab = "Vazão (L/s)")
Dia.da.Semana[Dia.da.Semana=="Domingo"]=1#codificando
Dia.da.Semana[Dia.da.Semana=="Segunda"]=2
Dia.da.Semana[Dia.da.Semana=="Terca"]=3
Dia.da.Semana[Dia.da.Semana=="Quarta"]=4
Dia.da.Semana[Dia.da.Semana=="Quinta"]=5
Dia.da.Semana[Dia.da.Semana=="Sexta"]=6
Dia.da.Semana[Dia.da.Semana=="Sabado"]=7

boxplot(PressÃ.o...PC.52..mca.~Mes, xlab="Mês", ylab = "Pressão PC (mca)")
boxplot(Pressao...PM.D52..mca.~Mes, xlab="Mês", ylab = "Pressão PM (mca)")
boxplot(Vazao..L.s.~Mes, xlab="Mês", ylab = "Vazão (L/s)")

#estudo cada 1 hora
Data.e.hora <- ymd_hms(Data.e.hora)
Data <- Data.e.hora

#Agrupar por horas 
x <- as.xts(x = PressÃ.o...PC.52..mca., order.by = Data)
f = "hours"
amostras_x <- particionar(x, f)

ST_medias_PPC <- sapply(amostras_x,mean)

#serie_ppc <- msts(PressÃ.o...PC.52..mca., seasonal.periods = 1000)
ts.plot(ST_medias_PPC, main='Série de Pressão PC por Hora',
        gpars=list(xlab="", ylab="Pressão PC", 
                   col="black", lty=c(1:3), 
                   xaxt="n", lwd = 2))
#datas = names(table(data_so))
nDatas= length(Horario); at_ = seq(1, nDatas, by = 4)
axis(side = 1,at = at_,  labels = Horario[at_], las = 2)

#modelando via regressão linear
n = length(ST_medias_PPC)
ut = ST_medias_PPC[3:n]
ut_1 = ST_medias_PPC[2:(n-1)]
ut_2 = ST_medias_PPC[1:(n-2)]
data = Horario[3:n]
Pressao_PC_lag2 = data.frame(data, ut, ut_1, ut_2)
modelo.presao.PC.LM = lm(ut~ut_1+ut_2, 
                 data = Pressao_PC_lag2)
summary(modelo.presao.PC.LM)

lm.coef = round(as.numeric(modelo.presao.PC.LM$coefficients), 2)
ts.plot(cbind(ST_medias_PPC, c(rep(NA, 2), modelo.presao.PC.LM$fitted.values)), 
        gpars=list(xlab="", ylab="Pressão PC (mca)", 
                   col=c("black", "red"), lty=c(1,2), 
                   xaxt="n", lwd = 2))
legend("topleft", legend = c(expression(paste("observado: ", u[t])), 
                                bquote(paste("previsto via LM:  ", hat(u)[t] == .(lm.coef[1]) + .(lm.coef[2])*u[t-1] + .(lm.coef[3])*u[t-2]))), 
       col = c("black", "red"), lty=c(1:2), lwd = 2, 
       bty = "n")
axis(side = 1,at = at_,  labels = Horario[at_], las = 2)

#estudando o erro do modelo.A.LM
residuos.PPC.LM= modelo.presao.PC.LM$residuals
summary(residuos.PPC.LM)
mean.residuos.PPC.LM = mean(residuos.PPC.LM)
sd.residuos.PPC.LM = sd(residuos.PPC.LM)
ll = mean.residuos.PPC.LM - 3.5*sd.residuos.PPC.LM
ul = mean.residuos.PPC.LM + 3.5*sd.residuos.PPC.LM

curve(dnorm(x, mean = mean.residuos.PPC.LM, sd = sd.residuos.PPC.LM), 
      from = ll, to = ul, col = "blue", lwd=2, 
      xlab = "resíduo para a pressão PC (observado - previsto via LM)", 
      ylab = "densidade")
hist(residuos.PPC.LM, freq = FALSE, add = TRUE, col = NULL)
sh.test = shapiro.test(residuos.PPC.LM)
text(x = -4, y = 0.25, adj=0, bquote(paste("p* = P(W>=",.(round(sh.test$statistic, 2)), ")=", .(round(sh.test$p.value, 2)))))

#modelagem via ARIMA
library("forecast")
set.seed(0)
modelo.PPC.ARIMA = auto.arima(y = ST_medias_PPC)
summary(modelo.PPC.ARIMA)
ts.plot(cbind(ST_medias_PPC, modelo.PPC.ARIMA$fitted), 
        gpars=list(xlab="", ylab="Pressão PC (mca)", 
                   col=c("black", "red"), lty=c(1,2), 
                   xaxt="n", lwd = 2))
legend("topright", legend = c(expression(paste("observado: ", u[t])), 
                                expression(paste("previsto via ARIMA:  ", hat(u)[t] == 1.52*u[t-1] - 0.52*u[t-2]))), 
       col = c("black", "red"), lty=c(1:2), lwd = 2, 
       bty = "n")
axis(side = 1,at = at_,  labels = data[at_], las = 2)
#estudando o erro do modelo.A.ARIMA
residuos.PPC.ARIMA= modelo.PPC.ARIMA$residuals
summary(residuos.PPC.ARIMA)
mean.residuos.PPC.ARIMA = mean(residuos.PPC.ARIMA)
sd.residuos.PPC.ARIMA = sd(residuos.PPC.ARIMA)
ll = mean.residuos.PPC.ARIMA - 3.5*sd.residuos.PPC.ARIMA
ul = mean.residuos.PPC.ARIMA + 3.5*sd.residuos.PPC.ARIMA
curve(dnorm(x, mean = mean.residuos.PPC.ARIMA, sd = sd.residuos.PPC.ARIMA), 
      from = ll, to = ul, col = "blue", lwd=2, 
      xlab = "resíduo para A (observado - previsto via ARIMA)", 
      ylab = "densidade")
hist(residuos.PPC.ARIMA, freq = FALSE, add = TRUE, col = NULL)
sh.test = shapiro.test(residuos.PPC.ARIMA)
text(x = -5, y = 0.25, adj=0, bquote(paste("p* = P(W>=",.(round(sh.test$statistic, 2)), ")=", .(round(sh.test$p.value, 2)))))

#modelando via ANN
library(neuralnet)
min_ = min(Pressao_PC_lag2[,2:4])
max_ = max(Pressao_PC_lag2[,2:4])
normalized.Pressao_PC_lag2 = (Pressao_PC_lag2[,2:4] - min_)/(max_-min_)
modelo.PPC.ANN = neuralnet(ut~ut_1+ut_2, 
                         data = normalized.Pressao_PC_lag2, 
                         linear.output = FALSE)
plot(modelo.PPC.ANN)
modelo.PPC.ANN$act.fct
modelo.PPC.ANN.forecasts = c(modelo.PPC.ANN$net.result[[1]]*(max_-min_) + min_)
View(cbind(Pressao_PC_lag2, modelo.PPC.ANN.forecasts))
ts.plot(cbind(ST_medias_PPC, c(NA, NA, modelo.PPC.ANN.forecasts)), 
        gpars=list(xlab="", ylab="Pressão PC (mca)", 
                   col=c("black", "red"), lty=c(1,2), 
                   xaxt="n", lwd = 2))
legend("topright", legend = c(expression(paste("observado: ", u[t])), 
                                expression(paste("previsto via ANN:  ", hat(u)[t] == f(u[t-1], u[t-2])))), 
       col = c("black", "red"), lty=c(1:2), lwd = 2, 
       bty = "n")
axis(side = 1,at = at_,  labels = data[at_], las = 2)

#estudando o erro do modelo.A.ANN
residuos.PPC.ANN= Pressao_PC_lag2$ut- modelo.PPC.ANN.forecasts
summary(residuos.PPC.ANN)
mean.residuos.PPC.ANN = mean(residuos.PPC.ANN, na.rm = TRUE)
sd.residuos.PPC.ANN = sd(residuos.PPC.ANN, na.rm = TRUE)
ll = mean.residuos.PPC.ANN - 3.5*sd.residuos.PPC.ANN
ul = mean.residuos.PPC.ANN + 3.5*sd.residuos.PPC.ANN
curve(dnorm(x, mean = mean.residuos.PPC.ANN, sd = sd.residuos.PPC.ANN), 
      from = ll, to = ul, col = "blue", lwd=2, 
      xlab = "resíduo para Pressão PPC (observado - previsto via ANN)", 
      ylab = "densidade")
hist(residuos.PPC.ANN, freq = FALSE, add = TRUE, col = NULL)
sh.test = shapiro.test(residuos.PPC.ANN)
text(x = -5, y = 0.25, adj=0, bquote(paste("p* = P(W>=",.(round(sh.test$statistic, 2)), ")=", .(round(sh.test$p.value, 2)))))

#resumindo desempenhos
cbind(residuos.PPC.LM = summary(residuos.PPC.LM), 
      residuos.PPC.ARIMA = summary(residuos.PPC.ARIMA), 
      residuos.PPC.ANN = summary(residuos.PPC.ANN))
residuos = data.frame(residuo=c(residuos.PPC.LM, residuos.PPC.ARIMA, residuos.PPC.ANN), 
                      modelo=c(rep("PPC.LM", length(residuos.PPC.LM)), 
                               rep("PPC.ARIMA", length(residuos.PPC.ARIMA)),
                               rep("PPC.ANN", length(residuos.PPC.ANN))))
boxplot(residuos$residuo ~residuos$modelo, xlab = "modelo de previsão para valor da pressão PC", ylab = "resíduo")
rbind(EQM.residuos.PPC.LM = mean(residuos.PPC.LM^2), 
      EQM.residuos.PPC.ARIMA = mean(residuos.PPC.ARIMA^2), 
      EQM.residuos.PPC.ANN = mean(residuos.PPC.ANN^2))


#estudo diario
#Agrupar por dia 
x <- as.xts(x = PressÃ.o...PC.52..mca., order.by = Data)
f = "days"
amostras_x <- particionar(x, f)

ST_medias_PPC <- sapply(amostras_x,mean)

ind <- numeric(length = length(amostras_x))
fechas <- numeric(length = length(amostras_x))

temp = 0
for(i in 1:length(amostras_x)){
  temp1 <- length(amostras_x[[i]]) 
  temp = temp + temp1
  ind[i] = temp
  fechas[i] = temp
}

fechas = ymd(fechas)
  
for(j in 1:length(amostras_x)){
  temp2 = Data.e.hora[ind[j]] 
  fechas[j] = temp2
}

ts.plot(ST_medias_PPC, main='Série de Pressão PC por Dias',
        gpars=list(xlab="Dias", ylab="Pressão PC", 
                   col="black", lty=c(1:3), 
                   xaxt="n", lwd = 2))
#datas = names(table(data_so))
nDatas= length(fechas); at_ = seq(1, nDatas, by = 1)
axis(side = 1,at = at_,  labels = fechas[at_], las = 2)

#modelando via regressão linear
n = length(ST_medias_PPC)
ut = ST_medias_PPC[2:n]
ut_1 = ST_medias_PPC[1:(n-1)]
data = fechas[2:n]
Pressao_PC_lag1 = data.frame(data, ut, ut_1)
modelo.presao.PC.LM = lm(ut~ut_1, 
                         data = Pressao_PC_lag1)
summary(modelo.presao.PC.LM)

lm.coef = round(as.numeric(modelo.presao.PC.LM$coefficients), 2)
ts.plot(cbind(ST_medias_PPC, c(rep(NA, 1), modelo.presao.PC.LM$fitted.values)), 
        gpars=list(xlab="", ylab="Pressão PC (mca)", 
                   col=c("black", "red"), lty=c(1,2), 
                   xaxt="n", lwd = 2))
legend("bottomright", legend = c(expression(paste("observado: ", u[t])), 
                             bquote(paste("previsto via LM:  ", hat(u)[t] == .(lm.coef[1]) + .(lm.coef[2])*u[t-1] ))), 
       col = c("black", "red"), lty=c(1:2), lwd = 2,bty = "n")
axis(side = 1,at = at_,  labels = fechas[at_], las = 2)

#estudando o erro do modelo.A.LM
residuos.PPC.LM= modelo.presao.PC.LM$residuals
summary(residuos.PPC.LM)
mean.residuos.PPC.LM = mean(residuos.PPC.LM)
sd.residuos.PPC.LM = sd(residuos.PPC.LM)
ll = mean.residuos.PPC.LM - 3.5*sd.residuos.PPC.LM
ul = mean.residuos.PPC.LM + 3.5*sd.residuos.PPC.LM

curve(dnorm(x, mean = mean.residuos.PPC.LM, sd = sd.residuos.PPC.LM), 
      from = ll, to = ul, col = "blue", lwd=2, 
      xlab = "resíduo para a pressão PC (observado - previsto via LM)", 
      ylab = "densidade")
hist(residuos.PPC.LM, freq = FALSE, add = TRUE, col = NULL)
sh.test = shapiro.test(residuos.PPC.LM)
text(x = -5, y = 0.20, adj=0, bquote(paste("p* = P(W>=",.(round(sh.test$statistic, 2)), ")=", .(round(sh.test$p.value, 2)))))

#modelagem via ARIMA
set.seed(0)
modelo.PPC.ARIMA = auto.arima(y = ST_medias_PPC)
summary(modelo.PPC.ARIMA)
ts.plot(cbind(ST_medias_PPC, modelo.PPC.ARIMA$fitted), 
        gpars=list(xlab="", ylab="Pressão PC (mca)", 
                   col=c("black", "red"), lty=c(1,2), 
                   xaxt="n", lwd = 2))
legend("bottomright", legend = c(expression(paste("observado: ", u[t])), 
                              expression(paste("previsto via ARIMA:  ", hat(u)[t] == 1.52*u[t-1] - 0.52*u[t-2]))), 
       col = c("black", "red"), lty=c(1:2), lwd = 2, 
       bty = "n")
axis(side = 1,at = at_,  labels = fechas[at_], las = 2)
#estudando o erro do modelo.A.ARIMA
residuos.PPC.ARIMA= modelo.PPC.ARIMA$residuals
summary(residuos.PPC.ARIMA)
mean.residuos.PPC.ARIMA = mean(residuos.PPC.ARIMA)
sd.residuos.PPC.ARIMA = sd(residuos.PPC.ARIMA)
ll = mean.residuos.PPC.ARIMA - 3.5*sd.residuos.PPC.ARIMA
ul = mean.residuos.PPC.ARIMA + 3.5*sd.residuos.PPC.ARIMA
curve(dnorm(x, mean = mean.residuos.PPC.ARIMA, sd = sd.residuos.PPC.ARIMA), 
      from = ll, to = ul, col = "blue", lwd=2, 
      xlab = "resíduo para A (observado - previsto via ARIMA)", 
      ylab = "densidade")
hist(residuos.PPC.ARIMA, freq = FALSE, add = TRUE, col = NULL)
sh.test = shapiro.test(residuos.PPC.ARIMA)
text(x = -6, y = 0.20, adj=0, bquote(paste("p* = P(W>=",.(round(sh.test$statistic, 2)), ")=", .(round(sh.test$p.value, 2)))))

#modelando via ANN
min_ = min(Pressao_PC_lag1[,2:3])
max_ = max(Pressao_PC_lag1[,2:3])
normalized.Pressao_PC_lag1 = (Pressao_PC_lag1[,2:3] - min_)/(max_-min_)
modelo.PPC.ANN = neuralnet(ut~ut_1, 
                           data = normalized.Pressao_PC_lag1, 
                           linear.output = FALSE)
plot(modelo.PPC.ANN)
modelo.PPC.ANN$act.fct
modelo.PPC.ANN.forecasts = c(modelo.PPC.ANN$net.result[[1]]*(max_-min_) + min_)
View(cbind(Pressao_PC_lag1, modelo.PPC.ANN.forecasts))
ts.plot(cbind(ST_medias_PPC, c(NA, modelo.PPC.ANN.forecasts)), 
        gpars=list(xlab="", ylab="Pressão PC (mca)", 
                   col=c("black", "red"), lty=c(1,2), 
                   xaxt="n", lwd = 2))
legend("bottomright", legend = c(expression(paste("observado: ", u[t])), 
                              expression(paste("previsto via ANN:  ", hat(u)[t] == f(u[t-1])))), 
       col = c("black", "red"), lty=c(1:2), lwd = 2, 
       bty = "n")
axis(side = 1,at = at_,  labels = fechas[at_], las = 2)

#estudando o erro do modelo.A.ANN
residuos.PPC.ANN= Pressao_PC_lag1$ut- modelo.PPC.ANN.forecasts
summary(residuos.PPC.ANN)
mean.residuos.PPC.ANN = mean(residuos.PPC.ANN, na.rm = TRUE)
sd.residuos.PPC.ANN = sd(residuos.PPC.ANN, na.rm = TRUE)
ll = mean.residuos.PPC.ANN - 3.5*sd.residuos.PPC.ANN
ul = mean.residuos.PPC.ANN + 3.5*sd.residuos.PPC.ANN
curve(dnorm(x, mean = mean.residuos.PPC.ANN, sd = sd.residuos.PPC.ANN), 
      from = ll, to = ul, col = "blue", lwd=2, 
      xlab = "resíduo para Pressão PPC (observado - previsto via ANN)", 
      ylab = "densidade")
hist(residuos.PPC.ANN, freq = FALSE, add = TRUE, col = NULL)
sh.test = shapiro.test(residuos.PPC.ANN)
text(x = -5, y = 0.25, adj=0, bquote(paste("p* = P(W>=",.(round(sh.test$statistic, 2)), ")=", .(round(sh.test$p.value, 2)))))

#resumindo desempenhos
cbind(residuos.PPC.LM = summary(residuos.PPC.LM), 
      residuos.PPC.ARIMA = summary(residuos.PPC.ARIMA), 
      residuos.PPC.ANN = summary(residuos.PPC.ANN))
residuos = data.frame(residuo=c(residuos.PPC.LM, residuos.PPC.ARIMA, residuos.PPC.ANN), 
                      modelo=c(rep("PPC.LM", length(residuos.PPC.LM)), 
                               rep("PPC.ARIMA", length(residuos.PPC.ARIMA)),
                               rep("PPC.ANN", length(residuos.PPC.ANN))))
boxplot(residuos$residuo ~residuos$modelo, xlab = "modelo de previsão para valor da pressão PC", ylab = "resíduo")
rbind(EQM.residuos.PPC.LM = mean(residuos.PPC.LM^2), 
      EQM.residuos.PPC.ARIMA = mean(residuos.PPC.ARIMA^2), 
      EQM.residuos.PPC.ANN = mean(residuos.PPC.ANN^2))





