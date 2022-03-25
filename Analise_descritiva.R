############Análises Descritiva###################
library(pastecs)
library(corrplot)
library(xts)
library(forecast)

dados = read.csv2("C:/Users/COPEL3/Documents/Informa/Compessa/pressao_vazao_tratamento_rod.csv" , sep="," , dec=".")
attach(dados)

dim(dados)    # mostra a dimensão dos dados
names(dados)  # mostra os nomes das variáveis
str(dados)
summary(dados) # medidas de posição para variáveis


#Variáveis categóricas
dados <- within(dados, {
  Turno <- factor(Turno, labels=c('Manhã','Tarde', 'Noite', 'Madrugada'))
})

dados <- within(dados, {
  Estacao <- factor(Estacao, labels=c('Verão','Outono'))
})

par(bg="#fdf6e3") 
par(mfrow=c(1,2),bg="#fdf6e3") 

contagem = table(Turno)
nomes = c('Manhã','Tarde', 'Noite', 'Madrugada')
porcent = round(contagem/sum(contagem)*100,2)
rotulo=paste(nomes," (",porcent,"%",")",sep="")
pie(table(Turno),labels=rotulo, main="Turnos", col=c("#99CCFF","#99CCCC","#9999FF", "#99FFCC"))  

contagem2 = table(Estacao)
nomes2 = c('Verão','Outono')
porcent2 = round(contagem2/sum(contagem2)*100,2)
rotulo2=paste(nomes2," (",porcent2,"%",")",sep="")
pie(table(Estacao),labels=rotulo2, main="Estação", col=c("#99CCCC","#99CCFF"))  

#distribuicao de frequencia univariada
dFrequencia.turno = table(Turno)
dFrequencia.estacao = table(Estacao)
plot(dFrequencia.turno)
plot(dFrequencia.estacao)

install.packages("qcc")
library("qcc")

par(bg="#fdf6e3") 
par(mfrow=c(1,2),bg="#fdf6e3") 
pareto.chart(dFrequencia.turno)
pareto.chart(dFrequencia.estacao)

#distribuições bivariadas
par(bg="#fdf6e3") 
par(mfrow=c(3,2),bg="#fdf6e3") 
boxplot(PressÃ.o...PC.52..mca. ~ Turno, xlab = "Turno", ylab = "Pressão PC", 
        main = "Pressão PC vs Turnos")
boxplot(Pressao...PM.D52..mca. ~ Turno, xlab = "Turno", ylab = "Pressão PM", 
        main = "Pressão PM vs Turnos")
boxplot(Vazao..L.s. ~ Turno, xlab = "Turno", ylab = "Vazão", 
        main = "Vazão vs Turnos")

boxplot(PressÃ.o...PC.52..mca. ~ Estacao, xlab = "Estação", ylab = "Pressão PC", 
        main = "Pressão PC vs Estações")
boxplot(Pressao...PM.D52..mca. ~ Estacao, xlab = "Estação", ylab = "Pressão PM", 
        main = "Pressão PM vs Estações")
boxplot(Vazao..L.s. ~ Estacao, xlab = "Estação", ylab = "Vazão", 
        main = "Vazão vs Estações")

#Variáveis numéricas
descr <- stat.desc(dados[2:4]) # Medidas descritivas
round(descr, 2)  #arredondar para 2 casas decimais

cat1 = replicate(3745, "PC")
cat2 = replicate(3745, "PM")
pressao = c(PressÃ.o...PC.52..mca., Pressao...PM.D52..mca.)
categoria = c(cat1, cat2)

boxplot(pressao~categoria, col = c("lightblue", "lightgreen"),
        boxwex=0.4, xlab="", ylab="Pressão", main = "Boxplots das Variáveis Pressão")

boxplot(Vazao..L.s., col = "lightgreen",
        boxwex=0.4, xlab="", ylab="Vazão", main = "Boxplots da Variável Vazão")

## Histogramas
hist(PressÃ.o...PC.52..mca.,prob=T,main='Histograma da pressão PC', xlab="Pressão PC", ylab="Densidade")
lines(density(PressÃ.o...PC.52..mca.),col='red')
rug(PressÃ.o...PC.52..mca.)

hist(Pressao...PM.D52..mca.,prob=T,main='Histograma da pressão PM', xlab="Pressão PM", ylab="Densidade")
lines(density(Pressao...PM.D52..mca.),col='red')
rug(Pressao...PM.D52..mca.)

hist(Vazao..L.s.,prob=T,main='Histograma da vazão', xlab="Vazão", ylab="Densidade")
lines(density(Vazao..L.s.),col='red')
rug(Vazao..L.s.)

plot(Vazao..L.s., PressÃ.o...PC.52..mca., main="Pressão Ponto Crítico x Vazão", 
     xlab="Pressão PC", ylab="Vazão", pch=20)   
     abline(lm(PressÃ.o...PC.52..mca. ~ Vazao..L.s.), col="red")

plot(Vazao..L.s., Pressao...PM.D52..mca., main="Pressão Ponto Médio x Vazão", 
     xlab="Pressão PM", ylab="Vazão", pch=20)   
     abline(lm(Pressao...PM.D52..mca.~Vazao..L.s.), col = "red")  

#Correlação
corrplot(cor(dados[2:4]), method = "number", type = "lower", diag = TRUE)
