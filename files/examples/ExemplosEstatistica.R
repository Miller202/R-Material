
url <- "C://CCEstatística 2020-1/TabelaLivro.csv"

milsa <- read.csv("TabelaLivro.csv", header = TRUE, sep = ";")
 
str(milsa)

civil.tb <- table(milsa$Est.civil)

#par (mfrow = c (1,2))

# GRAFICO DE Barras

barplot(civil.tb, cex.names=1.5, col=c("green", 
        "blue"), ylab="Número de Funcionários", 
        xlab="Estado civil", cex.axis=1.25,
        main="Proporção entre casados e solteiros",
        cex.lab=1.25, bty="n", ylim=c(0,25))

# GRAFICO DE PIZZA

labs<-paste(c("Casados = ", "Solteiros = "),
       round(civil.tb/length(milsa$Est.civil), 
       digits=2), "%")

pie(civil.tb,labels=labs,col=c("green", "blue"),
       main="Proporção entre casados e solteiros",
       cex=1.25)

#Plotando legenda no canto superior direito

legend("topright", pch=15, col=c("green","blue"), 
       legend=c("Casados", "Solteiros"),
       cex=1.1, bty="n")

## Variavel Qualitativa Ordinal 

## Frequência absoluta

inst.tb <- table(milsa$Inst)

## Frequência relativa
prop.table(inst.tb)

## Gráfico de Barras

barplot(sort(inst.tb,decreasing = FALSE),
        cex.names=1.15, 
        col=c("green", "blue", "red"),
        ylab="Instrução de Funcionários", 
        xlab="Escolaridade", cex.axis=1.25,
        main="Escolaridade dos Funcionários",
        cex.lab=1.25,bty="n", ylim=c(0,20))

barplot(sort(inst.tb,decreasing = TRUE),
        cex.names=1.15, 
        col=c("green", "blue", "red"),
        ylab="Instrução de Funcionários", 
        xlab="Escolaridade", cex.axis=1.25,
        main="Escolaridade dos Funcionários",
        cex.lab=1.25,bty="n", ylim=c(0,20))

## Variável quantitativa discreta

## Frequência absoluta

filhos.tb <- table(milsa$Filhos)
milsa$Filhos

plot(filhos.tb, col =  "green", type = "h",
     lwd = 5, cex.lab=1.2,
     main = " Frequência Absoluta ",
     xlab= "Número de filhos",
     ylab= "Quantidade de Filhos ") 

plot(filhos.tb, type = "S",col = "red",
     main = "Frequência relativa acumulada",
     lwd = 5 )

## Frequência relativa
filhos.tbr <- prop.table(filhos.tb)

## Frequência absoluta e relativa acumulada
filhos.tbra <- cumsum(filhos.tbr)
filhos.tba <- cumsum(filhos.tb)

filhosTabResul = cbind(filhos.tb,filhos.tba,
                 filhos.tbr = round(filhos.tbr*100,digits = 2),
                 filhos.tbra= round(filhos.tbra*100,digits = 2))
filhosTabResul

## Moda
names(filhos.tb)[which.max(filhos.tb)]
 
## Mediana
median(milsa$Filhos, na.rm = TRUE)
 
## Média
mean(milsa$Filhos, na.rm = TRUE)

## Quartis
quantile(milsa$Filhos, na.rm = TRUE)

## Máximo e mínimo
max(milsa$Filhos, na.rm = TRUE)

min(milsa$Filhos, na.rm = TRUE)

## As duas informações juntas
range(milsa$Filhos, na.rm = TRUE)

## Amplitude é a diferença entre máximo e mínimo
diff(range(milsa$Filhos, na.rm = TRUE))

## Variância
var(milsa$Filhos, na.rm = TRUE)

## Desvio-padrão
sd(milsa$Filhos, na.rm = TRUE)

## Coeficiente de variação
sd(milsa$Filhos, na.rm = TRUE)/mean(milsa$Filhos, na.rm = TRUE)
 
## Quartis
(filhos.qt <- quantile(milsa$Filhos, na.rm = TRUE))

## Summary() para resumir os dados de uma só vez
summary(milsa$Filhos)

## Variável quantitativa Continua

Salario.tb <- (milsa$Salario)
sort (Salario.tb)

Amplitude <- max (Salario.tb) - min(Salario.tb) ; Amplitude
NK <-  round( 1 + 3.222 * log10(length(Salario.tb))) ; NK #número de classes
AmpClasse <- Amplitude / NK ; AmpClasse ; AmpClasse <- 3.25

limitesclas <- c(4 ,7.25 ,10.50 ,13.75 ,17.00 ,20.25, 23.50)

classes<-c("04.00-07.25","07.25-10.50","10.50-13.75",
           "13.75-17.00","17.00-20.25", "20.25-23.50")

# Freq - absoluta, FreqAc - absoluta acumulada
# FreqRel - relativa, FreqRelAc - relativa acumulada

Freq = table(cut(Salario.tb, breaks = limitesclas, right=FALSE, labels=classes))

FreqAc <- cumsum(Freq); FreqRel <- prop.table(Freq); FreqRelAc <- cumsum(FreqRel)

TabResul = cbind(Freq,FreqAc, FreqRel = round(FreqRel*100,digits = 2),
                              FreqRelAc= round(FreqRelAc*100,digits = 2))
TabResul

h = hist(Salario.tb, breaks=limitesclas,
    ylab="Frequencias absolutas",  labels=classes,main="Histograma", 
    xlim=c(4,25), ylim = c (0,12), col="orange")

lines(c(min(h$breaks), h$mids, max(h$breaks)), 
       c(0,h$counts, 0), type = "l")
       
## Mediana
       median(milsa$Salario)
## Média
       mean(milsa$Salario)
       
## Quartis
       quantile(milsa$Salario)
       
## As duas informações juntas
       range(milsa$Salario)
       
## Amplitude é a diferença entre máximo e mínimo
       diff(range(milsa$Salario))
       
## Variância
       var(milsa$Salario)
## Desvio-padrão
       sd(milsa$Salario)
       
## Coeficiente de variação
       sd(milsa$Salario)/mean(milsa$Salario)
       
## Quartis
       (Salario.qt <- quantile(milsa$Salario))
       
## Summary() para resumir os dados de uma só vez
       summary(milsa$Salario)

par (mfrow = c (1,1))  

boxplot(milsa$Salario)

boxplot(milsa$Salario,  col = "orange", varwidth = TRUE, notch = TRUE)

## Qualitativa vs quantitativa (variáveis Instrução e Salário)
## Quartis de salario

quantile(milsa$Salario)

## Classificação de acordo com os quartis
salario.cut <- cut(milsa$Salario, breaks =  quantile(milsa$Salario),
                   include.lowest = TRUE)
## Tabela de frequências absolutas
inst.sal.tb <- table(milsa$Inst, salario.cut)
inst.sal.tb

barplot(inst.sal.tb, col=c("yellow","red","orange"), main= "Salário x Instrução",
        xlab = "Quantiles", ylab = "Frequência  Instrução",
        beside = TRUE, legend = TRUE)

boxplot(Salario ~ Instrucao, data = milsa, col=c("yellow", "red", "orange"))  

prop.table(inst.sal.tb)

## Quantitativa vs Quantitativa (considerar as variáveis Salario e Idade)

Anos.cut <- cut(milsa$Anos, breaks = quantile(milsa$Anos),include.lowest = TRUE)

salario.cut <- cut(milsa$Salario, breaks = quantile(milsa$Salario),
                   include.lowest = TRUE)
## Tabela cruzada

Anos.sal.tb <- table(Anos.cut, salario.cut)

plot(x = milsa$Anos, y = milsa$Salario)
plot(Salario ~ Anos, data = milsa)

## Correlação - verificar associação entre variaveis quatitativas

cor(milsa$Anos, milsa$Salario)
cor(milsa$Anos, milsa$Salario, method = "kendall")
cor(milsa$Anos, milsa$Salario, method = "spearman")
       