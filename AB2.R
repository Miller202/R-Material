### INTERVALO DE CONFIANÇA




## Estimativa pontual

# Exemplo:
# Considere a seguinte amostra de observações no módulo de elasticidade (GPa) de espécimes
# de um processo de fundição. X: 44,2; 43,9; 44,7; 44,2; 44,0; 43,8; 44,6; 43,1.
# Obter uma estimativa para média, variância e desvio padrão da amostra.

x <- c(44.2, 43.9, 44.7, 44.2, 44.0, 43.8, 44.6, 43.1)
mean(x) # media
var(x)  # variancia
sd(x)   # desvio padrão




## Intervalo de confiança da média - VARIÂNCIA CONHECIDA

# Exemplo:
# Uma máquina produz rolamentos que apresentam desvio padrão de '0,042' polegadas em seu
# diâmetro. Desejando-se conhecer o diâmetro médio dos rolamentos produzidos por esta 
# máquina, extraiu-se uma amostra de '100' rolamentos, observando-se uma média igual a 
# '0,824' polegadas: Obter o intervalo com '0,90' de confiança para o verdadeiro 
# diâmetro médio dos rolamentos.

alfa = 0.10 # Obs: 1 - alfa = 90%, logo, alfa = 10%
desvio = 0.042
media = 0.824
n = 100

zc = qnorm(1 - alfa/2, 0, 1) # procura-se o valor na tabela Z
zc = round(zc, 2)
zc

erro = zc * desvio / sqrt(n) # fórmula para encontrar o e0
erro = round(erro, 5)
erro

cat("(", media - erro, ",", media + erro, ")") # intervalo obtido




## Intervalo de confiança da média - VARIÂNCIA DESCONHECIDA

# Exemplo 1:
# Em um processo para obtenção de compostos químicos de tintas, obteve-se os seguintes
# tempos: 90, 92, 92, 95, 98, 99, 100, 100, 100 e 117 segundos.
# Construir um intervalo com nível confiança de 95%.

valores <- c(90, 92, 92, 95, 98, 99, 100, 100, 100, 117)

# utilizando a função t.test

t.test(valores, conf = 0.95)

# sem uso da função

alfa = 0.05
n = length(valores)
desvio = sd(valores)
media = mean(valores)

tc = qt(p = 1 - alfa/2, df = n - 1)
tc = round(tc, 3)

erro = tc * desvio / sqrt(n)
erro = round(erro, 3)

cat("(", media - erro, ",", media + erro, ")")


# Exemplo 2:
# Foram realizados testes glicêmicos em 25 pacientes após um jejum de 8 horas.
# Os resultados são apresentados na tabela abaixo. Encontrar um intervalo de confiança
# de nível de 95 %

testegli <- c(80, 118, 100, 90, 83, 117, 95, 84, 102, 80, 112, 78, 
              102, 121, 82, 77, 88, 73, 104, 88, 132, 91, 103, 140, 101)
t.test(testegli, conf = 0.95)




## Intervalo de confiança - PROPORCAO AMOSTRAL

# Exemplo 1:
# Considere testes de vazamentos identificados em dois municípios A e B. No
# município A foram examinados 500 edifícios e em 100 apresentaram falhas. No B foram
# examinados 1000 edifícios e em 300 apresentaram falhas.
# Construir um intervalo com nível confiança de 95% para o município A.

alfa = 0.05
n = 500
p = 100/500

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 5)

cat("(", p - erro, ",", p + erro, ")")


# Exemplo 2:
# Um partido deseja estimar a proporção de eleitores favoráveis a um determinado 
# candidato. Uma mostra piloto de 2.500 eleitores revelou 60% dos eleitores são 
# favoráveis a este candidato. Elaborar um intervalo de confiança de 95%.

alfa = 0.05
n = 2500
p = (0.6 * 2500) / 2500

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 2)

cat("(", p - erro, ",", p + erro, ")")




### TESTES DE HIPÓTESE...
